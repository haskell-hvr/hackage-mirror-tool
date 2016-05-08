{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString          as BS
-- import           Data.ByteString.Lazy (toStrict,fromStrict)
import qualified Data.ByteString.Short    as BSS
import qualified Data.HashMap.Strict      as HM
import           Data.String
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Network.Http.Client
import           System.Environment       (getEnv)
import           System.IO.Unsafe         (unsafePerformIO)
import           Options.Applicative      hiding ((<>))
import qualified Options.Applicative      as OA

import           IndexClient
import           IndexShaSum              (IndexShaEntry (..))
import qualified IndexShaSum
import           Sha256Md5Map
import           SimpleS3
import           System.Exit

----------------------------------------------------------------------------

{-# NOINLINE s3cfg #-}
s3cfg :: S3Cfg
s3cfg = S3Cfg {..}
  where
    s3cfgBaseUrl   = "https://objects-us-west-1.dream.io"
    s3cfgBucketId  = "hackage-mirror"
    s3cfgAccessKey = fromString <$> unsafePerformIO $ getEnv "S3_ACCESS_KEY"
    s3cfgSecretKey = fromString <$> unsafePerformIO $ getEnv "S3_SECRET_KEY"

-- used for index-download
hackageUri :: URI
hackageUri = fromMaybe (error "hackageUri") $ parseURI "http://hackage.haskell.org"

-- used for downloading source-tarballs
hackagePackageUri :: URL
hackagePackageUri = "http://hackage.haskell.org/package/"
-- hackagePackageUri = "http://hackage.fpcomplete.com/package/"
-- hackagePackageUri = "http://hdiff.luite.com/packages/archive/package/"

----------------------------------------------------------------------------

getSourceTarball :: ShortByteString -> IO (Either Int ByteString)
getSourceTarball pkgname = do
    -- putStrLn ("fetching " <> show pkgname <> " ...")
    t0 <- getPOSIXTime
    resp <- try $ get (hackagePackageUri <> BSS.fromShort pkgname) concatHandler'
    t1 <- getPOSIXTime
    case resp of
        Right pkgdat -> do
            logMsg DEBUG ("downloaded " <> show pkgname <> "; dt=" ++ show (t1-t0) ++ " size=" ++ show (BS.length pkgdat))
            return (Right pkgdat)

        Left e@(HttpClientError code _) -> do
            logMsg WARNING ("failed to get " <> show pkgname <> " with " <> show e)
            return (Left code)

----------------------------------------------------------------------------
-- CLI

data Opts = Opts
    { optDryMode :: Bool
    }

getOpts :: IO Opts
getOpts = OA.execParser opts
  where
    opts = info (helper <*> parseOpts)
           (fullDesc OA.<> header "Hackage mirroring tool")

    parseOpts = Opts <$> switch (long "dry")

main :: IO ()
main = do
    opts <- getOpts

    let !_ = s3cfg

    repoCacheDir <- makeAbsolute (fromFilePath "./index-cache/")

    indexChanged <- updateIndex hackageUri repoCacheDir
    case indexChanged of
        HasUpdates -> logMsg INFO "index changed"
        NoUpdates  -> logMsg INFO "no index changes"

    let indexTarFn = fragment "01-index.tar.gz"
        jsonFiles  = [ fragment "mirrors.json"
                     , fragment "root.json"
                     , fragment "snapshot.json"
                     , fragment "timestamp.json"
                     ]

    -- check meta-data files first to detect if anything needs to be done
    logMsg INFO "fetching meta-data file objects from S3..."
    metafiles <- bracket (establishConnection s3cfgBaseUrl) closeConnection $ s3ListObjectsFolder s3cfg
    metadirties <- forM jsonFiles $ \fn -> do
        objdat <- readStrictByteString (repoCacheDir </> fn)

        let obj_md5 = md5hash objdat
            obj_sz  = BS.length objdat
            objkey  = pathToObjKey fn
        let dirty = case HM.lookup objkey metafiles of
                Nothing -> True
                Just (OMI {..}) -> not (omiMD5 == obj_md5 && omiSize == obj_sz)

        pure dirty

    unless (or metadirties) $ do
        -- NB: 01-index.tar.gz is not compared
        logMsg INFO "meta-data files are synced and '--force-sync' was not given; nothing to do"
        exitSuccess

    ----------------------------------------------------------------------------
    -- dirty meta-files detected, do full sync

    md5map   <- loadMd5Map (Path "sha256_md5.dat" :: Path Relative)
    md5mapsz <- sizeMd5Map md5map
    logMsg INFO ("SHA256->MD5 map loaded with " <> show md5mapsz <> " entries")

    idx <- IndexShaSum.run (IndexShaSum.IndexShaSumOptions True (repoCacheDir </> indexTarFn) Nothing)

    logMsg INFO ("Hackage index contains " <> show (length idx) <> " src-tarball entries")

    logMsg INFO "Listing all S3 objects (may take a while) ..."
    objmap <- bracket (establishConnection s3cfgBaseUrl) closeConnection $ s3ListAllObjects s3cfg

    let s3cnt = length (filter (BS.isPrefixOf "package/" . BSS.fromShort) (HM.keys objmap))
    logMsg INFO ("S3 index contains " <> show s3cnt <> " src-tarball entries (total " <> show (HM.size objmap) <> ")")

    idxQ <- newMVar idx

    -- fire up some workers...
    workers <- forM [1..maxWorkers :: Int] $ \n -> async $ do
        bracket (establishConnection s3cfgBaseUrl) closeConnection $
            worker idxQ objmap md5map n

    forM_ workers link

    forM_ workers wait
    logMsg INFO "workers finished..."

    -- update meta-data last...
    bracket (establishConnection s3cfgBaseUrl) closeConnection $ \c -> do
        -- this one can take ~1 minute; so we need to update this first
        do tmp <- readStrictByteString (repoCacheDir </> indexTarFn)
           syncFile objmap tmp (pathToObjKey indexTarFn) c

        forM_ jsonFiles $ \fn -> do
            tmp <- readStrictByteString (repoCacheDir </> fn)
            syncFile objmap tmp (pathToObjKey fn) c

    logMsg INFO "sync job completed"

    return ()
  where
    S3Cfg {..} = s3cfg
    maxWorkers = 4

    syncFile objmap objdat objkey c = do
        let obj_md5 = md5hash objdat
            obj_sz  = BS.length objdat
        let dirty = case HM.lookup objkey objmap of
                Nothing -> True
                Just (OMI {..}) -> not (omiMD5 == obj_md5 && omiSize == obj_sz)
        if dirty
           then logMsg INFO  (show objkey ++ " needs sync")
           else logMsg DEBUG (show objkey ++ " is up-to-date")

        when dirty $ do
            t0 <- getPOSIXTime
            s3PutObject s3cfg c objdat objkey
            t1 <- getPOSIXTime
            logMsg DEBUG ("PUT completed; dt=" ++ show (t1-t0))

        return ()

    worker idxQ objmap md5map thrId c = do
        tmp <- modifyMVar idxQ (pure . popQ)
        case tmp of
            Nothing -> return () -- queue empty, terminate worker
            Just (IndexShaEntry pkg s256 sz) -> do
                case (HM.lookup ("package/" <> pkg) objmap) of
                    Nothing -> do -- miss
                        resp <- getSourceTarball pkg
                        case resp of
                            Right pkgdat -> do
                                let s256' = sha256hash pkgdat
                                    md5'  = md5hash pkgdat

                                unless (BS.length pkgdat == sz) $
                                    fail ("size mismatch (expected: " ++ show sz ++ ")")
                                unless (s256 == s256') $
                                    fail "sha256 mismatch"

                                insertMd5Map s256' md5' md5map

                                t0 <- getPOSIXTime
                                s3PutObject s3cfg c pkgdat ("package/" <> pkg)
                                t1 <- getPOSIXTime
                                logMsg DEBUG ("PUT completed; thr=" <> show thrId <> " dt=" ++ show (t1-t0))

                            Left _ -> logMsg WARNING "**skipping**" -- TODO: collect


                    Just (OMI {..}) -> do -- hit
                        mmd5 <- lookupMd5Map s256 md5map
                        case mmd5 of
                            Nothing                   ->
                                logMsg ERROR (show omiKey ++ "MD5 mapping missing")
                            Just md5 | omiMD5 == md5  -> pure ()
                                     | otherwise      -> do
                                           logMsg CRITICAL "MD5 corruption"
                                           fail "MD5 corruption"
                        return ()
                -- loop
                worker idxQ objmap md5map thrId c
      where
        popQ []     = ([],Nothing)
        popQ (x:xs) = (xs, Just x)

----------------------------------------------------------------------------

