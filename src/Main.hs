{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
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
import           Options.Applicative as OA

import           IndexClient
import           IndexShaSum              (IndexShaEntry (..))
import qualified IndexShaSum
import           SimpleS3
import           System.Exit

----------------------------------------------------------------------------


----------------------------------------------------------------------------

getSourceTarball :: URL -> ShortByteString -> IO (Either Int ByteString)
getSourceTarball hackagePackageUri pkgname = do
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
    { optDryMode       :: Bool
    , optForceSync     :: Bool
    , optHackageUrl    :: String
    , optHackagePkgUrl :: String
    , optS3BaseUrl     :: String
    , optS3BucketId    :: String
    , optMaxConns      :: Word
    }

getOpts :: IO Opts
getOpts = execParser opts
  where
    opts = info (helper <*> parseOpts)
           (fullDesc <> header "Hackage mirroring tool"
                     <> footer fmsg
           )

    parseOpts = Opts <$> switch (long "dry" <> help "operate in read-only mode")
                     <*> switch (long "force-sync" <> help "force a full sync even when meta-data appears synced")
                     <*> strOption (long "hackage-url" <> value "http://hackage.haskell.org"
                                    <> metavar "URL"
                                    <> help "URL used to retrieve Hackage meta-data" <> showDefault)
                     <*> strOption (long "hackage-pkg-url" <> value "http://hackage.haskell.org/package/"
                                    <> metavar "URL"
                                    <> help "URL used to retrieve Hackage package tarballs" <> showDefault)
                     <*> strOption (long "s3-base-url" <> value "https://objects-us-west-1.dream.io"
                                    <> metavar "URL"
                                    <> help "Base URL of S3 mirror bucket" <> showDefault)
                     <*> strOption (long "s3-bucket-id" <> value "hackage-mirror"
                                    <> help "id of S3 mirror bucket" <> showDefault)
                     <*> OA.option auto (long "max-connections" <> metavar "NUM" <> value 1
                                    <> help "max concurrent download connections" <> showDefault)


    fmsg = "Credentials are set via 'S3_ACCESS_KEY' & 'S3_SECRET_KEY' environment variables"


main :: IO ()
main = do
    Opts{..} <- getOpts

    let s3cfgBaseUrl   = fromString optS3BaseUrl
        s3cfgBucketId  = fromString optS3BucketId
    s3cfgAccessKey <- fromString <$> getEnv "S3_ACCESS_KEY"
    s3cfgSecretKey <- fromString <$> getEnv "S3_SECRET_KEY"

    try (main2 (Opts{..}) (S3Cfg{..})) >>= \case
        Left e   -> do
            logMsg CRITICAL ("exception: " ++ displayException (e::SomeException))
            exitFailure
        Right ExitSuccess -> do
            logMsg INFO "exiting"
            exitSuccess
        Right (ExitFailure rc) -> do
            logMsg ERROR ("exiting (code = " ++ show rc ++ ")")
            exitWith (ExitFailure rc)

main2 :: Opts -> S3Cfg -> IO ExitCode
main2 Opts{..} S3Cfg{..} = handle pure $ do
    when optDryMode $
        logMsg WARNING "--dry mode active"

    -- used for index-download
    let hackageUri = fromMaybe (error "hackageUri") $ parseURI optHackageUrl


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
    metafiles <- bracket (establishConnection s3cfgBaseUrl) closeConnection $ s3ListObjectsFolder (S3Cfg{..})
    metadirties <- forM jsonFiles $ \fn -> do
        objdat <- readStrictByteString (repoCacheDir </> fn)

        let obj_md5 = md5hash objdat
            obj_sz  = BS.length objdat
            objkey  = pathToObjKey fn
        let dirty = case HM.lookup objkey metafiles of
                Nothing -> True
                Just (OMI {..}) -> not (omiMD5 == obj_md5 && omiSize == obj_sz)

        pure dirty

    unless (optForceSync || or metadirties) $ do
        -- NB: 01-index.tar.gz is not compared
        logMsg INFO "meta-data files are synced and '--force-sync' was not given; nothing to do"
        exitSuccess

    ----------------------------------------------------------------------------
    -- dirty meta-files detected, do full sync

    idx <- IndexShaSum.run (IndexShaSum.IndexShaSumOptions True (repoCacheDir </> indexTarFn) Nothing)
    let idxBytes = sum [ fromIntegral sz | IndexShaEntry _ _ _ sz <- idx, sz >= 0 ] :: Word64

    logMsg INFO ("Hackage index contains " <> show (length idx) <> " src-tarball entries (" <> show idxBytes <> " bytes)")

    logMsg INFO "Listing all S3 objects (may take a while) ..."
    objmap <- s3ListAllObjects (S3Cfg{..})

    let s3cnt = length (filter isSrcTarObjKey (HM.keys objmap))
    logMsg INFO ("S3 index contains " <> show s3cnt <> " src-tarball entries (total " <> show (HM.size objmap) <> ")")

    idxQ <- newMVar idx

    -- fire up some workers...
    workers <- forM [1.. optMaxConns] $ \n -> async $ do
        bracket (establishConnection s3cfgBaseUrl) closeConnection $
            worker idxQ objmap n

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

    return ExitSuccess
  where
    isSrcTarObjKey k =
        and [ BS.isPrefixOf "package/" k'
            , BS.isSuffixOf ".tar.gz" k'
            , BS.count 0x2f k' == 1
            ]
      where
        k' = BSS.fromShort k


    s3PutObject' :: Connection -> Word -> ByteString -> ObjKey -> IO ()
    s3PutObject' conn thrId objdat objkey = do
        t0 <- getPOSIXTime
        if optDryMode
            then logMsg WARNING "no-op due to --dry"
            else s3PutObject (S3Cfg{..}) conn objdat objkey
        t1 <- getPOSIXTime
        logMsg DEBUG ("PUT completed; thr=" <> show thrId <> " dt=" ++ show (t1-t0))


    syncFile objmap objdat objkey conn = do
        let obj_md5 = md5hash objdat
            obj_sz  = BS.length objdat
        let dirty = case HM.lookup objkey objmap of
                Nothing -> True
                Just (OMI {..}) -> not (omiMD5 == obj_md5 && omiSize == obj_sz)
        if dirty
           then logMsg INFO  (show objkey ++ " needs sync")
           else logMsg DEBUG (show objkey ++ " is up-to-date")

        when dirty $ s3PutObject' conn 0 objdat objkey

        return ()


    worker idxQ objmap thrId conn = do
        tmp <- modifyMVar idxQ (pure . popQ)
        case tmp of
            Nothing -> return () -- queue empty, terminate worker
            Just (IndexShaEntry pkg s256 m5 sz) -> do
                case (HM.lookup ("package/" <> pkg) objmap) of
                    Nothing -> do -- miss
                        resp <- getSourceTarball (fromString optHackagePkgUrl) pkg
                        case resp of
                            Right pkgdat -> do
                                let s256' = sha256hash pkgdat
                                    m5'   = md5hash pkgdat

                                unless (BS.length pkgdat == sz) $
                                    fail ("size mismatch (expected: " ++ show sz ++ ")")
                                unless (s256 == s256') $
                                    fail "sha256 mismatch"
                                unless (m5 == m5') $
                                    fail "sha256 mismatch"

                                s3PutObject' conn thrId pkgdat ("package/" <> pkg)

                            Left _ -> logMsg WARNING "**skipping**" -- TODO: collect


                    Just (OMI {..}) -> do -- hit
                        if omiMD5 == m5
                            then pure () -- OK
                            else do
                              logMsg CRITICAL "MD5 corruption"
                              fail "MD5 corruption"
                        return ()
                -- loop
                worker idxQ objmap thrId conn
      where
        popQ []     = ([],Nothing)
        popQ (x:xs) = (xs, Just x)

----------------------------------------------------------------------------

