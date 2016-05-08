{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Temporary facility needed for keeping track of MD5 hashes until
--
-- * <https://github.com/well-typed/hackage-security/pull/161>
-- * <https://github.com/haskell/hackage-server/pull/494>
--
-- is merged

module Sha256Md5Map
    ( Md5Map
    , loadMd5Map
    , sizeMd5Map
    , lookupMd5Map
    , insertMd5Map
    ) where

import           Common

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BC8
import qualified Data.HashMap.Strict     as HM

data Md5Map = Md5Map (Path Absolute) !(MVar (HM.HashMap SHA256Val MD5Val))

loadMd5Map :: FsRoot root => Path root -> IO Md5Map
loadMd5Map fp0 = do
    fp <- makeAbsolute (FsPath fp0)
    rawdat <- readStrictByteString fp

    let pairs = map (asPairs . BC8.words) $ BC8.lines rawdat
    db <- evaluate (force $ HM.fromList pairs)
    Md5Map fp <$> newMVar db
  where
    asPairs [!a,!b] = case (sha256unhex a, md5unhex b) of
        (Just a', Just b') -> (a',b')
        _                  -> error "loadMd5Map: failed to parse line"
    asPairs _ = error "loadMd5Map: failed to parse line"

sizeMd5Map :: Md5Map -> IO Int
sizeMd5Map (Md5Map _ mm) = withMVar mm (pure . HM.size)

lookupMd5Map :: SHA256Val -> Md5Map -> IO (Maybe MD5Val)
lookupMd5Map sha256key (Md5Map _ mm) = withMVar mm $ \m -> pure (HM.lookup sha256key m)

insertMd5Map :: SHA256Val -> MD5Val -> Md5Map -> IO ()
insertMd5Map sha256key md5val (Md5Map afp mm) = modifyMVar_ mm $ \m -> do
    case HM.lookup sha256key m of
        Nothing -> do
            fp <- toAbsoluteFilePath afp
            BS.appendFile fp (sha256hex sha256key <> "\t" <> md5hex md5val <> "\n")
            evaluate (HM.insert sha256key md5val m)
        Just md5val'
            | md5val' == md5val -> return m -- NOOP
            | otherwise -> fail "insertMd5Map: conflicting entry detected"
