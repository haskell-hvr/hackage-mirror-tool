{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common
    ( module Common
    , module Data.Maybe
    , module Hackage.Security.Util.Path
    , module Data.Semigroup
    , ByteString
    , ShortByteString
    , Text
    , HM.HashMap
    , Proxy(..)
    , NFData
    ) where

import           Control.DeepSeq
import           Control.Exception
import qualified Crypto.Hash.MD5            as MD5
import qualified Crypto.Hash.SHA256         as SHA256
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as B16
import           Data.Semigroup
-- import           Data.ByteString.Lazy (toStrict,fromStrict)
import           Data.ByteString.Short      (ShortByteString, fromShort,
                                             toShort)
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Text                  (Text)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale)
import qualified Data.Time.Format           as DT
import           Hackage.Security.Util.Path

newtype SHA256Val = SHA256Val ShortByteString
                  deriving (Eq,Ord,Hashable,NFData)

instance IsString SHA256Val where
    fromString = fromMaybe (error "invalid SHA256Val string-literal") . sha256unhex . fromString

newtype MD5Val    = MD5Val    ShortByteString
                  deriving (Eq,Ord,Hashable,NFData)

instance Show MD5Val where
    show = show . md5hex

sha256hash :: ByteString -> SHA256Val
sha256hash = SHA256Val . toShort . SHA256.hash

sha256hex :: SHA256Val -> ByteString
sha256hex (SHA256Val x) = B16.encode (fromShort x)

sha256unhex :: ByteString -> Maybe SHA256Val
sha256unhex x = case B16.decode x of
    (d, rest) | BS.null rest, BS.length d == 32
                -> Just (SHA256Val (toShort d))
    _           -> Nothing

-- Special reserved 'SHA256Val'
sha256zero :: SHA256Val
sha256zero = SHA256Val $ toShort $ BS.replicate 32 0

md5hash :: ByteString -> MD5Val
md5hash = MD5Val . toShort . MD5.hash

md5hex :: MD5Val -> ByteString
md5hex (MD5Val x) = B16.encode (fromShort x)

md5unhex :: ByteString -> Maybe MD5Val
md5unhex x = case B16.decode x of
    (d, rest) | BS.null rest, BS.length d == 16
                -> Just (MD5Val (toShort d))
    _           -> Nothing

strictPair :: a -> b -> (a,b)
strictPair !a !b = (a,b)

-- strictTriple :: a -> b -> c -> (a,b,c)
-- strictTriple !a !b !c = (a,b,c)


data LogPrio
    = DEBUG
    | INFO
    | NOTICE
    | WARNING
    | ERROR
    | CRITICAL
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

logMsg :: LogPrio -> String -> IO ()
logMsg prio msg = do
    evaluate (rnf msg)
    now <- getCurrentTime
    let ts = DT.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" now
    putStrLn (take 23 ts ++ "Z | *" ++ show prio ++ "* " ++ msg)
  where

