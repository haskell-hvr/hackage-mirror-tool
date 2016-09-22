{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple lightweight S3 API implementation
--
-- NB: This has been tested only against Dreamhost's S3 service so far
module SimpleS3
    ( S3Cfg(..)
    , ObjKey, pathToObjKey
    , ObjMetaInfo(..)

    , s3ListAllObjects
    , s3ListObjectsFolder
    , s3PutObject
    )
    where

import           Common

import qualified Blaze.ByteString.Builder as Builder
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5          as MD5
import qualified Crypto.Hash.SHA1         as SHA1
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as BC8
-- import           Data.ByteString.Lazy (toStrict,fromStrict)
import qualified Data.ByteString.Short    as BSS
import qualified Data.HashMap.Strict      as HM
import           Data.String
import           Data.Time                (UTCTime)
import           Data.Time.Clock          (getCurrentTime)
-- import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.List
import           Data.Time.Format         (defaultTimeLocale)
import qualified Data.Time.Format         as DT
import           Network.Http.Client
import qualified System.IO.Streams        as Streams
import           Text.Read                (readMaybe)
import qualified Text.XML.Light           as X

data S3Cfg = S3Cfg
    { s3cfgBaseUrl   :: !URL          -- ^ Only the host-part is used currently, as it's the preferred scheme by Dreamhost
    , s3cfgBucketId  :: !ByteString   -- ^ w/o any @/@s
    , s3cfgAccessKey :: !ByteString
    , s3cfgSecretKey :: !ByteString
    }

type ObjKey = ShortByteString

data ObjMetaInfo = OMI
    { omiKey  :: !ObjKey
    , omiMD5  :: !MD5Val
    , omiSize :: !Int
    } deriving (Eq,Ord,Show)

pathToObjKey :: Path Unrooted -> ObjKey
pathToObjKey = fromString . toUnrootedFilePath


data S3ListStream
    = S3ListFrag  [ObjMetaInfo]  {- next -}  (IO S3ListStream) {- terminate -} (IO ())
    | S3ListEx    SomeException  {- retry -} (IO S3ListStream) {- terminates by default -}
    | S3ListDone {- terminates by default -}

-- | Stream bucket listing of objects
s3ListStreamAllObjects :: S3Cfg -> IO S3ListStream
s3ListStreamAllObjects S3Cfg{..} = go Nothing Nothing
  where
    go :: Maybe ObjKey -> Maybe Connection -> IO S3ListStream
    go mmarker Nothing = do
        res <- try $ establishConnection s3cfgBaseUrl
        case res of
          Left e -> pure (S3ListEx e (go mmarker Nothing))
          Right c -> go mmarker (Just c)
    go mmarker (Just c) = do
        res <- try $ s3ListObjects1 S3Cfg{..} c "" mmarker True
        case res of
          Left e -> pure (S3ListEx e (go mmarker Nothing))
          Right (isTrunc, objs)
              | isTrunc -> do
                    let mmarker' = Just $ omiKey (last objs)
                    pure $ S3ListFrag objs
                                      (go mmarker' (Just c))
                                      (closeConnection c)
              | otherwise -> do
                    closeConnection c
                    if null objs
                        then pure S3ListDone
                        else pure $ S3ListFrag objs
                                               (pure S3ListDone)
                                               (pure ())

-- | List all objects in a bucket
--
-- May require multiple requests if result contains more than 1000 entries
s3ListAllObjects :: S3Cfg -> IO (HashMap ObjKey ObjMetaInfo)
s3ListAllObjects s3cfg = s3ListStreamAllObjects s3cfg >>= go 5 []
  where
    go :: Int -> [[ObjMetaInfo]] -> S3ListStream -> IO (HashMap ObjKey ObjMetaInfo)
    go !_        !acc S3ListDone = evaluate $ lst2hm . concat $ acc
    go maxRetries acc (S3ListEx ex retry)
      | maxRetries > 1 = retry >>= go (maxRetries-1) acc
      | otherwise      = do
            putStrLn "s3ListAllObjects needed more than 5 retries"
            throwIO ex

    go maxRetries acc (S3ListFrag objs next _) =
        go maxRetries (objs:acc) =<< next

    lst2hm = HM.fromList . map (\obj->(omiKey obj,obj))

-- TODO: currently supports only non-paginated top-level folder
s3ListObjectsFolder :: S3Cfg -> Connection -> IO (HashMap ObjKey ObjMetaInfo)
s3ListObjectsFolder s3cfg c = do
    (False, objs) <- s3ListObjects1 s3cfg c "" Nothing False
    pure $! HM.fromList . map (\obj->(omiKey obj,obj)) $ objs

s3ListObjects1 :: S3Cfg -> Connection -> ObjKey -> Maybe ObjKey -> Bool -> IO (Bool,[ObjMetaInfo])
s3ListObjects1 (s3cfg@S3Cfg {..}) c pfx marker recurse = do
    now <- formatRFC1123 <$> getCurrentTime

    let q = buildRequest1 $
              setAWSHeaders s3cfg GET ("",qry) ("","",now) []

    sendRequest c q emptyBody
    tmp <- receiveResponse c concatHandler'
    -- BC8.writeFile "debug.get.xml" tmp

    lbresult <-
        case filterContent (s3qname "ListBucketResult") $ X.parseXML tmp of
            [x] -> return x
            res -> fail $ "filterContent ListBucketResult failure: " ++ show res

    let conts_ = X.findChildren (s3qname "Contents") lbresult

    conts <- forM conts_ $ \cont ->
        case parseXML cont :: Maybe ObjMetaInfo of
            Nothing -> fail $ "parseXML ObjMetaInfo failed: " ++ X.showElement cont
            Just x -> return x

    isTrunc <- case s3xmlGetStr lbresult "IsTruncated" of
            Just "true"  -> pure True
            Just "false" -> pure False
            _ -> fail "invalid or missing IsTruncated field"

    return (isTrunc,conts)
  where
    -- we could use max-keys=, but unfortunately AWS S3 doesn't appear
    -- to support returning more than 1000 entries (which is the
    -- default anyway)
    qryparms = [ "prefix=" <> BSS.fromShort pfx | pfx /= "" ] ++
               [ "delimiter=/" | not recurse ] ++
               [ "marker=" <> BSS.fromShort x | Just x <- [marker] ]
    qry | null qryparms = mempty
        | otherwise = "?" <> BC8.intercalate "&" qryparms

s3PutObject :: S3Cfg -> Connection -> ByteString -> ObjKey -> IO ()
s3PutObject (s3cfg@S3Cfg {..}) c objdata objkey = do
    now <- formatRFC1123 <$> getCurrentTime

    let q = buildRequest1 $ do
              setAWSHeaders s3cfg PUT (objkey,mempty) (cmd5,ctype,now) [("x-amz-acl","public-read")]
              -- sadly, `setHeader "Last-Modified" ...` doesn't seem have any effect
              setContentLength (fromIntegral $ BS.length objdata)

    sendRequest c q (bsBody objdata)
    _ <- receiveResponse c concatHandler'

    return ()
  where
    objkey' = BSS.fromShort objkey
    cmd5 = B64.encode (MD5.hash objdata) -- RFC1864
    ctype = case () of
        _ | ".gz"   `BS.isSuffixOf` objkey' -> "application/x-gzip"
        _ | ".json" `BS.isSuffixOf` objkey' -> "application/json"
        _ -> "application/binary"

    bsBody :: ByteString -> Streams.OutputStream Builder.Builder -> IO ()
    bsBody bs = Streams.write (Just (Builder.fromByteString bs))

-- | Wrapper around 'genSignatureV2', sets basic AWS headers
setAWSHeaders :: S3Cfg
           -> Method -> (ObjKey,ByteString) -> (ByteString,ByteString,ByteString) -> [(ByteString,ByteString)]
           -> RequestBuilder ()
setAWSHeaders (s3cfg@S3Cfg {..}) verb (objkey,query) stdhdrs@(cmd5,ctype,date) amzhdrs = do
    http verb ("/" <> s3cfgBucketId <> "/" <> BSS.fromShort objkey <> query)
    unless (BS.null date)  $ setHeader      "Date"        date
    unless (BS.null ctype) $ setContentType               ctype
    unless (BS.null cmd5)  $ setHeader      "Content-MD5" cmd5
    forM_ amzhdrs (uncurry setHeader)
    setHeader "Authorization" ("AWS " <> s3cfgAccessKey <> ":" <> signature)
  where
    signature = genSignatureV2 s3cfg verb stdhdrs amzhdrs objkey

{- | Compute S3 v2 signature
@
Authorization = "AWS" + " " + AWSAccessKeyId + ":" + Signature;

Signature = Base64( HMAC-SHA1( YourSecretAccessKeyID, UTF-8-Encoding-Of( StringToSign ) ) );

StringToSign = HTTP-Verb + "\n" +
	Content-MD5 + "\n" +
	Content-Type + "\n" +
	Date + "\n" +
	CanonicalizedAmzHeaders +
	CanonicalizedResource;

CanonicalizedResource = [ "/" + Bucket ] +
	<HTTP-Request-URI, from the protocol name up to the query string> +
	[ subresource, if present. For example "?acl", "?location", "?logging", or "?torrent"];

CanonicalizedAmzHeaders = ...
@

-}
genSignatureV2 :: S3Cfg -> Method -> (ByteString,ByteString,ByteString) -> [(ByteString,ByteString)] -> ObjKey -> ByteString
genSignatureV2 (S3Cfg {..}) verb (cmd5,ctype,date) amzhdrs objkey = B64.encode sig
  where
    sig = SHA1.hmac s3cfgSecretKey msg
    msg = BS.intercalate "\n" $
              [ verb'
              , cmd5
              , ctype
              , date
              ] ++
              [ k <> ":" <> v | (k,v) <- sort amzhdrs ] ++
              [ "/" <> s3cfgBucketId <> "/" <> BSS.fromShort objkey ]

    verb' = case verb of
        PUT    -> "PUT"
        GET    -> "GET"
        HEAD   -> "HEAD"
        DELETE -> "DELETE"
        _ -> error "genSignatureV2: unsupported verb"


formatRFC1123 :: UTCTime -> ByteString
formatRFC1123 = BC8.pack . DT.formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

----------------------------------------------------------------------------
-- XML parsing

s3qname :: String -> X.QName
s3qname n = X.QName { X.qName = n, X.qURI = Just s3xmlns, X.qPrefix = Nothing }
  where
    s3xmlns = "http://s3.amazonaws.com/doc/2006-03-01/"

s3xmlGetStr :: X.Element -> String -> Maybe String
s3xmlGetStr el k = X.strContent <$> X.findChild (s3qname k) el

class FromXML a where
    parseXML_  :: X.Element -> Maybe a
    tagFromXML :: Proxy a -> X.QName

parseXML :: forall a . FromXML a => X.Element -> Maybe a
parseXML el = do
    guard (X.elName el == tagFromXML (Proxy :: Proxy a))
    parseXML_ el

instance FromXML ObjMetaInfo where
    tagFromXML _   = s3qname "Contents"
    parseXML_ el = do
        omiKey   <- (fromString . X.strContent) <$> X.findChild (s3qname "Key") el
        omiEtag_ <- X.strContent <$> X.findChild (s3qname "ETag") el
        omiMD5'  <- readMaybe omiEtag_
        -- sometimes the reported MD5 is computed over chunks, in
        -- which case the etag has a "-<num>" suffix. For now, we just
        -- map those to the special zero MD5 as we can't do anything
        -- sensible with it anyway (but we may want to be able to
        -- detect that the MD5 reported was not a proper MD5)
        omiMD5   <- md5unhex omiMD5' <|> Just md5zero

        omiSize_ <- X.strContent <$> X.findChild (s3qname "Size") el
        omiSize  <- readMaybe omiSize_

        pure $! (OMI {..})

filterContent :: X.QName -> [X.Content] -> [X.Element]
filterContent q = filter ((== q) . X.elName) . X.onlyElems
