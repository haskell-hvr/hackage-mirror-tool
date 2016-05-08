{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      :  IndexShaSum
-- Copyright   :  Herbert Valerio Riedel
-- License     :  GPL-3
--
module IndexShaSum (run, unFlat, IndexShaEntry(..), IndexShaSumOptions(..)) where

import           Common

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.DeepSeq
import           Control.Monad
import qualified Data.Aeson             as J
import qualified Data.Aeson.Types       as J
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Short  as BSS
import qualified Data.HashMap.Strict    as HM
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.String
import           Data.Text.Encoding     as T
import           System.FilePath        as FP

data IndexShaSumOptions = IndexShaSumOptions
    { optFlatStyle   :: Bool
    , optISSIndexTar :: Path Absolute
    , optBaseDir     :: Maybe FilePath
    } deriving Show

data IndexShaEntry = IndexShaEntry !SrcTarName !SHA256Val !Int

instance NFData IndexShaEntry where rnf !_ = ()

type SrcTarName   = BSS.ShortByteString -- with .tar.gz suffix
-- type SrcTarSha256 = BSS.ShortByteString -- base16

run :: IndexShaSumOptions -> IO [IndexShaEntry]
run (IndexShaSumOptions {..}) = do
    idx <- readTarEntries optISSIndexTar
    pure (map fixupIdx $ collect idx)
  where
    collect :: [Tar.Entry] -> [IndexShaEntry]
    collect = go mempty mempty

    go :: Set SrcTarName -> Set SrcTarName -> [Tar.Entry] -> [IndexShaEntry]
    go !seen1 !seen2 []
      | missingCabs <- Set.difference seen1 seen2
      , not (Set.null missingCabs) = error "missing .cabal file(s)"
      | otherwise -- append files with missing checksum
      = [ IndexShaEntry missingSum sha256zero (-1) | missingSum <- Set.toList (Set.difference seen2 seen1) ]
    go !seen1 !seen2 (e:es)
      | takeExtension fn == ".cabal"
      , [pn,pv,_cn] <- splitDirectories fn
      = let fn' = fromString (pn ++ "-" ++ pv ++ ".tar.gz")
        in go seen1 (Set.insert fn' seen2) es
      | FP.takeFileName fn == "package.json"
      , Tar.NormalFile bs _sz <- Tar.entryContent e
      = let ent@(IndexShaEntry fn' _ _) = fromMaybe undefined (decodePkgJsonFile bs)
        in if Set.member fn' seen1
           then go seen1 seen2 es
           else (ent : go (Set.insert fn' seen1) seen2 es)
      | otherwise = go seen1 seen2 es
      where
        fn = Tar.entryPath e

-- | Convert to non-flat layout (i.e. @<name>/<ver>/<name>-<ver>.tar.gz@)
unFlat :: SrcTarName -> SrcTarName
unFlat fn0 = BSS.toShort $ mconcat [pn <> "-" <> pv <> "/" <> fn0']
  where
    fn0' = BSS.fromShort fn0

    Just base = stripSuffixBS ".tar.gz" fn0'

    (pn_, pv) = BS.spanEnd (\c -> (c >= 0x30 && c <= 0x3a) || c == 0x2e) base
    Just (pn, 0x2d) = BS.unsnoc pn_

-- | Read tarball lazily (and possibly decompress)
readTarEntries :: FsRoot root => Path root -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case snd $ Common.splitExtension idxtar of
            ".gz"  -> Tar.read . GZip.decompress <$> readLazyByteString idxtar
            ".tar" -> Tar.read                   <$> readLazyByteString idxtar
            ext    -> error ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

-- | Decode and extract source-tarball filename and sha256 checksum from TUF @package.json@
decodePkgJsonFile :: BSL.ByteString -> Maybe IndexShaEntry
decodePkgJsonFile bs = do
    metainfo <- J.decode' bs
    [(fn,s256,sz)] <- packagejson2sha metainfo

    s256' <- maybe (fail "bad SHA256 hash") pure $ sha256unhex s256

    return $! IndexShaEntry (BSS.toShort $ normaliseFn fn) s256' sz
  where
    normaliseFn fn = fromMaybe fn $ stripPrefixBS "<repo>/package/" fn

    packagejson2sha :: J.Value -> Maybe [(BS.ByteString, BS.ByteString, Int)]
    packagejson2sha = J.parseMaybe go1
      where
        go1 = J.withObject "PackageJson" $ \o -> do
            signed   <- o      J..: "signed"
            targets  <- signed J..: "targets"
            J.withObject "PackageJson.signed.targets" go2 targets

        go2 m = forM (HM.toList m) $ \(k,v) -> do
            J.withObject ".targets{}" (go3 k) v

        go3 k o = do
            hashes <- o J..: "hashes"
            sh256 <- hashes J..: "sha256"
            sz <- o J..: "length"
            return (T.encodeUtf8 k, T.encodeUtf8 sh256, sz)

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS pfx b
  | BS.isPrefixOf pfx b = Just $ BS.drop (BS.length pfx) b
  | otherwise           = Nothing


stripSuffixBS :: ByteString -> ByteString -> Maybe ByteString
stripSuffixBS sfx b
  | BS.isSuffixOf sfx b = Just $ BS.take (BS.length b - BS.length sfx) b
  | otherwise           = Nothing



-- | Inject missing SHA256 sums
--
-- Workaround needed until <https://github.com/haskell/hackage-server/issues/488>
-- is fixed.
fixupIdx :: IndexShaEntry -> IndexShaEntry
fixupIdx ent@(IndexShaEntry k cksum sz)
  | sz < 0 || cksum == sha256zero
    = case [ ent' | ent'@(IndexShaEntry k' _ _) <- IndexShaSum.brokenEntries, k' == k ] of
        [x] -> x
        _   -> error ("fixupIdx " ++ show k)
  | otherwise = ent

-- | See 'fixupIdx'
brokenEntries :: [IndexShaEntry]
brokenEntries = force [ IndexShaEntry k h s | (k,h,s) <- tab ]
  where
    tab = [ ("amazonka-codedeploy-1.4.0.tar.gz"   , "54dcede69badb68d6bd1b0d44ae39a511840305dca9efe7c60cef08a101810e7" ,   51822)
          , ("amazonka-config-1.3.5.tar.gz"       , "43e9d8103d40b13b77ba7d07c6bcbf6ab7e1419ae38aacebb3816caf039c49f1" ,   45702)
          , ("amazonka-importexport-1.3.7.tar.gz" , "ec40b77cc0f365d01c6614cf0daa1dbbdbcf97cd4cb1ac1b258cd8382ff3ae6d" ,   17961)
          , ("amazonka-importexport-1.4.0.tar.gz" , "8f7151dc995efd7e4fd431e334747aa32162cdbea3b2801a4546a8835e0b5890" ,   17916)
          , ("amazonka-s3-1.3.6.tar.gz"           , "4867f20e331f1c5197b212d1ba6051887631419bc92cbc74dd26f0eed1987087" ,   87830)
          , ("amazonka-sdb-1.4.0.tar.gz"          , "aebe7ba2ba8492bace5d04971a4164735a26c8f3b99520d516a93d2c4f9f199b" ,   21622)
          , ("eventloop-0.5.0.0.tar.gz"           , "8771bed9a4246ea1c55bf301fdb81adb2f08906152a0bdbc9edf95bb8d72531b" ,   35030)
          , ("gi-gdkpixbuf-0.2.31.12.tar.gz"      , "612709d8ac6d86b60cbdca6e038f38ef74d8fb55679ed5a70eb43781d47a30d9" ,   31646)
          , ("gi-gio-0.2.46.13.tar.gz"            , "7a44b89ec398d272f601a4526cd208373f6f8b0435429f0f30f17e6bb8d1ee27" ,  446351)
          , ("gi-girepository-0.1.44.12.tar.gz"   , "c7f53dee512511df7a4c8f00d2e5ae37ae52f49859efd83afef77e9c3f71fd80" ,   28655)
          , ("glue-ekg-0.4.4.tar.gz"              , "5aa00ec498baa8f5a960c535f7e0b9fa9b572b0f1f139c0dbe9ace9e900ccee8" ,    3740)
          , ("gnuidn-0.2.2.tar.gz"                , "61346b1f764cead5633bdc83f7dc9836b1627f484984094cdffa95dfd365b96f" ,   17534)
          , ("gogol-autoscaler-0.0.1.tar.gz"      , "0337f8d46c6b52cce6752c0f5c0b58b39af43398ea2e76379ea4001355f08acc" ,   19038)
          , ("hops-0.4.0.tar.gz"                  , "7683f48ef77af14236a5a4f65f5292504cd1db333f76df91aab84a18925287d6" ,   34738)
          , ("hprotoc-2.1.11.tar.gz"              , "87962aee9b1a5e5d7d069d98c5b279e2cf61e0832033ff03aff91de8c7e1463e" ,  134796)
          , ("hzaif-0.1.0.0.tar.gz"               , "1d600bf4940412059c0d3916573fd2042c3695d627993fb1277b38e241fa9a55" ,    3094)
          , ("repa-io-3.4.0.2.tar.gz"             , "bfda3a8b5dc92783e6099e361a0e76b0994b26fc7a8073a22696cb3fe6cb8147" ,    5942)
          ]
