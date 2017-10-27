-- |
-- Copyright : Herbert Valerio Riedel
-- License   : GPLv3
--
module IndexClient
    ( updateIndex
    , HasUpdates(..)
      -- convenience
    , parseURI
    , URI
    ) where

import           Hackage.Security.Client
import qualified Hackage.Security.Client.Repository.Cache        as Cache
import qualified Hackage.Security.Client.Repository.HttpLib.HTTP as HttpLib.HTTP
import qualified Hackage.Security.Client.Repository.Remote       as Remote
import           Hackage.Security.Util.Path
import           Hackage.Security.Util.Pretty

import           Control.Monad
import           Data.Time                                       (getCurrentTime)
import           Network.URI                                     (URI, parseURI)

import           Common

updateIndex :: FsRoot root => URI -> Path root -> IO HasUpdates
updateIndex hackageUri cacheDir = do
    now <- getCurrentTime

    globalCache <- makeAbsolute (FsPath cacheDir)

    let cache = Cache.Cache { Cache.cacheRoot   = globalCache
                            , Cache.cacheLayout = layout
                            }

    HttpLib.HTTP.withClient $ \browser hc -> do
        HttpLib.HTTP.setOutHandler browser (\_msg -> pure ())
        Remote.withRepository hc
                              [hackageUri]
                              repoOpts
                              cache
                              hackageRepoLayout
                              hackageIndexLayout
                              logMsg' $ \rep -> uncheckClientErrors $ do
            needBoot <- requiresBootstrap rep
            when needBoot $ do
                bootstrap rep initialRootKeys (KeyThreshold 3)

            checkForUpdates rep (Just now)
  where
    -- hardcoded keys for now
    initialRootKeys =
        [ KeyId "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
        , KeyId "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
        , KeyId "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3"
        , KeyId "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
        , KeyId "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
        ]

    layout :: CacheLayout
    layout = CacheLayout
        { cacheLayoutRoot       = rootPath $ fragment "root.json"
        , cacheLayoutTimestamp  = rootPath $ fragment "timestamp.json"
        , cacheLayoutSnapshot   = rootPath $ fragment "snapshot.json"
        , cacheLayoutMirrors    = rootPath $ fragment "mirrors.json"
        , cacheLayoutIndexTar   = rootPath $ fragment "01-index.tar"
        , cacheLayoutIndexIdx   = rootPath $ fragment "01-index.tar.idx"
        , cacheLayoutIndexTarGz = rootPath $ fragment "01-index.tar.gz"
        }

    repoOpts :: Remote.RepoOpts
    repoOpts = Remote.defaultRepoOpts

    logMsg' :: LogMessage -> IO ()
    logMsg' msg = logMsg sev $ pretty msg
      where
        sev = case msg of
            LogCannotUpdate      {} -> WARNING
            LogDownloading       {} -> INFO
            LogMirrorFailed      {} -> WARNING
            LogRootUpdated       {} -> INFO
            LogSelectedMirror    {} -> INFO
            LogUpdating          {} -> INFO
            LogVerificationError {} -> ERROR
