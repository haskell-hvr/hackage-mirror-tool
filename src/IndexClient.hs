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
    -- FIXME
    initialRootKeys =
        [ KeyId "89e692e45b53b575f79a02f82fe47359b0b577dec23b45d846d6e734ffcc887a"
        , KeyId "dc4b6619e8ea2a0b72cad89a3803382f6acc8beda758be51660b5ce7c15e535b"
        , KeyId "1035a452fd3ada87956f9e77595cfd5e41446781d7ba9ff9e58b94488ac0bad7"
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
