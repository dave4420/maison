import           FileTree
import           PageSite

-- base
import qualified Control.Exception         as X
import           Control.Monad
import qualified Data.Foldable             as F
import           Data.Maybe
import           Data.Monoid
import           System.Environment
import           System.Exit

-- lens
import           Control.Lens.Operators

-- maison
import           Http

-- hslogger
import           System.Log.Handler.Simple
import           System.Log.Logger

-- unix
import           System.Posix.User

-- warp
import qualified Network.Wai.Handler.Warp  as WARP


main :: IO ()
main = do
        args <- getArgs
        let port = if null args then 3000 else 80
        when (not . null $ args) initLoggingForDaemon
        WARP.runSettings (WARP.setPort port
                          . WARP.setBeforeMainLoop (maybe (return ()) dropPrivs
                                                    $ listToMaybe args)
                          $ WARP.defaultSettings)
            . waiApplication Http
            $ (settingsSites .~ sites)
              . (settingsOnException
                 .~ (\e -> lift $ warningM "" ("Exception: " ++ show e)))

initLoggingForDaemon :: IO ()
initLoggingForDaemon = do
        toFile <- fileHandler "/var/log/maison" INFO
        saveGlobalLogger . setHandlers [toFile] =<< getRootLogger

dropPrivs :: String -> IO ()
dropPrivs nUser = do
        user <- getUserEntryForName nUser
        setGroupID $ userGroupID user
        setUserID $ userID user
        noticeM "" $ "Running as " ++ nUser
      `X.catch` onError
    where
        onError :: X.IOException -> IO ()
        onError e = do
                criticalM "" $ "Terminating because can't drop privs: "
                               ++ show e
                exitFailure


sites :: Sites
sites = F.foldMap (`singleSite` (multiUserFileTreeSite "Dionysus"
                                                       ["dave", "faith"]
                                 <> underPath
                                    ("pages" :| [])
                                    (pageSite "/home/dave/tmp/pages")))
                  ["dionysus", "dionysus.lan", "192.168.1.104"]
