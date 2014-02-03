import           Ledger

-- base
import qualified Control.Exception         as X
import           Control.Monad
import           Data.Maybe
import           System.Environment
import           System.Exit

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
        WARP.runSettings WARP.defaultSettings {
                WARP.settingsPort = port,
                WARP.settingsBeforeMainLoop = maybe (return ()) dropPrivs
                                              $ listToMaybe args}
            . waiApplicationFromSitesForHttp
            $ sites

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
sites = singleSite "dionysus"
        $ ledgerSite "102 Richmond Road Accounts"
                     "/home/dave/notes/102-richmond-road.journal"
