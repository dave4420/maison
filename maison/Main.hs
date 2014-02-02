import           Ledger

-- base
import           Control.Monad
import           System.Environment

-- maison
import           Http

-- hslogger
import           System.Log.Handler.Simple
import           System.Log.Logger

-- warp
import qualified Network.Wai.Handler.Warp  as WARP


main :: IO ()
main = do
        args <- getArgs
        when (not . null $ args) initLoggingForDaemon
        WARP.runSettings WARP.defaultSettings
            . waiApplicationFromSitesForHttp
            $ sites

initLoggingForDaemon :: IO ()
initLoggingForDaemon = do
        toFile <- fileHandler "/var/log/maison" INFO
        saveGlobalLogger . setHandlers [toFile] =<< getRootLogger

sites :: Sites
sites = singleSite (Authority "dionysus" 3000)
        $ ledgerSite "102 Richmond Road Accounts"
                     "/home/dave/notes/102-richmond-road.journal"
