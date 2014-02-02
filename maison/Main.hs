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
        let port' = if null args then 3000 else 80
        when (not . null $ args) initLoggingForDaemon
        WARP.runSettings WARP.defaultSettings {
                WARP.settingsPort = port'}
            . waiApplicationFromSitesForHttp
            $ sites port'

initLoggingForDaemon :: IO ()
initLoggingForDaemon = do
        toFile <- fileHandler "/var/log/maison" INFO
        saveGlobalLogger . setHandlers [toFile] =<< getRootLogger

sites :: Int -> Sites
sites port' = singleSite (Authority "dionysus" port')
              $ ledgerSite "102 Richmond Road Accounts"
                           "/home/dave/notes/102-richmond-road.journal"
