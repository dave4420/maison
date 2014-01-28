import           Ledger

-- maison
import           Http

-- warp
import qualified Network.Wai.Handler.Warp  as WARP


main :: IO ()
main = WARP.runSettings WARP.defaultSettings
       . waiApplicationFromSitesForHttp
       $ sites

sites :: Sites
sites = singleSite (Authority "dionysus" 3000)
        $ ledgerSite "102 Richmond Road Accounts"
                     "/home/dave/notes/102-richmond-road.journal"
