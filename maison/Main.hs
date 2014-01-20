import           Ledger

-- maison
import           Http

-- warp
import qualified Network.Wai.Handler.Warp  as WARP


main :: IO ()
main = WARP.runSettings WARP.defaultSettings
       . waiApplicationFromSites
       $ sites

sites :: Sites
sites = singleSite (Authority "dionysus" . Just . Just $ 3000)
        $ ledgerSite "/home/dave/notes/102-richmond-road.journal"
