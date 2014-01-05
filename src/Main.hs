import           Http

-- base
import           Data.Monoid

-- warp
import qualified Network.Wai.Handler.Warp as WARP


main :: IO ()
main = WARP.runSettings WARP.defaultSettings
       . waiApplicationFromSites
       $ mempty

