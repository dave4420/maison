-- import maison
import           Http

-- blaze-builder
import qualified Blaze.ByteString.Builder  as Z

-- text
import           Data.Text (Text)
import qualified Data.Text.Encoding        as T

-- warp
import qualified Network.Wai.Handler.Warp  as WARP


main :: IO ()
main = WARP.runSettings WARP.defaultSettings
       . waiApplicationFromSites
       $ sites

sites :: Sites
sites = singleSite (Authority "dionysus" . Just . Just $ 3000) site

site :: Site
site = Site $ \path _query -> return $ case path of
        [] -> Right . yield $ "Oh no!"
        [""] -> Right . yield $ "Fantastic!"
        _ -> Left defaultMissingResource

yield :: Text -> ExistingResource
yield message = (defaultExistingResource :: ExistingResource)
                {existingGet = Just get}
    where
        get = return ([], Entity{..})
        entityType = "text/plain; charset=utf-8"
        entityBody = EntityBodyFromBuilder . Z.fromByteString . T.encodeUtf8
                     $ message
