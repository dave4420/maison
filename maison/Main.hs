-- import maison
import           Http

-- text
import           Data.Text (Text)

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
        entityBody = entityBodyFromStrictText message
