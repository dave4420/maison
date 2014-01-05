module Http where

-- base
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Monoid

-- blaze-builder
import qualified Blaze.ByteString.Builder  as Z

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC

-- containers
import           Data.Map (Map)

-- errors
import           Control.Error

-- http-types
import qualified Network.HTTP.Types        as HTTP

-- lens
import qualified Control.Lens              as L
import           Control.Lens.Operators

-- text
import           Data.Text (Text)

-- transformers
import           Control.Monad.Trans.Class

-- wai
import qualified Network.Wai               as WAI


data Authority = Authority {
        _host :: ByteString,
        _port :: Maybe (Maybe Int)}
    deriving (Eq, Ord)
$(L.makeLenses ''Authority)

parseAuthority :: ByteString -> Maybe Authority
parseAuthority bsAuth = do
        let (_host, suffix) = BC.break (':' ==) bsAuth
        guard . not . B.null $ _host
        _port <- case B.uncons suffix of
                Nothing      -> return Nothing
                Just (_, "") -> return $ Just Nothing
                Just (_, bs) -> guard (BC.all isDigit bs)
                                >> (return . Just . Just . read . BC.unpack) bs
        return Authority{..}


newtype Sites = Sites (Map Authority Site)

instance Monoid Sites where
        mempty = Sites mempty
        mappend (Sites x) (Sites y) = Sites (x <> y)

lookupSite :: Authority -> Sites -> Maybe Site
lookupSite authority (Sites sites) = sites ^. L.at authority


newtype Site = Site ([Text] -> HTTP.Query -> IO Resource)

retrieveResource :: Site -> [Text] -> HTTP.Query -> IO Resource
retrieveResource (Site site) = site


data Method = HEAD | GET

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


type Resource = Either MissingResource ExistingResource
type MissingResource = ()
type ExistingResource = ()


waiApplicationFromSites :: Sites -> WAI.Application
{- ^ Formalisation of
<http://upload.wikimedia.org/wikipedia/commons/8/8a/Http-headers-status.svg>.
-}
waiApplicationFromSites sites request = eitherT return return $ do

        authority
         <- fmap parseAuthority
            . hoistEither
            . note (oops HTTP.badRequest400)
            . WAI.requestHeaderHost
            $ request

        site
         <- hoistEither
            . note (oops HTTP.badRequest400)
            $ flip lookupSite sites =<< authority

        _method
         <- hoistEither
            . note (oops HTTP.notImplemented501)
            $ uncheckedMethod

        let handleMissingResource _ = return (oops HTTP.notFound404)

        let handleExistingResource _
                = return (oops HTTP.internalServerError500)

        either handleMissingResource handleExistingResource
            =<< lift
                ((retrieveResource site
                  <$> WAI.pathInfo
                  <*> WAI.queryString)
                 request)

    where

        uncheckedMethod = parseMethod $ WAI.requestMethod request

        oops = oops' []
        oops' headers status = WAI.responseBuilder status headers body where
                body = case uncheckedMethod of
                        Just HEAD -> mempty
                        _         -> Z.fromByteString
                                     $ HTTP.statusMessage status
