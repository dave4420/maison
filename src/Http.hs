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
import           Control.Monad.Trans.Reader

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


newtype Site' m = Site ([Text] -> HTTP.Query -> m Resource)
type Site = Site' IO

retrieveResource :: Monad m => Site' m -> [Text] -> HTTP.Query -> m Resource
retrieveResource (Site site) = site


data Method = HEAD | GET

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


type Resource = Either MissingResource ExistingResource
type MissingResource = ()
type ExistingResource = ()


newtype HttpT m a = HttpT (EitherT UglyStatus (ReaderT WAI.Request m) a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans HttpT where
        lift = HttpT . lift . lift

data UglyStatus = UglyStatus HTTP.ResponseHeaders HTTP.Status

runHttpT :: Monad m =>
            (UglyStatus -> ReaderT WAI.Request m a) ->
            HttpT m a ->
            WAI.Request ->
            m a
runHttpT ugly (HttpT act) = runReaderT (eitherT ugly return act)

asksRequest :: Monad m => (WAI.Request -> a) -> HttpT m a
asksRequest f = HttpT . lift $ asks f

oops :: Monad m => HTTP.Status -> HttpT m a
oops = oops' []

oops' :: Monad m => HTTP.ResponseHeaders -> HTTP.Status -> HttpT m a
oops' headers status = HttpT . left $ UglyStatus headers status

defaultUgly :: Monad m => UglyStatus -> ReaderT WAI.Request m WAI.Response
defaultUgly (UglyStatus headers status) = do
        method <- asks WAI.requestMethod
        return . WAI.responseBuilder status headers $ case method of
                "HEAD" -> mempty
                _      -> Z.fromByteString $ HTTP.statusMessage status


waiApplicationFromSites :: Sites -> WAI.Application
{- ^ Formalisation of
<http://upload.wikimedia.org/wikipedia/commons/8/8a/Http-headers-status.svg>.
-}
waiApplicationFromSites sites = runHttpT defaultUgly (httpMain sites)


httpMain :: Sites -> HttpT IO WAI.Response
httpMain sites = do

        authority
         <- maybe (oops HTTP.badRequest400) (return . parseAuthority)
            =<< asksRequest WAI.requestHeaderHost

        site
         <- maybe (oops HTTP.badRequest400) return
            $ flip lookupSite sites =<< authority

        _method
         <- maybe (oops HTTP.notImplemented501) return
            =<< asksRequest (parseMethod . WAI.requestMethod)

        let handleMissingResource _ = oops HTTP.notFound404

        let handleExistingResource _resource = oops HTTP.internalServerError500

        either handleMissingResource handleExistingResource
            =<< lift
            =<< asksRequest (retrieveResource site
                             <$> WAI.pathInfo
                             <*> WAI.queryString)
