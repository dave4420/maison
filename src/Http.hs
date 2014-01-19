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
import qualified Data.Map                  as M

-- errors
import           Control.Error

-- http-types
import qualified Network.HTTP.Types        as HTTP

-- lens
import qualified Control.Lens              as L
import           Control.Lens.Operators

-- text
import           Data.Text (Text)
import qualified Data.Text.Encoding        as T

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


newtype Sites' m = Sites (Map Authority (Site' m))
type Sites = Sites' IO

instance Monoid (Sites' m) where
        mempty = Sites mempty
        mappend (Sites x) (Sites y) = Sites (x <> y)

lookupSite :: Authority -> (Sites' m) -> Maybe (Site' m)
lookupSite authority (Sites sites) = sites ^. L.at authority

singleSite :: Monad m => Authority -> Site' m -> Sites' m
singleSite authority site = Sites $ M.singleton authority site


newtype Site' m = Site ([Text] -> HTTP.Query -> m (Resource' m))
type Site = Site' IO

retrieveResource :: Monad m =>
                    Site' m -> [Text] -> HTTP.Query -> m (Resource' m)
retrieveResource (Site site) = site


data Method = HEAD | GET
    deriving Eq

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


type Resource = Resource' IO
type MissingResource = MissingResource' IO
type ExistingResource = ExistingResource' IO
type Resource' m = Either (MissingResource' m) (ExistingResource' m)

data MissingResource' m = MissingResource (m ())

data ExistingResource' m = ExistingResource {
        existingGet :: Maybe (m ([ExtraHeader], Entity))}

defaultMissingResource :: Monad m => MissingResource' m
defaultMissingResource = MissingResource (return ())

defaultExistingResource :: Monad m => ExistingResource' m
defaultExistingResource = ExistingResource Nothing


data ExtraHeader = ExtraHeader HTTP.Header

unExtraHeader :: ExtraHeader -> HTTP.ResponseHeaders
unExtraHeader = \case
        ExtraHeader header -> [header]

data Entity = Entity {
        entityType :: ByteString,
        entityBody :: EntityBody}

data EntityBody
        = EntityBodyFromFile FilePath (Maybe WAI.FilePart)
        | EntityBodyFromBuilder Z.Builder

entityBodyFromStrictText :: Text -> EntityBody
entityBodyFromStrictText = entityBodyFromStrictByteString . T.encodeUtf8

entityBodyFromStrictByteString :: ByteString -> EntityBody
entityBodyFromStrictByteString = EntityBodyFromBuilder . Z.fromByteString


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


httpMain :: Monad m => Sites' m -> HttpT m WAI.Response
httpMain sites = do

        authority
         <- maybe (oops HTTP.badRequest400) (return . parseAuthority)
            =<< asksRequest WAI.requestHeaderHost

        site
         <- maybe (oops HTTP.badRequest400) return
            $ flip lookupSite sites =<< authority

        method
         <- maybe (oops HTTP.notImplemented501) return
            =<< asksRequest (parseMethod . WAI.requestMethod)

        (either <$> handleMissingResource <*> handleExistingResource) method
            =<< lift
            =<< asksRequest (retrieveResource site
                             <$> WAI.pathInfo
                             <*> WAI.queryString)


handleMissingResource
        :: Monad m => Method -> MissingResource' m -> HttpT m WAI.Response
handleMissingResource _method _resource = oops HTTP.notFound404


handleExistingResource
        :: Monad m => Method -> ExistingResource' m -> HttpT m WAI.Response
handleExistingResource method resource = do
        --TODO: caching preconditions
        (extraHeaders, entity)
         <- maybe (oops HTTP.methodNotAllowed405) lift $ case method of
                GET -> existingGet resource
                       --TODO: range requests
                HEAD -> existingGet resource
                        --TODO: early return for HEAD
        --TODO: content negotiation
        return $ waiResponse (method == HEAD) HTTP.ok200 extraHeaders entity


waiResponse :: Bool -> HTTP.Status -> [ExtraHeader] -> Entity -> WAI.Response
waiResponse omitBody status extraHeaders Entity{..}
        | omitBody  = WAI.responseBuilder status headers mempty
        | otherwise = case entityBody of
                EntityBodyFromFile nf part
                 -> WAI.responseFile status headers nf part
                EntityBodyFromBuilder builder
                 -> WAI.responseBuilder status headers builder
    where
        headers = concatMap unExtraHeader extraHeaders
                  ++ [(HTTP.hContentType, entityType)]
