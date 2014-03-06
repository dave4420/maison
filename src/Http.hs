module Http (
        Authority (Authority), authorityHost, authorityPort,
        Sites, Sites',
        singleSite, singleSitePort,
        underSite, underSitePort,
        Site, Site'(..),
        Method(..),
        Resource, Resource',
        ExistingResource, ExistingResource'(), existingResource,
        existingGet,
        MissingResource, MissingResource'(), missingResource,
        missingBecause,
        MissingBecause'(..),
        Transience(..),
        NotFound(..),
        ExtraHeader(..),
        Entity(),
        entityFromStrictText, entityFromStrictByteString, entityFromHtml,
        Settings(), settingsSites, settingsOnException,
        waiApplicationFromSitesForHttp, waiApplicationFromSitesForHttps,
        NonEmpty(..),
        Query,
        Path,
        Uri(..),
        RelUri(..),
        Protocol(..),
)
where

import           DefMap (DefMap)
import qualified DefMap                        as DM

-- base
import           Control.Applicative
import qualified Control.Exception             as X
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Monoid

-- blaze-builder
import qualified Blaze.ByteString.Builder      as Z

-- blaze-html
import qualified Text.Blaze.Html               as ZH
import qualified Text.Blaze.Html.Renderer.Utf8 as ZH

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

-- containers
import           Data.Map (Map)
import qualified Data.Map                      as M

-- errors
import           Control.Error

-- http-types
import           Network.HTTP.Types (Query)
import qualified Network.HTTP.Types            as HTTP

-- lens
import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- semigroups
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty            as SG

-- text
import           Data.Text (Text)
import qualified Data.Text.Encoding            as T

-- transformers
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

-- wai
import qualified Network.Wai                   as WAI


data Authority = Authority {
        _authorityHost :: ByteString,
        _authorityPort :: Int}
    deriving (Eq, Ord)
$(L.makeLenses ''Authority)

parseAuthority :: Int -> ByteString -> Maybe Authority
parseAuthority defaultPort bsAuth = do
        let (_authorityHost, suffix) = BC.break (':' ==) bsAuth
        guard . not . B.null $ _authorityHost
        _authorityPort <- case B.uncons suffix of
                Nothing      -> return defaultPort
                Just (_, "") -> return defaultPort
                Just (_, bs) -> guard (BC.all isDigit bs)
                                >> (return . read . BC.unpack) bs
        return Authority{..}

bsFromAuthority :: Protocol -> Authority -> ByteString
bsFromAuthority protocol authority
        = authority ^. authorityHost
          <> (if protocolPort protocol == authority ^. authorityPort
                 then ""
                 else BC.pack $ ':' : show (authority ^. authorityPort))


data Uri = Uri Protocol Authority Path Query

data RelUri
        = AbsUri Uri
        | RelProtocolUri Authority Path Query
        | RelHostUri Path Query
        | RelPathUri Int Path Query
        | SameUri

type Path = NonEmpty Text

data Protocol = Http | Https

(.+.) :: Uri -> RelUri -> Uri
_  .+. AbsUri uri = uri
Uri protocol _ _ _ .+. RelProtocolUri host path query
        = Uri protocol host path query
Uri protocol host _ _ .+. RelHostUri path query = Uri protocol host path query
Uri protocol host path _ .+. RelPathUri up path' query
        = Uri protocol host (SG.fromList newPath) query
    where
        newPath = reverse (drop up . SG.tail . SG.reverse $ path)
                  ++ SG.toList path'
uri .+. SameUri = uri

protocolPort :: Protocol -> Int
protocolPort Http = 80
protocolPort Https = 443

bsFromUri :: Uri -> ByteString
bsFromUri (Uri protocol authority path query) = mconcat [
        case protocol of { Http -> "http://" ; Https -> "https://" },
        bsFromAuthority protocol authority,
        Z.toByteString $ HTTP.encodePath (SG.toList path) query]


newtype Sites' m = Sites (Map ByteString (DefMap Int (Site' m), -- here
                                          DefMap Int (Site' m), -- def for below
                                          Sites' m))            -- below
type Sites = Sites' IO

instance Monoid (Sites' m) where
        mempty = Sites mempty
        mappend (Sites x) (Sites y) = Sites (M.unionWith (<>) x y)

lookupSite :: Authority -> (Sites' m) -> Maybe (Site' m)
lookupSite authority
        = view (L.view (DM.defAt $ authority ^. authorityPort))
               (SG.reverse . atomiseHost $ authority ^. authorityHost)
    where
        view k (node :| nodes) (Sites sites) = do
                (here, def, below) <- sites ^. L.at node
                maybe (k here)
                      (\nodes' -> view k nodes' below <|> k def)
                      (nonEmpty nodes)

singleSite :: Monad m => ByteString -> Site' m -> Sites' m
singleSite host = singleSite' host . DM.defVal

singleSitePort :: Monad m => ByteString -> Int -> Site' m -> Sites' m
singleSitePort host port = singleSite' host . DM.singleton port

singleSite' :: Monad m => ByteString -> DefMap Int (Site' m) -> Sites' m
singleSite' = \host -> let node :| nodes = atomiseHost host
                       in flip (foldl branch) nodes . leaf node
    where
        leaf node x = Sites $ M.singleton node (x, mempty, mempty)
        branch x node = Sites $ M.singleton node (mempty, mempty, x)

underSite :: Monad m => ByteString -> Site' m -> Sites' m
underSite host = underSite' host . DM.defVal

underSitePort :: Monad m => ByteString -> Int -> Site' m -> Sites' m
underSitePort host port = underSite' host . DM.singleton port

underSite' :: Monad m => ByteString -> DefMap Int (Site' m) -> Sites' m
underSite' = \host -> let node :| nodes = atomiseHost host
                      in flip (foldl branch) nodes . leaf node
    where
        leaf node x = Sites $ M.singleton node (mempty, x, mempty)
        branch x node = Sites $ M.singleton node (mempty, mempty, x)

atomiseHost :: ByteString -> NonEmpty ByteString
atomiseHost = fromJust . nonEmpty . BC.split '.'


newtype Site' m = Site (Path -> Query -> m (Resource' m))
type Site = Site' IO

retrieveResource :: Monad m => Site' m -> Path -> Query -> m (Resource' m)
retrieveResource (Site site) = site


data Method = HEAD | GET
    deriving Eq

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


data ExtraHeader = ExtraHeader HTTP.Header

unExtraHeader :: ExtraHeader -> HTTP.ResponseHeaders
unExtraHeader = \case
        ExtraHeader header -> [header]

data Entity = Entity {
        entityExtraHeaders :: [ExtraHeader],
        entityType :: ByteString,
        entityBody :: EntityBody}

data EntityBody
        = EntityBodyFromFile FilePath (Maybe WAI.FilePart)
        | EntityBodyFromBuilder Z.Builder

entityFromStrictText :: ByteString -> Text -> Entity
entityFromStrictText contentType body
        = entityFromStrictByteString (contentType <> "; charset=utf-8")
                                     (T.encodeUtf8 body)

entityFromStrictByteString :: ByteString -> ByteString -> Entity
entityFromStrictByteString entityType body = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromBuilder (Z.fromByteString body)

entityFromHtml :: ZH.Html -> Entity
entityFromHtml html = Entity{..} where
        entityExtraHeaders = []
        entityType = "text/html; charset=utf-8"
        entityBody = EntityBodyFromBuilder (ZH.renderHtmlBuilder html)


type Resource = Resource' IO
type MissingResource = MissingResource' IO
type ExistingResource = ExistingResource' IO
type Resource' m = Either (MissingResource' m) (ExistingResource' m)

data MissingResource' m = MissingResource {
        _missingBecause :: MissingBecause' m}

data MissingBecause' m
        = Moved Transience RelUri
        | NotFound NotFound (Maybe (m Entity))

data Transience = Permanently | Temporarily
data NotFound = Gone | NeverExisted

data ExistingResource' m = ExistingResource {
        _existingGet :: Maybe (m Entity)}

$(L.makeLenses ''MissingResource')
$(L.makeLenses ''ExistingResource')

missingResource :: Monad m =>
                   (MissingResource' m -> MissingResource' m) -> Resource' m
missingResource f = Left . f . MissingResource $ NotFound NeverExisted Nothing

existingResource :: Monad m =>
                    (ExistingResource' m -> ExistingResource' m) -> Resource' m
existingResource f = Right . f . ExistingResource $ Nothing


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


data Settings = Settings {
        _settingsProtocol :: Protocol,
        _settingsSites :: Sites,
        _settingsOnException :: X.IOException -> IO ()}
$(L.makeLenses ''Settings)

waiApplicationFromSitesForHttp :: (Settings -> Settings) -> WAI.Application
waiApplicationFromSitesForHttp f
        = waiApplicationFromSites
          $ (settingsProtocol .~ Http) . f

waiApplicationFromSitesForHttps :: (Settings -> Settings) -> WAI.Application
waiApplicationFromSitesForHttps f
        = waiApplicationFromSites
          $ (settingsProtocol .~ Https) . f

waiApplicationFromSites :: (Settings -> Settings) -> WAI.Application
{- ^ Formalisation of
<http://upload.wikimedia.org/wikipedia/commons/8/8a/Http-headers-status.svg>.
-}
waiApplicationFromSites f request
        = runHttpT defaultUgly
                   (httpMain (settings ^. settingsProtocol)
                             (settings ^. settingsSites))
                   request
           `X.catch` panic
    where
        settings = f $ Settings Http mempty (const $ return ())
        panic :: X.IOException -> IO WAI.Response
        panic e = do
                (settings ^. settingsOnException) e
                runHttpT defaultUgly (oops HTTP.internalServerError500) request


httpMain :: Monad m => Protocol -> Sites' m -> HttpT m WAI.Response
httpMain protocol sites = do

        authority
         <- maybe (oops HTTP.badRequest400) return
            . (parseAuthority (protocolPort protocol) =<<)
            =<< asksRequest WAI.requestHeaderHost

        site
         <- maybe (oops HTTP.badRequest400) return $ lookupSite authority sites

        method
         <- maybe (oops HTTP.notImplemented501) return
            =<< asksRequest (parseMethod . WAI.requestMethod)

        path <- asksRequest $ fromMaybe (pure "") . nonEmpty . WAI.pathInfo
        query <- asksRequest WAI.queryString
        let uri = Uri protocol authority path query

        (either <$> handleMissingResource uri <*> handleExistingResource) method
            =<< lift (retrieveResource site path query)


handleMissingResource
        :: Monad m =>
           Uri -> Method -> MissingResource' m -> HttpT m WAI.Response
handleMissingResource uri method resource = do
        --TODO: potentially handle PUT
        case resource ^. missingBecause of
                Moved transience relUri
                  -> oops' [(HTTP.hLocation, bsFromUri $ uri .+. relUri)]
                     $ case transience of
                        Permanently -> HTTP.movedPermanently301
                        Temporarily -> HTTP.temporaryRedirect307
                NotFound why mkEntity
                        --TODO: potentially handle POST
                  -> maybe (oops status)
                           (liftM (waiResponse (method == HEAD) status) . lift)
                           mkEntity
                  where
                        status = case why of
                                Gone         -> HTTP.gone410
                                NeverExisted -> HTTP.notFound404


handleExistingResource
        :: Monad m => Method -> ExistingResource' m -> HttpT m WAI.Response
handleExistingResource method resource = do
        --TODO: caching preconditions
        entity
         <- maybe (oops HTTP.methodNotAllowed405) lift $ case method of
                GET -> resource ^. existingGet
                       --TODO: range requests
                HEAD -> resource ^. existingGet
                        --TODO: early return for HEAD
        --TODO: content negotiation
        return $ waiResponse (method == HEAD) HTTP.ok200 entity


waiResponse :: Bool -> HTTP.Status -> Entity -> WAI.Response
waiResponse omitBody status Entity{..}
        | omitBody  = WAI.responseBuilder status headers mempty
        | otherwise = case entityBody of
                EntityBodyFromFile nf part
                 -> WAI.responseFile status headers nf part
                EntityBodyFromBuilder builder
                 -> WAI.responseBuilder status headers builder
    where
        headers = concatMap unExtraHeader entityExtraHeaders
                  ++ [(HTTP.hContentType, entityType)]
