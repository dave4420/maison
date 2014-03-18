module Http (
        -- * Sites
        Site, Site',
        Sites, Sites',
        singleSite, singleSitePort,
        underSite, underSitePort,
        -- * Auth
        Auth, Auth',
        AuthChallenge, AuthChallenge',
        AuthResult, AuthResult',
        QA.Realm, QA.Username, QA.Password,
        noAuth,
        basicAuth,
        -- * Resources
        Resource, Resource',
        ExistingResource, ExistingResource'(), existingResource,
        existingGet,
        MissingResource, MissingResource'(), missingResource,
        missingBecause,
        MissingBecause, MissingBecause', moved, notFound,
        QR.Transience(..),
        QR.NotFound(..),
        -- * Etc
        sealSite, sealSiteNoAuth,
        module Http.Entity,
        module Http.Uri,
        Method(..),
        Settings(), settingsSites, settingsOnException,
        waiApplication,
        NonEmpty(..),
)
where

import qualified Http.Auth                     as QA
import           Http.Entity
import qualified Http.Resource                 as QR
import qualified Http.Sites                    as QS
import           Http.Uri

-- base
import           Control.Applicative
import qualified Control.Exception             as X
import           Control.Monad
import           Data.Maybe
import           Data.Monoid

-- blaze-builder
import qualified Blaze.ByteString.Builder      as Z

-- bytestring
import           Data.ByteString (ByteString)

-- errors
import           Control.Error

-- http-types
import qualified Network.HTTP.Types            as HTTP

-- lens
import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- semigroups
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)

-- transformers
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

-- wai
import qualified Network.Wai                   as WAI


type Site' m = QS.Site (QA.Auth QR.Resource Entity) m
type Site = Site' IO

type Sites' m = QS.Sites (QA.Auth QR.Resource Entity) m
type Sites = Sites' IO

singleSite, underSite :: Monad m => ByteString -> Site' m -> Sites' m
singleSite = QS.singleSite
underSite = QS.underSite

singleSitePort, underSitePort
        :: Monad m => ByteString -> Int -> Site' m -> Sites' m
singleSitePort = QS.singleSitePort
underSitePort = QS.underSitePort


type Auth = Auth' IO
type AuthChallenge = AuthChallenge' IO
type AuthResult = AuthResult' IO

type Auth' m = QA.Auth QR.Resource Entity m
type AuthChallenge' m = QA.AuthChallenge QR.Resource Entity m
type AuthResult' m = QA.AuthResult QR.Resource Entity m

noAuth :: Resource' m -> Auth' m
noAuth = QA.noAuth

basicAuth :: Monad m
          => QA.Realm
             -> (QA.Username -> QA.Password -> m Bool)
             -> (QA.Username -> m (Resource' m))
             -> Auth' m
basicAuth = QA.basicAuth


type Resource = Resource' IO
type MissingResource = MissingResource' IO
type ExistingResource = ExistingResource' IO
type MissingBecause = MissingBecause' IO

type Resource' m = QR.Resource Entity m
type MissingResource' m = QR.MissingResource Entity m
type ExistingResource' m = QR.ExistingResource Entity m
type MissingBecause' m = QR.MissingBecause Entity m

missingResource :: Monad m =>
                   (MissingResource' m -> MissingResource' m) ->
                   Resource' m
missingResource = QR.missingResource

missingBecause :: L.Lens' (MissingResource' m) (MissingBecause' m)
missingBecause = QR.missingBecause

moved :: Monad m => QR.Transience -> RelUri -> MissingBecause' m
moved = QR.Moved

notFound :: Monad m => QR.NotFound -> Maybe (m Entity) -> MissingBecause' m
notFound = QR.NotFound

existingResource :: Monad m =>
                    (ExistingResource' m -> ExistingResource' m) ->
                    Resource' m
existingResource = QR.existingResource

existingGet :: L.Lens' (ExistingResource' m) (Maybe (m Entity))
existingGet = QR.existingGet


sealSite :: Monad m => (Path -> Query -> m (Auth' m)) -> Site' m
sealSite = QS.sealSite

sealSiteNoAuth :: Monad m => (Path -> Query -> m (Resource' m)) -> Site' m
sealSiteNoAuth = sealSite . (fmap . fmap . liftM) QA.noAuth


data Method = HEAD | GET
    deriving Eq

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


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

waiApplication :: Protocol -> (Settings -> Settings) -> WAI.Application
{- ^ Formalisation of
<http://upload.wikimedia.org/wikipedia/commons/8/8a/Http-headers-status.svg>.
-}
waiApplication protocol f request
        = runHttpT defaultUgly
                   (httpMain (settings ^. settingsProtocol)
                             (settings ^. settingsSites))
                   request
           `X.catch` panic
    where
        settings = f $ Settings protocol mempty (const $ return ())
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
         <- maybe (oops HTTP.badRequest400) return
            $ sites ^. QS.atAuthority authority

        path <- asksRequest $ fromMaybe (pure "") . nonEmpty . WAI.pathInfo
        query <- asksRequest WAI.queryString
        let uri = Uri protocol authority path query

        resource
         <- handleAuthentication
            =<< lift (site ^! QS.atPathQuery path query)

        method
         <- maybe (oops HTTP.notImplemented501) return
            =<< asksRequest (parseMethod . WAI.requestMethod)

        (QR.eitherResource <$> handleMissingResource uri
                           <*> handleExistingResource)
            method
            resource


handleAuthentication :: Monad m => Auth' m -> HttpT m (Resource' m)
handleAuthentication = \case
        QA.NoAuth (QA.Forbidden _entity)   -> oops HTTP.forbidden403
        QA.NoAuth (QA.OK resource)         -> return resource
        QA.AuthRequired challenges _entity -> do
                result
                 <- maybe (return $ QA.Forbidden Nothing) lift
                    =<< asksRequest (QA.checkAuthentication challenges
                                     <=< lookup HTTP.hAuthorization
                                         . WAI.requestHeaders)
                case result of
                        QA.Forbidden _entity'
                          -> oops' [("WWW-Authenticate",
                                     QA.renderChallenges challenges)]
                                   HTTP.unauthorized401
                        QA.OK resource
                          -> return resource


handleMissingResource
        :: Monad m =>
           Uri -> Method -> MissingResource' m -> HttpT m WAI.Response
handleMissingResource uri method resource = do
        --TODO: potentially handle PUT
        case resource ^. missingBecause of
                QR.Moved transience relUri
                  -> oops' [(HTTP.hLocation, bsFromUri $ uri .+. relUri)]
                     $ case transience of
                        QR.Permanently -> HTTP.movedPermanently301
                        QR.Temporarily -> HTTP.temporaryRedirect307
                QR.NotFound why mkEntity
                        --TODO: potentially handle POST
                  -> maybe (oops status)
                           (liftM (waiResponse (method == HEAD) status) . lift)
                           mkEntity
                  where
                        status = case why of
                                QR.Gone         -> HTTP.gone410
                                QR.NeverExisted -> HTTP.notFound404


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
