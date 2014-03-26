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
        module Control.Monad.Trans.Class,
)
where

import qualified Http.Auth                     as QA
import           Http.Entity
import qualified Http.Resource                 as QR
import qualified Http.Sites                    as QS
import           Http.Uri

-- base
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid

-- bytestring
import           Data.ByteString (ByteString)

-- errors
import           Control.Error

-- exceptions
import qualified Control.Monad.Catch           as X

-- http-types
import qualified Network.HTTP.Types            as HTTP

-- lens
import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- mtl
import           Control.Monad.Reader.Class

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


instance X.MonadCatch m => X.MonadCatch (EitherT e m) where
        throwM = lift . X.throwM
        catch main recover
                = EitherT $ X.catch (runEitherT main) (runEitherT . recover)
        mask = liftMask EitherT runEitherT X.mask
        uninterruptibleMask = liftMask EitherT runEitherT X.uninterruptibleMask

-- Adapted from MonadCatch instance for Identity in Control.Monad.Catch,
-- by Edward Kmett.
liftMask :: X.MonadCatch m
         => (forall a. m (s a) -> n a)
            -> (forall a. n a -> m (s a))
            -> (forall c. ((forall a. m a -> m a) -> m c) -> m c)
            -> ((forall a. n a -> n a) -> n b)
            -> n b
liftMask wrap unwrap mask a = wrap $ mask $ \u -> unwrap (a $ q u) where
        q u = wrap . u . unwrap


data Method = HEAD | GET
    deriving Eq

parseMethod :: ByteString -> Maybe Method
parseMethod "HEAD" = Just HEAD
parseMethod "GET"  = Just GET
parseMethod _      = Nothing


data Request = Request {_requestWaiRequest :: WAI.Request,
                        _requestUri :: Uri}
$(L.makeLenses ''Request)

newtype HttpT m a = HttpT (EitherT UglyStatus (ReaderT Request m) a)
    deriving (Functor, Applicative, Monad, X.MonadCatch, MonadReader Request)

instance MonadTrans HttpT where
        lift = HttpT . lift . lift

data UglyStatus = UglyStatus HTTP.ResponseHeaders HTTP.Status

oops :: Monad m => HTTP.Status -> HttpT m a
oops = oops' []

oops' :: Monad m => HTTP.ResponseHeaders -> HTTP.Status -> HttpT m a
oops' headers status = HttpT . left $ UglyStatus headers status

type Response = (HTTP.Status, Entity)

defaultUgly :: UglyStatus -> Response
defaultUgly (UglyStatus headers status)
        = (,) status
          . concatResponseHeaders headers
          . entityFromStrictByteString "text/html; charset=utf-8"
          . HTTP.statusMessage
          $ status


data Settings = Settings {
        _settingsSites :: Sites,
        _settingsOnException :: IOError -> HttpT IO ()}
$(L.makeLenses ''Settings)

waiApplication :: Protocol -> (Settings -> Settings) -> WAI.Application
{- ^ Formalisation of
<http://upload.wikimedia.org/wikipedia/commons/8/8a/Http-headers-status.svg>.
-}
waiApplication protocol f request
        = uncurry (waiResponse $ method' == Just HEAD)
          <$> maybe (return . defaultUgly $ UglyStatus [] HTTP.badRequest400)
                    (\authority
                     -> runReaderT (eitherT (return . defaultUgly) return ermx)
                                   (Request request
                                           (Uri protocol authority path query)))
                    authority'
    where
        settings = f $ Settings mempty (const $ return ())
        HttpT ermx = httpMain method' (settings ^. settingsSites)
                     `X.catch` panic
        panic :: IOError -> HttpT IO Response
        panic e = do
                (settings ^. settingsOnException) e
                oops HTTP.internalServerError500
        method' = parseMethod . WAI.requestMethod $ request
        authority' = parseAuthority (protocolPort protocol)
                     <=< WAI.requestHeaderHost
                     $ request
        path = fromMaybe (pure "") . nonEmpty . WAI.pathInfo $ request
        query = WAI.queryString request


httpMain :: Monad m => Maybe Method -> Sites' m -> HttpT m Response
httpMain method' sites = do

        site
         <- maybe (oops HTTP.badRequest400) return
            . (sites ^.)
            =<< L.view (requestUri . L.to (QS.atAuthority <$> uriAuthority))

        resource
         <- handleAuthentication
            =<< lift . (site ^!)
            =<< L.view (requestUri
                        . L.to (QS.atPathQuery <$> uriPath <*> uriQuery))

        method
         <- maybe (oops HTTP.notImplemented501) return method'

        QR.eitherResource handleMissingResource
                          (handleExistingResource method)
                          resource


handleAuthentication :: Monad m => Auth' m -> HttpT m (Resource' m)
handleAuthentication = \case
        QA.NoAuth (QA.Forbidden _entity)   -> oops HTTP.forbidden403
        QA.NoAuth (QA.OK resource)         -> return resource
        QA.AuthRequired challenges _entity -> do
                result
                 <- maybe (return $ QA.Forbidden Nothing) lift
                    =<< L.view (requestWaiRequest
                                . L.to (QA.checkAuthentication challenges
                                        <=< lookup HTTP.hAuthorization
                                            . WAI.requestHeaders))
                case result of
                        QA.Forbidden _entity'
                          -> oops' [("WWW-Authenticate",
                                     QA.renderChallenges challenges)]
                                   HTTP.unauthorized401
                        QA.OK resource
                          -> return resource


handleMissingResource
        :: Monad m =>
           MissingResource' m -> HttpT m Response
handleMissingResource resource = do
        --TODO: potentially handle PUT
        case resource ^. missingBecause of
                QR.Moved transience relUri
                  -> do
                        absUri <- L.view $ requestUri . L.to (.+. relUri)
                        oops' [(HTTP.hLocation, bsFromUri absUri)]
                            $ case transience of
                                QR.Permanently -> HTTP.movedPermanently301
                                QR.Temporarily -> HTTP.temporaryRedirect307
                QR.NotFound why mkEntity
                        --TODO: potentially handle POST
                  -> maybe (oops status)
                           (liftM ((,) status) . lift)
                           mkEntity
                  where
                        status = case why of
                                QR.Gone         -> HTTP.gone410
                                QR.NeverExisted -> HTTP.notFound404


handleExistingResource
        :: Monad m => Method -> ExistingResource' m -> HttpT m Response
handleExistingResource method resource = do
        --TODO: caching preconditions
        entity
         <- maybe (oops HTTP.methodNotAllowed405) lift $ case method of
                GET -> resource ^. existingGet
                       --TODO: range requests
                HEAD -> resource ^. existingGet
                        --TODO: early return for HEAD
        --TODO: content negotiation
        return (HTTP.ok200, entity)
