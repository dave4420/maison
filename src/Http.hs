module Http (
        -- * Sites
        Site, Site',
        Sites, Sites',
        singleSite, singleSitePort,
        underSite, underSitePort,
        sealSite,
        -- * Etc
        module Http.Entity,
        module Http.Uri,
        module Http.Resource,
        Method(..),
        Settings(), settingsSites, settingsOnException,
        waiApplication,
        NonEmpty(..),
)
where

import           Http.Entity
import           Http.Resource
import qualified Http.Sites                    as Q
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


type Site' m = Q.Site Resource' m
type Site = Site' IO

type Sites' m = Q.Sites Resource' m
type Sites = Sites' IO

singleSite, underSite :: Monad m => ByteString -> Site' m -> Sites' m
singleSite = Q.singleSite
underSite = Q.underSite

singleSitePort, underSitePort
        :: Monad m => ByteString -> Int -> Site' m -> Sites' m
singleSitePort = Q.singleSitePort
underSitePort = Q.underSitePort

sealSite :: (Path -> Query -> m (Resource' m)) -> Site' m
sealSite = Q.sealSite


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
            $ sites ^. Q.atAuthority authority

        method
         <- maybe (oops HTTP.notImplemented501) return
            =<< asksRequest (parseMethod . WAI.requestMethod)

        path <- asksRequest $ fromMaybe (pure "") . nonEmpty . WAI.pathInfo
        query <- asksRequest WAI.queryString
        let uri = Uri protocol authority path query

        (eitherResource <$> handleMissingResource uri
                        <*> handleExistingResource)
            method
            =<< lift (site ^! Q.atPathQuery path query)


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
