module Http.Sites (
        Sites(),
        singleSite, singleSitePort,
        underSite, underSitePort,
        Site(),
        sealSite,
        atAuthority,
        atPathQuery,
)
where

import           DefMap (DefMap)
import qualified DefMap                        as DM
import           Http.Uri

-- base
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8         as BC

-- containers
import           Data.Map (Map)
import qualified Data.Map                      as M

-- lens
import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- semigroups
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty            as SG


newtype Sites r m
        = Sites (Map ByteString (DefMap Int (Site r m), -- here
                                 DefMap Int (Site r m), -- def for below
                                 Sites r m))            -- below

instance Monoid (Sites r m) where
        mempty = Sites mempty
        mappend (Sites x) (Sites y) = Sites (M.unionWith (<>) x y)

atAuthority :: Authority -> L.Getter (Sites r m) (Maybe (Site r m))
atAuthority authority
        = L.to $ view (SG.reverse . atomiseHost $ authority ^. authorityHost)
    where
        view (node :| nodes) (Sites sites) = do
                (here, def, below) <- sites ^. L.at node
                maybe (k here)
                      (\nodes' -> view nodes' below <|> k def)
                      (nonEmpty nodes)
        k = L.view (DM.defAt $ authority ^. authorityPort)

singleSite :: Monad m => ByteString -> Site r m -> Sites r m
singleSite host = singleSite' host . DM.defVal

singleSitePort :: Monad m => ByteString -> Int -> Site r m -> Sites r m
singleSitePort host port = singleSite' host . DM.singleton port

singleSite' :: Monad m => ByteString -> DefMap Int (Site r m) -> Sites r m
singleSite' = \host -> let node :| nodes = atomiseHost host
                       in flip (foldl branch) nodes . leaf node
    where
        leaf node x = Sites $ M.singleton node (x, mempty, mempty)
        branch x node = Sites $ M.singleton node (mempty, mempty, x)

underSite :: Monad m => ByteString -> Site r m -> Sites r m
underSite host = underSite' host . DM.defVal

underSitePort :: Monad m => ByteString -> Int -> Site r m -> Sites r m
underSitePort host port = underSite' host . DM.singleton port

underSite' :: Monad m => ByteString -> DefMap Int (Site r m) -> Sites r m
underSite' = \host -> let node :| nodes = atomiseHost host
                      in flip (foldl branch) nodes . leaf node
    where
        leaf node x = Sites $ M.singleton node (mempty, x, mempty)
        branch x node = Sites $ M.singleton node (mempty, mempty, x)

atomiseHost :: ByteString -> NonEmpty ByteString
atomiseHost = fromJust . nonEmpty . BC.split '.'


newtype Site r m = Site (Path -> Query -> m (r m))

atPathQuery :: Monad m => Path -> Query -> L.Action m (Site r m) (r m)
atPathQuery path query = L.act $ \(Site f) -> f path query

sealSite :: (Path -> Query -> m (r m)) -> Site r m
sealSite = Site
