module Http.Sites (
        Sites, Sites',
        singleSite, singleSitePort,
        underSite, underSitePort,
        Site, Site'(..),
        lookupSite,
        retrieveResource,
)
where

import           DefMap (DefMap)
import qualified DefMap                        as DM
import           Http.Resource
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
