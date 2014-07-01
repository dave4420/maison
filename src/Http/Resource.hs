module Http.Resource (
        Resource(),
        eitherResource,
        ExistingResource(), existingResource,
        existingGet,
        MissingResource(), missingResource,
        missingBecause,
        MissingBecause(..),
        Transience(..),
        NotFound(..),
)
where

import           Http.Uri

-- lens
import qualified Control.Lens                  as L

-- semigroups
import           Data.Semigroup


data Resource r m
        = Missing (MissingResource r m)
        | Existing (ExistingResource r m)

instance Semigroup (Resource r m) where
        x @ (Existing _) <> _ = x
        _ <> y @ (Existing _) = y
        Missing x <> Missing y = Missing (x <> y)

instance Monad m => Monoid (Resource r m) where
        mempty = missingResource id
        mappend = (<>)

newtype MissingResource r m = MissingResource {
        _missingBecause :: MissingBecause r m}
    deriving Semigroup

data MissingBecause r m
        = Moved Transience RelUri
        | NotFound NotFound (Maybe (m r))

data Transience = Permanently | Temporarily
data NotFound = Gone | NeverExisted

instance Semigroup (MissingBecause r m) where
        x @ (Moved Permanently _) <> _ = x
        _ <> y @ (Moved Permanently _) = y
        x @ (Moved Temporarily _) <> _ = x
        _ <> y @ (Moved Temporarily _) = y
        x @ (NotFound Gone _) <> _ = x
        _ <> y @ (NotFound Gone _) = y
        x <> _ = x

data ExistingResource r m = ExistingResource {
        _existingGet :: Maybe (m r)}

missingResource :: Monad m =>
                   (MissingResource r m -> MissingResource r m) ->
                   Resource r m
missingResource f
        = Missing . f . MissingResource $ NotFound NeverExisted Nothing

existingResource :: Monad m =>
                    (ExistingResource r m -> ExistingResource r m) ->
                    Resource r m
existingResource f = Existing . f . ExistingResource $ Nothing

eitherResource :: (MissingResource r m -> a) ->
                  (ExistingResource r m -> a) ->
                  Resource r m ->
                  a
eitherResource f _ (Missing x) = f x
eitherResource _ g (Existing y) = g y

$(L.makeLenses ''MissingResource)
$(L.makeLenses ''ExistingResource)
