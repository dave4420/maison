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


data Resource r m
        = Missing (MissingResource r m)
        | Existing (ExistingResource r m)

data MissingResource r m = MissingResource {
        _missingBecause :: MissingBecause r m}

data MissingBecause r m
        = Moved Transience RelUri
        | NotFound NotFound (Maybe (m r))

data Transience = Permanently | Temporarily
data NotFound = Gone | NeverExisted

data ExistingResource r m = ExistingResource {
        _existingGet :: Maybe (m r)}

$(L.makeLenses ''MissingResource)
$(L.makeLenses ''ExistingResource)

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
