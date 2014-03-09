module Http.Resource (
        Resource, Resource',
        eitherResource,
        ExistingResource, ExistingResource'(), existingResource,
        existingGet,
        MissingResource, MissingResource'(), missingResource,
        missingBecause,
        MissingBecause'(..),
        Transience(..),
        NotFound(..),
)
where

import           Http.Entity
import           Http.Uri

-- lens
import qualified Control.Lens                  as L


type Resource = Resource' IO
type MissingResource = MissingResource' IO
type ExistingResource = ExistingResource' IO

data Resource' m
        = Missing (MissingResource' m)
        | Existing (ExistingResource' m)

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
missingResource f
        = Missing . f . MissingResource $ NotFound NeverExisted Nothing

existingResource :: Monad m =>
                    (ExistingResource' m -> ExistingResource' m) -> Resource' m
existingResource f = Existing . f . ExistingResource $ Nothing

eitherResource :: (MissingResource' m -> a) ->
                  (ExistingResource' m -> a) ->
                  Resource' m ->
                  a
eitherResource f _ (Missing x) = f x
eitherResource _ g (Existing y) = g y
