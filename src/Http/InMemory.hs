module Http.InMemory where

import           DefMap
import           Http

-- base
import           Data.Maybe

-- lens
import           Control.Lens.Operators

-- text
import           Data.Text (Text)


sealInMemorySite :: Monad m => DefMap (NonEmpty Text) (Resource' m) -> Site' m
sealInMemorySite table = Site $ \path _query -> return
        $ fromMaybe (Left defaultMissingResource) (table ^. defAt path)
