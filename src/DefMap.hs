module DefMap (
        DefMap(),
        defAt,
        empty,
        singleton,
        defVal,
) where

-- base
import           Control.Applicative ((<|>))
import           Data.Monoid

-- containers
import           Data.Map (Map)
import qualified Data.Map               as M

-- lens
import qualified Control.Lens           as L
import           Control.Lens.Operators


newtype DefMap k v = DefMap (Maybe v, Map k v)
    deriving (Eq, Functor)

$(L.makeIso ''DefMap)

instance Ord k => Monoid (DefMap k v) where
        mempty = empty
        DefMap (xd, xm) `mappend` DefMap (yd, ym) = DefMap (xd <|> yd, xm <> ym)

defAt :: Ord k => k -> L.Getter (DefMap k v) (Maybe v)
defAt k f s = fmap (const s) . f $ (s ^. L.from defMap . L._2 . L.at k)
                                   <|> (s ^. L.from defMap . L._1)

empty :: DefMap k v
empty = DefMap (Nothing, M.empty)

singleton :: k -> v -> DefMap k v
singleton k v = DefMap (Nothing, M.singleton k v)

defVal :: v -> DefMap k v
defVal v = DefMap (Just v, M.empty)
