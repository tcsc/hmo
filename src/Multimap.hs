module Multimap (
  Multimap, 
  empty,
  lookup,
  insert) where
  
import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Multimap k v = MM (Map.Map k (Set.Set v))
  deriving(Show, Eq, Ord)
  
empty :: (Ord k, Ord v) => Multimap k v
empty = MM Map.empty

insert :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
insert key value (MM m) = MM $ Map.alter setValue key m
 where
   setValue Nothing = Just (Set.singleton value)
   setValue (Just set) = Just (Set.insert value set)
   
lookup :: Ord k => k -> Multimap k v -> Maybe [v]
lookup key (MM m) = case Map.lookup key m of
                      Just s -> Just $ Set.elems s
                      Nothing -> Nothing
    