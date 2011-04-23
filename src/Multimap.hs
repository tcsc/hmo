module Multimap (
  Multimap, 
  empty,
  fromList,
  lookup,
  insert,
  fold,
  unitTests) where
  
import Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit

data Multimap k v = MM (Map.Map k (Set.Set v))
  deriving(Show, Eq, Ord)
  
empty :: (Ord k, Ord v) => Multimap k v
empty = MM Map.empty

fromList :: (Ord k, Ord v) => [(k,v)] -> Multimap k v
fromList [] = empty
fromList l = List.foldl' (\mm (key,value) -> insert key value mm) empty l

insert :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
insert key value (MM m) = MM $ Map.alter setValue key m
 where
   setValue Nothing = Just (Set.singleton value)
   setValue (Just set) = Just (Set.insert value set)
   
lookup :: Ord k => k -> Multimap k v -> Maybe [v]
lookup key (MM m) = case Map.lookup key m of
                      Just s -> Just $ Set.elems s
                      Nothing -> Nothing

-- (key -> val -> b -> b) -> b -> Map k a -> b

fold :: ((k,v) -> a -> a) -> a -> Multimap k v -> a
fold f acc (MM m) = Map.foldrWithKey foldSet acc m
  where 
    foldSet key setV acc = Set.fold (\val a -> f (key, val) a) acc setV


-- ----------------------------------------------------------------------------
--  Unit Tests
-- ---------------------------------------------------------------------------- 
testFold = TestCase (do
  let mmap = fromList [(100, 1), (100,2), (200,3), (200,4), (200,5), (300,6)]
  assertEqual "fold" (1100, 21) $ 
    fold (\(key,val) (ak,av) -> (ak + key, av + val)) (0,0) mmap)

unitTests = TestList [testFold]
