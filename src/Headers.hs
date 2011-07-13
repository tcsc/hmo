module Headers(Headers,
               empty,
               get,
               getHeaders,
               set,
               fold,
               Headers.map,
               Headers.unitTests) 
where 

import qualified Data.List as List
import qualified Multimap as MM
import CaseInsensitiveString
import Test.HUnit
  
data Headers = HS (MM.Multimap CaseInsensitiveString String) 
  deriving (Show)
  
empty :: Headers
empty = HS MM.empty

fromList :: [(String,String)] -> Headers
fromList hs = HS $ MM.fromList $ List.map (\(h,v) -> (fromString h, v)) hs
  
-- | Fetches a single header value from a header collection
get :: String -> Headers -> Maybe String
get h hs = case getHeaders h hs of
             Just [] -> Nothing
             Just hs -> Just $ head hs
             Nothing -> Nothing
                
set :: String -> String -> Headers -> Headers
set n v (HS mmap) = HS $ MM.insert (fromString n) v mmap 
  
-- | Fetches a set of header values from a header collection
getHeaders :: String -> Headers -> Maybe [String]
getHeaders h (HS mmap) = MM.lookup (fromString h) mmap

-- | Implements a basic fold over the entire set of headers
fold :: ((String, String) -> a -> a) -> a -> Headers -> a
fold f seed (HS mmap) = MM.fold (\(h,v) acc -> f (asString h, v) acc) seed mmap 

map :: ((String, String) -> a) -> Headers -> [a]
map f (HS mmap) = MM.map  (\(h,v) -> f(asString h, v)) mmap

-- ----------------------------------------------------------------------------
--  Unit Tests
-- ----------------------------------------------------------------------------
unitTests = TestList [testFold]

testFold = TestCase (do
  let hs = fromList [("alpha", "a1"), ("Alpha", "a2"), ("Beta", "b1")]
  let result = fold (\(h,v) (hs,vs) -> (head h : hs, head v : vs)) ([],[]) hs
  assertEqual "Fold" ("aaB", "aab") result)
  
