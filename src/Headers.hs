module Headers(Headers,
               empty,
               getHeader,
               getHeaders,
               setHeader,
               fold,
               Headers.unitTests) 
where 

import qualified Multimap as MM
import CaseInsensitiveString
import Test.HUnit
  
data Headers = HS (MM.Multimap CaseInsensitiveString String) 
  deriving (Show)
  
empty :: Headers
empty = HS MM.empty

fromList :: [(String,String)] -> Headers
fromList hs = HS $ MM.fromList $ map (\(h,v) -> (fromString h, v)) hs
  
-- | Fetches a single header value from a header collection
getHeader :: String -> Headers -> Maybe String
getHeader h hs = case getHeaders h hs of
                   Just [] -> Nothing
                   Just hs -> Just $ head hs
                   Nothing -> Nothing
                   
setHeader :: String -> String -> Headers -> Headers
setHeader n v (HS mmap) = HS $ MM.insert (fromString n) v mmap 
  
-- | Fetches a set of header values from a header collection
getHeaders :: String -> Headers -> Maybe [String]
getHeaders h (HS mmap) = MM.lookup (fromString h) mmap

-- | Implements a basic fold over the entire set of headers
fold :: ((String, String) -> a -> a) -> a -> Headers -> a
fold f seed (HS mmap) = MM.fold (\(h,v) acc -> f (asString h, v) acc) seed mmap 

-- ----------------------------------------------------------------------------
--  Unit Tests
-- ----------------------------------------------------------------------------
unitTests = TestList [testFold]

testFold = TestCase (do
  let hs = fromList [("alpha", "a1"), ("Alpha", "a2"), ("Beta", "b1")]
  let result = fold (\(h,v) (hs,vs) -> (head h : hs, head v : vs)) ([],[]) hs
  assertEqual "Fold" ("aaB", "aab") result)
  
