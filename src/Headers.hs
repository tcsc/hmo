module Headers(Headers,
               empty,
               getHeader,
               getHeaders,
               setHeader) 
where 

import qualified Multimap as MM
import CaseInsensitiveString
  
data Headers = HS (MM.Multimap CaseInsensitiveString String) 
  deriving (Show)
  
empty :: Headers
empty = HS MM.empty
  
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

