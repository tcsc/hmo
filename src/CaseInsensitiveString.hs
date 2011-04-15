module CaseInsensitiveString (CaseInsensitiveString, fromString) where

import Data.Char

--| Wraps a standard string with a phantom type for performing case-insensitive
--  comparisons
newtype CaseInsensitiveString = CIS String deriving (Show)

instance Eq CaseInsensitiveString where
  CIS a == CIS b = (map toUpper a) == (map toUpper b)
  
instance Ord CaseInsensitiveString where 
  compare (CIS a) (CIS b) = compare (map toUpper a) (map toUpper b)
  
fromString :: String -> CaseInsensitiveString
fromString s = CIS s