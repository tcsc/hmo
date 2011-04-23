module CaseInsensitiveString (
  CaseInsensitiveString, 
  fromString, 
  unitTests
) where

import Data.Char
import Test.HUnit

-- | Wraps a standard string with a phantom type for performing case-insensitive
--   comparisons
newtype CaseInsensitiveString = CIS String deriving (Show)

instance Eq CaseInsensitiveString where
  CIS a == CIS b = (map toUpper a) == (map toUpper b)
  
instance Ord CaseInsensitiveString where 
  compare (CIS a) (CIS b) = compare (map toUpper a) (map toUpper b)
  
fromString :: String -> CaseInsensitiveString
fromString s = CIS s

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList [
  "Simple Equality"    ~: True  ~=? (fromString "lower") == (fromString "lower"),
  "Mixed Eqaulity"     ~: True  ~=? (fromString "mIxeDCAse") == (fromString "MixedCase"),
  "Mixed Ordering LT"  ~: True  ~=? (fromString "a") <  (fromString "B"),
  "Mixed Ordering GTE" ~: False ~=? (fromString "a") >= (fromString "B"),
  "Mixed Ordering GT"  ~: True  ~=? (fromString "b") >  (fromString "A"),
  "Mixed Ordering LTE" ~: False ~=? (fromString "b") <= (fromString "A")
  ]
