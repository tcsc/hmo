module Flags (enumerateFlags) where 

import Data.Bits
  
enumerateFlags :: (Eq a, Enum a, Bounded a) => Int -> [a]
enumerateFlags flags = enum (minBound) (maxBound) flags []
  where 
    enum :: (Eq a, Enum a) => a -> a -> Int -> [a] -> [a]
    enum val bound flags acc = 
      let acc' = case (fromEnum val) .&. flags of 
                   0 -> acc
                   n -> (toEnum n) : acc
      in if val == bound 
           then acc' 
           else enum (succ val) bound flags acc'