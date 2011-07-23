{-# LANGUAGE TypeSynonymInstances #-}

module Hex where 

import Char
import Control.Monad
import Data.Bits
import Data.Word
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Char8 as B

class Hex t where 
  hex :: t -> String
  --unhex :: Monad m => String -> m t
     
instance Hex String where
  hex = concatMap (hexify . c2w)
  
instance Hex Word8 where
  hex = hexify
     
hexify :: Word8 -> String
hexify w = let ls = enc $ w .&. 0x0f
               ms = enc $ w `shiftR` 4
               enc = intToDigit . fromIntegral
           in [ms, ls]
  
hexDigits :: B.ByteString
hexDigits = B.pack "0123456789ABCDEF"