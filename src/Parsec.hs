{-# LANGUAGE FlexibleContexts #-}

module Parsec(
  uri, 
  decimalInteger,
  hexInteger, 
  maybeInt, 
  isImplicit, 
  quoted,
  optionallyQuoted,
  caseInsensitiveString,
  URI ) where

import Network.URI  
import Data.Char (digitToInt, toLower, toUpper, isAlpha)
import Data.Maybe
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String

uri :: (Stream s m Char) => ParsecT s u m URI
uri = do
  s <- many1 (noneOf " \n\r\t")
  case parseURI s of 
    Just uri -> return uri
    Nothing -> fail "Uri"

integer :: (Stream s m Char) => ParsecT s u m Integer
integer = do
  t <- many1 digit
  case maybeInt t of 
    Just n -> return n
    Nothing -> fail "Integer"
    
decimalInteger :: (Stream s m Char) => ParsecT s u m Integer
decimalInteger = number 10 digit

hexInteger :: (Stream s m Char) => ParsecT s u m Integer
hexInteger = number 16 hexDigit

number base baseDigit = do 
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

isImplicit p s = do
  pos <- getPosition 
  p
  setPosition pos
  return s
  
maybeInt s = case (reads s :: [(Integer, String)]) of
               [] -> Nothing
               [(n, _)] -> Just n

quoted p = do char '\"'
              x <- p 
              char '\"'
              return x
              
optionallyQuoted p = do q <- optionMaybe $ char '\"';
                        v <- p
                        if isNothing q then return () 
                                       else do { char '\"'; return (); };
                        return v

caseInsensitiveString s = do { walk s; return s; }
  where walk []     = return ()
        walk (c:cs) = do { caseChar c <?> msg; walk cs }
        caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                    | otherwise  = char c
        msg         = show s