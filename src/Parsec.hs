{-# LANGUAGE FlexibleContexts #-}

module Parsec(uri, integer, maybeInt, isImplicit, URI) where

import Network.URI  
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


isImplicit p s = do
  pos <- getPosition 
  p
  setPosition pos
  return s


maybeInt s = case (reads s :: [(Integer, String)]) of
               [] -> Nothing
               [(n, _)] -> Just n

