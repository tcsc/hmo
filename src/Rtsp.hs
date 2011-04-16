{-# LANGUAGE NoMonomorphismRestriction #-}
module Rtsp where

import Data.Char
import Data.List
import qualified Data.ByteString.UTF8 as Utf8
import Network.URI
import Text.Parsec
import Text.Parsec.Char

import qualified Multimap as MM
import Headers

data Status = OK
            | NotFound
            | InternalServerError
 deriving (Show, Eq)

instance Enum Status where 
  toEnum = Rtsp.toEnum
  fromEnum = Rtsp.fromEnum

data Verb = Describe | Announce | Setup | Play | Teardown
  deriving (Eq, Show)

type Version = (Int, Int)

data Message = Request Int Verb URI Version Headers
             | Response Int Status Headers
  
instance Show Message where 
  show (Request s v u _ _) = "#" ++ (show s) ++ " " ++ (show v) ++ " " ++ (show u) 
  show (Response _ s _) = (show s) 

-- | gets the sequence number of a message
msgSequence :: Message -> Int
msgSequence (Request s _ _ _ _) = s 
msgSequence (Response s _ _) = s 

msgHeaders :: Message -> Headers
msgHeaders (Request _ _ _ _ hs) = hs 
msgHeaders (Response _ _ hs) = hs 

-- | Fills in the missing bits of a half-parsed message once we know what they are
msgUpdate :: Int -> Headers -> Message -> Message
msgUpdate s headers (Request _ verb uri ver _) = Request s verb uri ver headers
msgUpdate s headers (Response _ status _) = Response s status headers  

message = do 
    msg <- (try request) <|> response  
    endOfLine
    h <- headers
    s <- case readSequence h of 
           Just n -> return n
           Nothing -> fail "Missing sequence number"
    return $ msgUpdate s h msg
  where
    readSequence :: Headers -> Maybe Int
    readSequence hs = do 
      s <- getHeader "cseq" hs
      maybeInt s
      
request = do 
  v <- verb
  spaces
  u <- uri
  spaces
  ver <- version
  return $ Request (-1) v u ver empty

response = do
  ver <- version
  spaces  
  s <- status
  many (noneOf ['\r','\n']) 
  return $ Response (-1) s empty
  
verb = do 
  text <- many1 letter
  let verb = case map (toUpper) text of
               "ANNOUNCE" -> Announce
               "DESCRIBE" -> Describe
               "SETUP"    -> Setup
               "PLAY"     -> Play
               "TEARDOWN" -> Teardown
  return verb 
  
uri = do
  s <- manyTill anyChar space
  case parseURI s of 
    Just uri -> return uri
    Nothing -> fail "Uri"
  
integer = do
  t <- many1 digit
  case maybeInt t of 
    Just n -> return n
    Nothing -> fail "Integer"
    
version = do 
  string ("RTSP/")
  major <- integer
  char '.'
  minor <- integer
  return (major, minor) 

-- | 
headers = do
  hs <- manyTill header endOfLine
  return $ foldl' (\acc (n, v) -> setHeader n v acc) (Headers.empty) hs
  
header = do 
  n <- manyTill anyChar (char ':')
  spaces 
  v <- manyTill anyChar endOfLine
  return (n, v) 

status = do
  t <- many1 digit
  let i = read t :: Int
  return $ Rtsp.toEnum i

endOfLine = do
   try (string "\r\n") 
          <|> string "\r"
          <|> string "\n"

maybeInt s = case (reads s :: [(Int, String)]) of
               [] -> Nothing
               [(n, _)] -> Just n


toEnum :: Int -> Status
toEnum n = case n of 
             200 -> OK
             404 -> NotFound
             500 -> InternalServerError 

fromEnum :: Status -> Int
fromEnum s = case s of 
               OK -> 200
               NotFound -> 404
               InternalServerError -> 500