{-# LANGUAGE NoMonomorphismRestriction #-}

module Rtsp (
  Message (..),
  Packet (..),
  Status (..),
  Verb (..),
  Version,
  embeddedPacket,
  extractMessageBytes,
  parseMessage,
  formatMessage,
  formatPacket,
  msgSequence,
  msgHeaders,
  msgContentLength,
  unitTests
) where

import Data.Binary.Strict.Get
import Data.Binary.Put
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as Utf8
import Data.ByteString.Search
import Network.URI
import Test.HUnit
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
             
data Packet = Packet Int B.ByteString deriving (Show)
  
instance Show Message where 
  show (Request s v u _ _) = "#" ++ (show s) ++ " " ++ (show v) ++ " " ++ (show u) 
  show (Response _ s _) = (show s) 

-- | gets the sequence number of a message
msgSequence :: Message -> Int
msgSequence (Request s _ _ _ _ ) = s 
msgSequence (Response s _ _) = s 

msgHeaders :: Message -> Headers
msgHeaders (Request _ _ _ _ hs) = hs 
msgHeaders (Response _ _ hs) = hs 

msgContentLength :: Message -> Int
msgContentLength (Request _ _ _ _ hs) = maybe 0 id (contentLength hs)
msgContentLength (Response _ _ _) = 0 

-- | Fills in the missing bits of a half-parsed message once we know what they are
msgUpdate :: Int -> Headers -> Message -> Message
msgUpdate s headers (Request _ verb uri ver _) = Request s verb uri ver headers
msgUpdate s headers (Response _ status _) = Response s status headers  

parseMessage :: B.ByteString -> Maybe Message
parseMessage bytes = case parse message "" bytes of
                       Left _ -> Nothing
                       Right msg -> Just msg

message = do 
    msg <- (try request) <|> response  
    endOfLine
    h <- headers
    sq <- case readSequence h of 
            Just n -> return n
            Nothing -> fail "Missing sequence number"
    return $ msgUpdate sq h msg
  where
    readSequence :: Headers -> Maybe Int
    readSequence hs = do 
      s <- getHeader "cseq" hs
      maybeInt s
      
contentLength :: Headers -> Maybe Int
contentLength hs = case (getHeader "content-length" hs) of
                     Nothing -> Nothing
                     Just txt -> maybeInt txt
      
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

embeddedPacket :: B.ByteString -> Maybe (Packet, B.ByteString)
embeddedPacket bytes = case runGet getPacket bytes of
                         (Right p, remainder) -> Just (p, remainder)
                         (Left _, _) -> Nothing
  where getPacket :: Get Packet
        getPacket = do 
          _ <- getWord8
          channel <- getWord8
          size <- getWord16be
          packet <- getByteString (fromIntegral size)
          return $ Packet (fromIntegral channel) packet 
          
          
extractMessageBytes :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
extractMessageBytes s =
  let (h,t) = breakAfter endOfMessage s
  in case (endOfMessage `B.isSuffixOf` h) of
    True -> Just (h, t)
    False -> Nothing
  
formatMessage :: Message -> Maybe B.ByteString -> B.ByteString
formatMessage (Request s v uri ver hs) _ = B.empty

formatPacket :: Packet -> B.ByteString
formatPacket (Packet channel bytes) = (B.concat . L.toChunks) $ runPut $ do 
  putWord8 (fromIntegral 0x24)
  putWord8 (fromIntegral channel)
  putWord16be (fromIntegral (B.length bytes))
  putByteString bytes
  
endOfMessage :: B.ByteString 
endOfMessage = B.pack [13, 10, 13, 10] 
          
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

-- ----------------------------------------------------------------------------
-- Static data
-- ----------------------------------------------------------------------------

hdrSequence = "CSeq"
hdrContentLength = "Content-Length"
 
-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------

testParseMinimalRequest = TestCase(do 
  let bytes = Utf8.fromString "DESCRIBE rtsp://localhost/root/1 RTSP/1.0\r\nCSeq: 1\r\n\r\n"
  let mmsg = parseMessage bytes
  assertBool "Parse Success" (isJust mmsg)
  let (Request sq verb uri ver hdr) = fromJust mmsg
  assertEqual "sequence" 1 sq
  assertEqual "verb" Describe verb
  assertEqual "version" (1,0) ver
  assertEqual "contentLength" 0 (maybe 0 (id) (contentLength hdr)))

testParsePacket = TestCase (do
  let bytes = B.pack [0x24, 0x01, 0x00, 0x05, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
  let mp = embeddedPacket bytes
  assertBool "Parse failed" (isJust mp)
  let ((Packet channel payload), remainder) = fromJust mp
  assertEqual "channel" channel 1
  assertEqual "payload size" 5 (B.length payload)
  assertEqual "remainder size" 2 (B.length remainder))
  
testParsePacketNotEnoughData = TestCase (do
    let bytes = B.pack [0x24, 0x01, 0x00, 0x10, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
    let mp = embeddedPacket bytes
    assertBool "Parse failed" (isNothing mp))
  
testParseVerb s v = 
  case parse verb "" (Utf8.fromString s) of
    Right verb -> (v == verb)
    Left _ -> False
  
testVerbs = TestList [
  "Parse Describe" ~: testParseVerb "DESCRIBE" Describe ~=? True,
  "Parse Announce" ~: testParseVerb "ANNOUNCE" Announce ~=? True,
  "Parse Setup" ~: testParseVerb "SETUP" Setup ~=? True,
  "Parse Play" ~: testParseVerb "PLAY" Play ~=? True,
  "Parse Teardown" ~: testParseVerb "TEARDOWN" Teardown ~=? True  ]

unitTests = TestList [TestLabel "Parse Minimal Request" testParseMinimalRequest,
                      TestLabel "Packet Parsing - simple" testParsePacket,
                      TestLabel "Packet Parsing - insuffcient data" testParsePacketNotEnoughData,
                      testVerbs]