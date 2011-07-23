{-# LANGUAGE NoMonomorphismRestriction #-}

module Rtsp (
  Message (..),
  MessageBody,
  SequenceNumber,
  Packet (..),
  Status (..),
  Verb (..),
  Version,
  embeddedPacket,
  extractMessageBytes,
  parseMessage,
  formatMessage,
  formatPacket,
  hdrSession,
  hdrAuthenticate,
  msgSequence,
  msgHeaders,
  msgGetHeaderValue,
  msgSetHeaders,
  msgSequenceNumber,
  msgContentLength,
  Rtsp.unitTests
) where

import Data.Binary.Strict.Get
import Data.Binary.Put
import qualified Data.Bimap as Bimap
import Data.Char
import Data.Either.Utils
import Data.List
import qualified Data.Map as Map
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
import qualified Headers as Headers
import Parsec

data Status = OK
            | BadRequest
            | AuthorizationRequired
            | NotFound
            | SessionNotFound
            | InternalServerError
            | NotImplemented
            | BadGateway
            | ServiceUnavailable
            | GatewayTimeout
            | VersionNotSupported
            | OptionNotSupported
            | Unknown Int
 deriving (Show, Eq, Ord)

instance Enum Status where 
  toEnum = Rtsp.toStatus
  fromEnum = fromStatus

data Verb = Describe | Announce | Setup | Play | Teardown | OtherVerb String
  deriving (Eq, Show)

type Method = String
type SequenceNumber = Int
type Version = (Int, Int)

data Message = Request SequenceNumber Method   URI Version Headers.Headers
             | Response SequenceNumber Status Headers.Headers

type MessageBody = Maybe B.ByteString
             
data Packet = Packet Int B.ByteString deriving (Show)
  
instance Show Message where 
  show (Request s v u _ hs) = "#" ++ (show s) ++ " " ++ (show v) ++ " " ++ (show u) 
                                -- ++ "\n" 
                                -- ++ (intercalate "\n" $ Headers.map (\(n,v) -> n ++ ": " ++ v) hs) 
  show (Response _ s _) = (show s) 

-- | gets the sequence number of a message
msgSequence :: Message -> Int
msgSequence (Request s _ _ _ _ ) = s 
msgSequence (Response s _ _) = s 

msgHeaders :: Message -> Headers.Headers
msgHeaders (Request _ _ _ _ hs) = hs 
msgHeaders (Response _ _ hs) = hs 

msgGetHeaderValue :: Message -> String -> Maybe String
msgGetHeaderValue msg n = Headers.get n $ msgHeaders msg

msgSetHeaders :: Message -> [(String, String)] -> Message
msgSetHeaders (Response cseq stat hs) values = 
  let hs' = foldl' (\h (n, v) -> Headers.set n v h) hs values
  in Response cseq stat hs'

msgSequenceNumber :: Message -> SequenceNumber
msgSequenceNumber (Request cseq _ _ _ _) = cseq
msgSequenceNumber (Response cseq _ _) = cseq

msgContentLength :: Message -> Int
msgContentLength (Request _ _ _ _ hs) = maybe 0 fromIntegral (contentLength hs)
msgContentLength (Response _ _ _) = 0 

-- | Fills in the missing bits of a half-parsed message once we know what they are
msgUpdate :: Int -> Headers.Headers -> Message -> Message
msgUpdate s headers (Request _ verb uri ver _) = Request s verb uri ver headers
msgUpdate s headers (Response _ status _) = Response s status headers  

-- | 
parseMessage :: B.ByteString -> Maybe Message
parseMessage bytes = case parse message "" bytes of
                       Left _ -> Nothing
                       Right msg -> Just msg

hdrSession :: String
hdrSession = "Session"

hdrAuthenticate :: String
hdrAuthenticate = "WWW-Authenticate"

-- ----------------------------------------------------------------------------
--  RTSP parser
-- ----------------------------------------------------------------------------

message = do 
    msg <- (try request) <|> response  
    endOfLine
    h <- headers
    sq <- case readSequence h of 
            Just n -> return (fromIntegral n)
            Nothing -> fail "Missing sequence number"
    return $ msgUpdate sq h msg
  where
    readSequence :: Headers.Headers -> Maybe Integer
    readSequence hs = do 
      s <- Headers.get "cseq" hs
      maybeInt s
      
contentLength :: Headers.Headers -> Maybe Integer
contentLength hs = case (Headers.get "content-length" hs) of
                     Nothing -> Nothing
                     Just txt -> maybeInt txt
      
request = do 
  v <- many1 letter --verb
  spaces
  u <- uri
  spaces
  ver <- version
  return $ Request (-1) v u ver Headers.empty

response = do
  ver <- version
  spaces  
  s <- status
  many (noneOf ['\r','\n']) 
  return $ Response (-1) s Headers.empty
  
verb = do 
  text <- many1 letter
  let verb = case map (toUpper) text of
               "ANNOUNCE" -> Announce
               "DESCRIBE" -> Describe
               "SETUP"    -> Setup
               "PLAY"     -> Play
               "TEARDOWN" -> Teardown
               s          -> OtherVerb text
  return verb 
  
    
version = do 
  string ("RTSP/")
  major <- decimalInteger
  char '.'
  minor <- decimalInteger
  return (fromIntegral major, fromIntegral minor) 

-- | 
headers = do
  hs <- manyTill header endOfLine
  return $ foldl' (\acc (n, v) -> Headers.set n v acc) (Headers.empty) hs
  
header = do 
  n <- manyTill anyChar (char ':')
  spaces 
  v <- manyTill anyChar endOfLine
  return (n, v) 

status = do
  t <- many1 digit
  let i = read t :: Int
  return $ Rtsp.toStatus i

endOfLine = do
   try (string "\r\n") 
          <|> string "\r"
          <|> string "\n"

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

toStatus :: Int -> Status
toStatus n = case Bimap.lookup n statusMap of
             Just s -> s
             Nothing -> Unknown n

fromStatus :: Status -> Int
fromStatus (Unknown n) = n 
fromStatus s = (Bimap.!>) statusMap s

-- ----------------------------------------------------------------------------
-- RTSP formatter
-- ----------------------------------------------------------------------------

formatMessage :: Message -> Maybe B.ByteString -> B.ByteString
formatMessage (Request s v uri ver hs) _ = B.empty
formatMessage (Response sq status hs) body = 
  let responseLine = B.concat $ intersperse (Utf8.fromString " ") [
                                             Utf8.fromString "RTSP/1.0",
                                             (Utf8.fromString . show . fromStatus) status, 
                                             statusText status]
      headers = formatHeaders $ setSpecialHeaders sq body hs
      bodyBytes = maybe B.empty (id) body
      chunks = [responseLine, eol, headers, eol, eol, bodyBytes]
  in B.concat chunks

setSpecialHeaders :: Int -> Maybe B.ByteString -> Headers.Headers -> Headers.Headers
setSpecialHeaders sq body hs = 
  let hs' = Headers.set hdrSequence (show sq) hs
  in maybe hs' (\b -> Headers.set hdrContentLength (show $ B.length b) hs') body

formatHeaders :: Headers.Headers -> B.ByteString
formatHeaders hs = let fmt = \(h,v) -> Utf8.fromString (h ++ ": " ++ v)
                       hdrs = reverse $ Headers.fold (\hdr acc -> (fmt hdr) : acc) [] hs
                   in B.concat $ intersperse eol hdrs

formatPacket :: Packet -> B.ByteString
formatPacket (Packet channel bytes) = (B.concat . L.toChunks) $ runPut $ do 
  putWord8 (fromIntegral 0x24)
  putWord8 (fromIntegral channel)
  putWord16be (fromIntegral (B.length bytes))
  putByteString bytes

statusText :: Status -> Utf8.ByteString
statusText s = 
  case Map.lookup s statusDescriptions of
    Just t -> t
    Nothing -> (Utf8.fromString . show . fromStatus) s

-- ----------------------------------------------------------------------------
-- Static data
-- ----------------------------------------------------------------------------

endOfMessage :: B.ByteString 
endOfMessage = B.concat [eol, eol] 

eol :: B.ByteString 
eol = B.pack [13, 10]

hdrSequence = "CSeq"
hdrContentLength = "Content-Length"

statusDescriptions = Map.fromList $ map (\(s,t) -> (s, Utf8.fromString t)) [ 
                        (OK,                    "OK"),
                        (BadRequest,            "BadRequest"),
                        (AuthorizationRequired, "Authorization Required"),
                        (NotFound,              "Not Found"),
                        (SessionNotFound,       "Session Not Found"),
                        (InternalServerError,   "Internal Server Error"), 
                        (NotImplemented,        "Not Implemented"),
                        (BadGateway,            "Bad Gateway"),
                        (ServiceUnavailable,    "Service Unavailable"),
                        (GatewayTimeout,        "Gateway Timeout"),
                        (VersionNotSupported,   "RTSP Version Not Supported"),
                        (OptionNotSupported,    "Option Not Supported") ]
 
statusMap = Bimap.fromList [ (200, OK),
                             (400, BadRequest),
                             (401, AuthorizationRequired),
                             (404, NotFound),
                             (454, SessionNotFound),
                             (500, InternalServerError), 
                             (501, NotImplemented),
                             (502, BadGateway),
                             (503, ServiceUnavailable),
                             (504, GatewayTimeout),
                             (505, VersionNotSupported),
                             (551, OptionNotSupported) ]

-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------

testParseMinimalRequest = TestCase(do 
  let bytes = Utf8.fromString "DESCRIBE rtsp://localhost/root/1 RTSP/1.0\r\nCSeq: 1\r\n\r\n"
  let mmsg = parseMessage bytes
  assertBool "Parse Success" (isJust mmsg)
  let (Request sq verb uri ver hdr) = fromJust mmsg
  assertEqual "sequence" 1 sq
  assertEqual "verb" "DESCRIBE" verb
  assertEqual "version" (1,0) ver
  assertEqual "contentLength" 0 (maybe 0 (id) (contentLength hdr)))

testFormatMinimalResponse = TestCase(do
  let response = Response 42 NotImplemented Headers.empty
  let expected = (Utf8.fromString "RTSP/1.0 501 Not Implemented\r\nCSeq: 42\r\n\r\n")
  assertEqual "Formatted Message" expected (formatMessage response Nothing)) 

testFormatResponseWithBody = TestCase(do
  let response = Response 1702 OK Headers.empty
  let expected = (Utf8.fromString "RTSP/1.0 200 OK\r\nCSeq: 1702\r\nContent-Length: 12\r\n\r\nHello, World")
  assertEqual "Formatted Message" expected (formatMessage response (Just $ Utf8.fromString "Hello, World"))) 

testParseBadStatusResponse = TestCase(do 
  let bytes = Utf8.fromString "RTSP/1.0 000 Not a valid status\r\nCSeq: 1\r\n\r\n"
  let mmsg = parseMessage bytes
  assertBool "Parse successs" (isJust mmsg)
  let (Response sq status _) = fromJust mmsg
  assertEqual "Status" (Unknown 0) status)

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
  
testParseVerb s = fromRight $ parse verb "" (Utf8.fromString s) 
  
testVerbs = TestList [
  "Parse Describe" ~: Describe         ~=? testParseVerb "DESCRIBE",
  "Parse Announce" ~: Announce         ~=? testParseVerb "ANNOUNCE",
  "Parse Setup"    ~: Setup            ~=? testParseVerb "SETUP",
  "Parse Play"     ~: Play             ~=? testParseVerb "PLAY",
  "Parse Teardown" ~: Teardown         ~=? testParseVerb "TEARDOWN",
  "Other"          ~: OtherVerb "narf" ~=? testParseVerb "narf"]

testStatusToEnum = TestList [
  "OK"                ~: OK                  ~=? Rtsp.toStatus 200,
  "Bad Request"       ~: BadRequest          ~=? Rtsp.toStatus 400,
  "Not Found"         ~: NotFound            ~=? Rtsp.toStatus 404, 
  "Session Not Found" ~: SessionNotFound     ~=? Rtsp.toStatus 454, 
  "Error"             ~: InternalServerError ~=? Rtsp.toStatus 500,
  "Not Implemented"   ~: NotImplemented      ~=? Rtsp.toStatus 501,
  "Bad Gateway"       ~: BadGateway          ~=? Rtsp.toStatus 502,
  "Unavailable"       ~: ServiceUnavailable  ~=? Rtsp.toStatus 503,
  "Gateway Timeout"   ~: GatewayTimeout      ~=? Rtsp.toStatus 504,
  "Unsupported Ver"   ~: VersionNotSupported ~=? Rtsp.toStatus 505,
  "Unsupported Opt"   ~: OptionNotSupported  ~=? Rtsp.toStatus 551,
  "Unknown"           ~: Unknown 0           ~=? Rtsp.toStatus 0]

unitTests = TestList [TestLabel "Parse Minimal Request" testParseMinimalRequest,
                      TestLabel "Format Minimal Response" testFormatMinimalResponse,
                      TestLabel "Format Body Response" testFormatResponseWithBody,
                      TestLabel "Parse Request - Bad Status" testParseBadStatusResponse,
                      TestLabel "Packet Parsing - simple" testParsePacket,
                      TestLabel "Packet Parsing - insuffcient data" testParsePacketNotEnoughData,
                      testVerbs,
                      testStatusToEnum]
