{-# LANGUAGE NoMonomorphismRestriction #-}

module Rtsp (
  Message,
  MessageHeader(..),
  MessageBody,
  SequenceNumber,
  Packet (..),
  Status (..),
  Verb (..),
  Version,
  embeddedPacket,
  extractMessageBytes,
  parseMessage,
  parseTransport,
  formatMessage,
  formatPacket,
  hdrContentType,
  hdrTransport,
  hdrPublic,
  hdrSession,
  hdrAuthenticate,
  msgSequence,
  msgHeaders,
  msgGetHeaderValue,
  msgSetHeaders,
  msgSequenceNumber,
  msgContentLength,
  reqURI,
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
import SessionDescription
import RtpTransport 

data Status = OK
            | BadRequest
            | AuthorizationRequired
            | NotFound
            | SessionNotFound
            | UnsupportedTransport
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

data MessageHeader = Request SequenceNumber Method URI Version Headers.Headers
                   | Response SequenceNumber Status Headers.Headers

type MessageBody = Maybe B.ByteString

type Message = (MessageHeader, MessageBody)
             
data Packet = Packet Int B.ByteString deriving (Show)
  
instance Show MessageHeader where 
  show (Request s v u _ hs) = "#" ++ (show s) ++ " " ++ (show v) ++ " " ++ (show u) 
                                -- ++ "\n" 
                                -- ++ (intercalate "\n" $ Headers.map (\(n,v) -> n ++ ": " ++ v) hs) 
  show (Response _ s _) = (show s) 

-- | gets the sequence number of a message
msgSequence :: MessageHeader -> Int
msgSequence (Request s _ _ _ _ ) = s 
msgSequence (Response s _ _) = s 

msgHeaders :: MessageHeader -> Headers.Headers
msgHeaders (Request _ _ _ _ hs) = hs 
msgHeaders (Response _ _ hs) = hs 

msgGetHeaderValue :: MessageHeader -> String -> Maybe String
msgGetHeaderValue msg n = Headers.get n $ msgHeaders msg

msgSetHeaders :: MessageHeader -> [(String, String)] -> MessageHeader
msgSetHeaders (Response cseq stat hs) values = 
  let hs' = foldl' (\h (n, v) -> Headers.set n v h) hs values
  in Response cseq stat hs'

msgSequenceNumber :: MessageHeader -> SequenceNumber
msgSequenceNumber (Request cseq _ _ _ _) = cseq
msgSequenceNumber (Response cseq _ _) = cseq

msgContentLength :: MessageHeader -> Int
msgContentLength (Request _ _ _ _ hs) = maybe 0 fromIntegral (contentLength hs)
msgContentLength (Response _ _ _) = 0 

-- | Fills in the missing bits of a half-parsed message once we know what they are
msgUpdate :: Int -> Headers.Headers -> MessageHeader -> MessageHeader
msgUpdate s headers (Request _ verb uri ver _) = Request s verb uri ver headers
msgUpdate s headers (Response _ status _) = Response s status headers  

reqURI :: MessageHeader -> URI
reqURI (Request _ _ uri _ _) = uri

-- | 
parseMessage :: B.ByteString -> Maybe MessageHeader
parseMessage bytes = case parse message "" bytes of
                       Left _ -> Nothing
                       Right msg -> Just msg

hdrContentType = "Content-Type" :: String
hdrTransport = "Transport" :: String

hdrPublic :: String
hdrPublic = "Public"

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
               "ANNOUNCE" -> Rtsp.Announce
               "DESCRIBE" -> Rtsp.Describe
               "SETUP"    -> Rtsp.Setup
               "PLAY"     -> Rtsp.Play
               "TEARDOWN" -> Rtsp.Teardown
               s          -> Rtsp.OtherVerb text
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

parseTransport :: String -> Maybe TransportSpec
parseTransport s = case parse transport "" s of
                     Left _  -> Nothing
                     Right t -> return t 

formatTransport :: TransportSpec -> String
formatTransport t = 
  let params = transportParams t
      proto = formatProto t 
  in concat $ intersperse ";" (proto : map formatParam params)
  where
    formatProto :: TransportSpec -> String
    formatProto (TransportSpec RTP AVP UDP _)  = "RTP/AVP/UDP"
    formatProto (TransportSpec RTP AVP TCP _)  = "RTP/AVP/TCP"
    formatProto (TransportSpec SRTP AVP UDP _) = "SRTP/AVP/UDP"
    formatProto (TransportSpec SRTP AVP TCP _) = "SRTP/AVP/TCP"
    
    formatParam :: TransportParam -> String
    formatParam Multicast        = "multicast"
    formatParam Unicast          = "unicast"
    formatParam (Destination s)  = "destination=" ++ s
    formatParam (Interleaved cs) = "interleaved=" ++ (formatPortList cs)
    formatParam Append           = "append"
    formatParam (TTL n)          = "ttl=" ++ (show n)
    formatParam (Port ps)        = "port=" ++ (formatPortList ps)
    formatParam (ClientPort ps)  = "client_port=" ++ (formatPortList ps) 
    formatParam (ServerPort ps)  = "server_port=" ++ (formatPortList ps) 
    formatParam (SyncSource n)   = "ssrc="
    formatParam (Layers n)       = "layers=" ++ (show n)
    formatParam (Mode m)         = let s = case m of 
                                            RtpTransport.Play -> "PLAY"; 
                                            RtpTransport.Record -> "RECORD";
                                            RtpTransport.Receive -> "RECEIVE"
                                   in "mode=\"" ++ s ++ "\""
                                
    formatPortList :: [Integer] -> String
    formatPortList = ((concat . intersperse "-") . map show) 

instance Show TransportSpec where 
  show = formatTransport

transport = do
    proto <- transportProtocol
    char '/'
    string "AVP"
    lowerTx <- lowerTransport <|> return UDP
    char ';'
    parameters <- transportParameters
    return $! TransportSpec {
      transportProtocol       = proto,
      transportProfile        = AVP,
      transportLowerTransport = lowerTx,
      transportParams         = parameters }
  where
    lowerTransport = do char '/'
                        s <- many1 (noneOf ";")
                        case (map toLower s) of
                          "tcp" -> return TCP
                          "udp" -> return UDP
                          _ -> fail "expected TCP or UDP"
                          
    transportProtocol = do s <- many1 (noneOf "/")
                           case (map toLower s) of
                             "rtp"  -> return $ RTP
                             "srtp" -> return $ SRTP
                             _      -> fail "expected RTP or SRTP"

transportParameters = param `sepBy` (char ';')
 where param = unicast <|> multicast 
               <|> destination
               <|> interleaved
               <|> append
               <|> ttl
               <|> layers
               <|> port 
               <|> clientPorts 
               <|> serverPorts
               <|> syncSource
               <|> mode
       unicast     = simpleValue "unicast" Unicast
       multicast   = simpleValue "multicast" Multicast
       destination = namedValue "destination" (many $ noneOf ";") Destination
       interleaved = namedValue "interleaved" portList Interleaved
       append      = simpleValue "append" Append
       ttl         = namedValue "ttl" decimalInteger TTL
       layers      = namedValue "layers" decimalInteger Layers
       port        = namedValue "port" portList Port
       clientPorts = namedValue "client_port" portList ClientPort
       serverPorts = namedValue "server_port" portList ServerPort
       syncSource  = namedValue "ssrc" hexInteger SyncSource
       mode        = namedValue "mode" transportMode Mode
        
       portList = decimalInteger `sepBy` (char '-')
        
       transportMode = optionallyQuoted (play <|> record <|> receive)
         where play    = literal "play" RtpTransport.Play
               record  = literal "record" Record
               receive = literal "receive" Receive
        
       literal s ctor = do { caseInsensitiveString s; return ctor; }
        
       simpleValue name ctor = do { try (string name); return ctor; }
        
       namedValue name p ctor = do { try (string name);
                                     char '=';
                                     value <- p;
                                     return $ ctor value; } 
                         

-- ----------------------------------------------------------------------------
-- RTSP formatter
-- ----------------------------------------------------------------------------

formatMessage :: MessageHeader -> Maybe B.ByteString -> B.ByteString
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
                        (UnsupportedTransport,  "Unsupported Transport"),
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
                             (461, UnsupportedTransport),
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


testParseTransport = TestList [unicastRecord, interleavedRecord, interleavedPlay, interleavedUnspec]
  where 
    test name text expected = TestCase (
      do case parseTransport text of 
          Nothing -> assertFailure (name ++ ": parse failed on \"" ++ text ++ "\"")
          Just t -> assertEqual name expected t)
          
    unspecifiedLower = 
          let text = "RTP/AVP;unicast;client_port=6970-6971;mode=record"
              expected = TransportSpec RTP AVP UDP [Unicast, ClientPort [6970, 6971], Mode Record]
          in test "Parse Unicast Transport" text expected
        
    unicastRecord = 
      let text = "RTP/AVP;unicast;client_port=6970-6971;mode=record"
          expected = TransportSpec RTP AVP UDP [Unicast, ClientPort [6970, 6971], Mode Record]
      in test "Parse Unicast Transport" text expected

    interleavedUnspec = 
      let text = "RTP/AVP/TCP;interleaved=0-1"
          expected = TransportSpec RTP AVP TCP [Interleaved [0, 1]]
      in test "Parse Interleaved Transport" text expected
      
    interleavedRecord = 
      let text = "RTP/AVP/TCP;interleaved=0-1;mode=record"
          expected = TransportSpec RTP AVP TCP [Interleaved [0, 1], Mode Record]
      in test "Parse Interleaved Transport (Record)" text expected
      
    interleavedPlay = 
         let text = "RTP/AVP/TCP;interleaved=0-1;mode=play"
             expected = TransportSpec RTP AVP TCP [Interleaved [0, 1], Mode RtpTransport.Play]
         in test "Parse Interleaved Transport (Play)" text expected

testFormatTransport = TestList [interleaved]
  where interleaved = TestCase (
          let name = "Format Interleaved Transport"
              transport = TransportSpec RTP AVP TCP [Interleaved [0, 1]]
              expected = "RTP/AVP/TCP;interleaved=0-1"
          in assertEqual name expected $ show transport)

testVerbs = 
  let testParseVerb s = fromRight $ parse verb "" (Utf8.fromString s) 
  in TestList [
    "Parse Describe" ~: Rtsp.Describe         ~=? testParseVerb "DESCRIBE",
    "Parse Announce" ~: Rtsp.Announce         ~=? testParseVerb "ANNOUNCE",
    "Parse Setup"    ~: Rtsp.Setup            ~=? testParseVerb "SETUP",
    "Parse Play"     ~: Rtsp.Play             ~=? testParseVerb "PLAY",
    "Parse Teardown" ~: Rtsp.Teardown         ~=? testParseVerb "TEARDOWN",
    "Other"          ~: Rtsp.OtherVerb "narf" ~=? testParseVerb "narf"]

testStatusToEnum = TestList [
  "OK"                    ~: OK                   ~=? Rtsp.toStatus 200,
  "Bad Request"           ~: BadRequest           ~=? Rtsp.toStatus 400,
  "Not Found"             ~: NotFound             ~=? Rtsp.toStatus 404, 
  "Session Not Found"     ~: SessionNotFound      ~=? Rtsp.toStatus 454, 
  "Unsupported Transport" ~: UnsupportedTransport ~=? Rtsp.toStatus 461, 
  "Error"                 ~: InternalServerError  ~=? Rtsp.toStatus 500,
  "Not Implemented"       ~: NotImplemented       ~=? Rtsp.toStatus 501,
  "Bad Gateway"           ~: BadGateway           ~=? Rtsp.toStatus 502,
  "Unavailable"           ~: ServiceUnavailable   ~=? Rtsp.toStatus 503,
  "Gateway Timeout"       ~: GatewayTimeout       ~=? Rtsp.toStatus 504,
  "Unsupported Ver"       ~: VersionNotSupported  ~=? Rtsp.toStatus 505,
  "Unsupported Opt"       ~: OptionNotSupported   ~=? Rtsp.toStatus 551,
  "Unknown"               ~: Unknown 0            ~=? Rtsp.toStatus 0]

unitTests = TestList [
  TestLabel "Parse Minimal Request" testParseMinimalRequest,
  TestLabel "Format Minimal Response" testFormatMinimalResponse,
  TestLabel "Format Body Response" testFormatResponseWithBody,
  TestLabel "Parse Request - Bad Status" testParseBadStatusResponse,
  TestLabel "Packet Parsing - simple" testParsePacket,
  TestLabel "Packet Parsing - insuffcient data" testParsePacketNotEnoughData,
  testVerbs,
  testStatusToEnum,
  testParseTransport,
  testFormatTransport ]
