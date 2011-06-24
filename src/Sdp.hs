{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Sdp where 

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as Utf8
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Either.Utils
import Data.Maybe
import Network.URI
import Numeric
import Text.Parsec
import Text.Parsec.Char
import Test.HUnit
import Parsec

import Network.URI(parseURI)
import Network.Socket(Family(..), HostAddress, HostAddress6)

data Address = Addr HostAddress
             | Addr6 HostAddress6
             | HostName String
             deriving(Eq)
             
instance Show Address where
  show (Addr addr)     = "(Addr 0x" ++ ((showHex addr) ")")
  show (Addr6 addr)    = "(Addr6 " ++ (show addr) ++ ")"
  show (HostName addr) = "HostName " ++ addr

data MediaType = Audio | Video | Text | Application | Message
                 deriving(Eq, Show)

data Protocol = Udp | Rtp | SecureRtp | Other String
                deriving(Eq, Show)

data BandwidthMode = ConfidenceTotal | ApplicationSpecific | UnknownBw String
                     deriving (Eq, Show)
                     
--data Originator = Originator {                 
--                } deriving (Eq, Show)
                     
data RtpParams = RtpParams { 
                   rtpFormat   :: !Int,
                   rtpEncoding :: !String,
                   rtpClock    :: !Integer,
                   rtpParams   :: !String
                } deriving (Eq, Show)

data MediaStream = Stream {
                    streamType :: !MediaType,
                    streamPorts :: ![Int],
                    streamProtocol :: !Protocol,
                    streamFormats :: ![Int]
                  } deriving (Eq, Show)

data SdpLine = Version Integer
             | Orig String Integer Integer Family Address
             | Name String
             | Info String
             | Uri URI
             | Email String
             | ConnectionData Family Address (Maybe Int) (Maybe Int)
             | Phone
             | Bandwidth BandwidthMode Int
             | Timing Integer Integer
             | RepeatTiming Integer Integer [Integer]
             | Media MediaStream -- MediaType [Int] Protocol [Int]
             | RtpP RtpParams
             | FmtP Int String
             | Attribute String
             deriving (Show, Eq)

isMediaLine (Media _) = True
isMediaLine _ = False

type RtpMap = Map.Map Int RtpParams

type FmtMap = Map.Map Int String

-- | A parsed description of a media session, enumerating all of the streams
--   and media types that make up a combined media presentation.
data SessionDescription = SD {
      sessionName    :: !String,
      sessionInfo    :: !String,
      sessionUri     :: !(Maybe URI),
      sessionStreams :: [MediaStream],
      sessionRtpMap  :: !RtpMap,
      sessionFmtMap  :: !FmtMap
    } deriving (Eq, Show)
  
parseSdp :: B.ByteString -> Maybe SessionDescription
parseSdp s =
    case parse sdpLines "" s of
      Left _   -> Nothing
      Right ls -> let name    = extractName ls
                      info    = extractInfo ls
                      uri     = extractUri ls   
                      rtpMap  = collateRtpMap ls
                      fmtMap  = collateFmtMap ls
                      streams = collateStreams ls 
                  in Just $! SD name info uri streams rtpMap fmtMap
  where
    extractName :: [SdpLine] -> String
    extractName ls = maybe "" (\(Name s) -> s) $ find (\x -> case x of Name _ -> True; _ -> False) ls

    extractInfo :: [SdpLine] -> String
    extractInfo ls = maybe "" (\(Info s) -> s) $ find (\x -> case x of Info _ -> True; _ -> False) ls
        
    extractUri :: [SdpLine] -> Maybe URI
    extractUri ls = maybe (Nothing) (\(Uri u) -> Just u) $ find (\x -> case x of Uri _ -> True; _ -> False;) ls
      
    collateRtpMap :: [SdpLine] -> RtpMap
    collateRtpMap ls = 
      let select x = case x of RtpP _ -> True; _ -> False
          index (RtpP rtp) = let idx = rtpFormat rtp in (idx, rtp)
      in Map.fromList $ map index $ filter select ls

    collateFmtMap :: [SdpLine] -> FmtMap
    collateFmtMap ls = 
      let select x = case x of FmtP _ _ -> True; _ -> False
          index (FmtP idx s) = (idx, s)
      in Map.fromList $ map index $ filter select ls

    collateStreams :: [SdpLine] -> [MediaStream]
    collateStreams ls = 
      let select x = case x of Media _ -> True; _ -> False;
          unpack (Media s) = s
      in map unpack $ filter select ls

-- | finds and groups all of the sdp lines about streams  
extractStreams :: [SdpLine] -> [[SdpLine]]
extractStreams ls = extract ls []
  where
    extract :: [SdpLine] -> [[SdpLine]] -> [[SdpLine]]
    extract (h:t) acc
      | isMediaLine h = let s = h : (takeWhile (not . isMediaLine) t) 
                        in extract t (s : acc)
      | otherwise = extract t acc
    extract [] acc = reverse acc
      
sdpLines = many $ do { l <- sdpLine; many newline; return l }

sdpLine = version <|> originator <|> name <|> info <|> Sdp.uri <|> email <|> 
          connectionData <|> timing <|> repeatTiming <|> mediaDescription <|> 
          attribute

version = do 
  char 'v'
  char '='
  n <- integer
  return $! Version n
  
originator = do 
  char 'o'
  char '='
  userName   <- manyTill anyChar space
  sessionId  <- integer
  space
  sessionVer <- integer
  space
  string "IN"
  space
  family <- addressFamily
  space
  addr  <- address
  return $! Orig userName sessionId sessionVer family addr

name = do 
  char 's'
  char '='
  s <- many1 (noneOf "\r\n")
  return $! Name s
  
info = do 
  char 'i'
  char '='
  s <- many1 (noneOf "\r\n")
  return $! Info s
  
uri = do 
  char 'u'
  char '='
  u <- Parsec.uri
  return $! Uri u
  
email = do
  char 'e'
  char '='
  s <- many1 (noneOf "\r\n")
  return $! Email s
  
connectionData = do 
  char 'c'
  char '='
  string "IN"
  space
  f <- addressFamily
  space 
  a <- address
  ttl <- addressParam <|> return Nothing
  count <- addressParam <|> return Nothing
  return $! ConnectionData f a ttl count
  
timing = do
  char 't'
  char '='
  start <- integer
  space
  stop <- integer
  return $! Timing start stop
  
repeatTiming = do
  char 'r'
  char '='
  interval <- timeSpan
  space 
  duration <- timeSpan
  space 
  offsets <- many1 $ do { d <- timeSpan; many space; return d }
  return $ RepeatTiming interval duration offsets

timeSpan = do
  i <- integer
  scale <- (char 'd' >> return 86400) <|> 
           (char 'h' >> return 3600) <|> 
           (char 'm' >> return 60) <|> 
           (char 's' >> return 1) <|> 
           (eof >> return 1) <|> 
           (lookAhead space >> return 1) <?> "timespan unit"
  return $! (i * scale)
  
bandwidth = do
  s <- many1 (noneOf ":")
  char ':'
  i <- integer
  let mode = case s of
              "CT" -> ConfidenceTotal
              "AS" -> ApplicationSpecific
              _ -> UnknownBw s
  return $! Bandwidth mode (fromIntegral i) 
  
mediaDescription = do 
    char 'm'
    char '='
    mt <- mediaType
    space
    port <- integer
    c <- addressParam <|> return Nothing
    let ports = reverse $ unfoldr (unpack port) $ maybe 1 (fromIntegral) c
    space
    p <- protocol
    space 
    fmt <- many1 $ do x <- integer
                      many space
                      return (fromIntegral x)
    return $! Media $ Stream mt ports p fmt
  where 
    unpack p 0 = Nothing
    unpack p n = let n' = n-1 in Just (fromIntegral p + n', n')

attribute = do 
  char 'a'
  char '='
  s <- many1 (noneOf ":\r\n")
  many (char ':')
  case s of 
    "rtpmap" -> rtpMap
    "fmtp" -> formatp
    _ -> return $! Attribute s

rtpMap = do
    fmtId <- integer
    space
    encoding <- many1 (noneOf "/ \t")
    char '/'
    clock <- integer
    args <- args <|> return ""
    return $ RtpP $ RtpParams (fromIntegral fmtId) encoding (fromIntegral clock) args
  where
    args = do 
      char '/'
      s <- many1 (noneOf "\r\n")
      return s 

formatp = do 
  fmtId <- integer
  space 
  s <- many1 (noneOf "\r\n")
  return $ FmtP (fromIntegral fmtId) s

mediaType = do 
  s <- many1 (noneOf " \t")
  case (map toLower s) of
    "video"       -> return Video
    "audio"       -> return Audio
    "text"        -> return Text
    "application" -> return Application
    "message"     -> return Message
    _ -> fail $ "Invalid media type: " ++ s

protocol = do 
  s <- many1 (noneOf " \t")
  case (map toLower s) of
    "udp"      -> return Udp
    "rtp/avp"  -> return Rtp
    "rtp/savp" -> return SecureRtp
    _          -> return $ Other s
  
addressParam = do 
  char '/'
  n <- integer
  return $ Just (fromIntegral n) 
  
addressFamily = do
  s <- many1 (noneOf " \t\r\n")
  case s of 
    "IP4" -> return AF_INET
    "IP6" -> return AF_INET6
    _ -> fail "Address Type"

address = ip4Addr <|> ip6Addr <|> hostName

ip4Addr = do
  a <- integer
  char '.'
  b <- integer
  char '.'
  c <- integer
  char '.'
  d <- integer
  return $ Addr $ ((fromIntegral a) `shiftL` 24) .|. 
                  ((fromIntegral b) `shiftL` 16) .|. 
                  ((fromIntegral c) `shiftL` 8) .|. 
                   (fromIntegral d)

ip6Addr = fail "6"

hostName = do
  s <- manyTill anyChar newline 
  return $ HostName s 

testParseLine :: String -> SdpLine
testParseLine = fromRight . parse sdpLine ""

-- Test parsing the basic lines as described in the SDP RFC, using the examples
-- provided there
rfcLineTests = TestList [
  "Version"     ~: (Version 0)                                                           ~=? testParseLine "v=0",
  "Originator"  ~: (Orig "jdoe" 2890844526 2890842807 AF_INET (Addr 0x0a2f1005))         ~=? testParseLine "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5",
  "Name"        ~: (Name "SDP Seminar")                                                  ~=? testParseLine "s=SDP Seminar",
  "Uri"         ~: (Uri (fromJust $ parseURI "http://www.example.com/seminars/sdp.pdf")) ~=? testParseLine "u=http://www.example.com/seminars/sdp.pdf",
  "Info"        ~: (Info "A Seminar on the session description protocol")                ~=? testParseLine "i=A Seminar on the session description protocol", 
  "Email"       ~: (Email "j.doe@example.com (Jane Doe)")                                ~=? testParseLine "e=j.doe@example.com (Jane Doe)",
  "Connection"  ~: (ConnectionData AF_INET (Addr 0xe002110c) (Just 127) Nothing)         ~=? testParseLine "c=IN IP4 224.2.17.12/127",
  "Timing"      ~: (Timing 2873397496 2873404696)                                        ~=? testParseLine "t=2873397496 2873404696",
  "Repeat"      ~: (RepeatTiming 604800 3600 [0,90000])                                  ~=? testParseLine "r=604800 3600 0 90000",
  "RepeatUnits" ~: (RepeatTiming 604800 3600 [0,90000])                                  ~=? testParseLine "r=7d 1h 0 25h",
  "Media"       ~: (Media (Stream Video [49170,49171] Rtp [31]))                         ~=? testParseLine "m=video 49170/2 RTP/AVP 31",
  "Media Map"   ~: (Media (Stream Audio [49230] Rtp [96,97,98]))                         ~=? testParseLine "m=audio 49230 RTP/AVP 96 97 98",
  "RtpMap"      ~: (RtpP (RtpParams 99 "h263-1998" 90000 ""))                            ~=? testParseLine "a=rtpmap:99 h263-1998/90000",
  "FormatP"     ~: (FmtP 99 "dummy text")                                                ~=? testParseLine "a=fmtp:99 dummy text"
  ]
  
rfcSession = TestCase $ do 
  let text = "v=0\n"                                              ++ 
             "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\n"   ++
             "s=SDP Seminar\n"                                    ++ 
             "i=A Seminar on the session description protocol\n"  ++
             "u=http://www.example.com/seminars/sdp.pdf\n"        ++
             "e=j.doe@example.com (Jane Doe)\n"                   ++
             "c=IN IP4 224.2.17.12/127\n"                         ++
             "t=2873397496 2873404696\n"                          ++
             "a=recvonly\n"                                       ++
             "m=audio 49170 RTP/AVP 0\n"                          ++ 
             "m=video 51372/2 RTP/AVP 99\n"                       ++
             "a=rtpmap:99 h263-1998/90000"                        ++
             "a=fmtp:99 1234567890ABCDEF"

  let expected = SD "SDP Seminar"
                    "A Seminar on the session description protocol"
                    (parseURI "http://www.example.com/seminars/sdp.pdf")
                    [Stream Audio [49170] Rtp [0], Stream Video [51372,51373] Rtp [99]]
                    (Map.fromList [(99, RtpParams 99 "h263-1998" 90000 "")])
                    (Map.fromList [(99, "1234567890ABCDEF")])
                     
  assertEqual "Session Description" expected $ fromJust (parseSdp $ Utf8.fromString text) 
{-
  let expected = [Version 0,
                  Originator "jdoe" 2890844526 2890842807 AF_INET (Addr 0x0a2f1005),
                  Name "SDP Seminar",
                  Info "A Seminar on the session description protocol",
                  Uri (fromJust $ parseURI "http://www.example.com/seminars/sdp.pdf"),
                  Email "j.doe@example.com (Jane Doe)",
                  ConnectionData AF_INET (Addr 0xe002110c) (Just 127) Nothing,
                  Timing 2873397496 2873404696,
                  Attribute "recvonly",
                  Media (Stream Audio [49170] Rtp [0]),
                  Media (Stream Video [51372,51373] Rtp [99]),
                  RtpP (RtpParams 99 "h263-1998" 90000 ""),
                  FmtP 99 "1234567890ABCDEF"] 
  let ls = fromRight $ parse sdpLines "" text
  assertEqual "Session Description" expected ls
-}

unitTests = TestList [rfcLineTests, rfcSession]
