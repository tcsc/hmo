{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

-- ----------------------------------------------------------------------------
-- |
-- Module: Sdp
-- Author: Trent Clarke
-- 
-- Implements an SDP parser as per RFC 4566. Well, maybe not all of it - but
-- enough to be going on with for now.
-- ----------------------------------------------------------------------------

module Sdp (
  Sdp.parse,
  unitTests
) where 

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
import Network.URI(parseURI)
import Network.Socket(Family(..), HostAddress, HostAddress6)

import Parsec
import CommonTypes
import SessionDescription


data Address = Addr HostAddress
             | Addr6 HostAddress6
             | HostName String
             deriving(Eq)
             
instance Show Address where
  show (Addr addr)     = "(Addr 0x" ++ ((showHex addr) ")")
  show (Addr6 addr)    = "(Addr6 " ++ (show addr) ++ ")"
  show (HostName addr) = "HostName " ++ addr

data BandwidthMode = ConfidenceTotal | ApplicationSpecific | UnknownBw String
                     deriving (Eq, Show)
              
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
             | Media MediaType PortList Protocol [FormatId]
             | RtpP FormatId String Integer String
             | FmtP FormatId String
             | Attribute String String
             | ControlUri String
             deriving (Show, Eq)

-- | Parses an SDP session description into a SessionDescription record,
--   returning Nothing if the parse fails.
parse :: B.ByteString -> Maybe SessionDescription
parse s =
    case Text.Parsec.parse sdpLines "" s of
      Left _   -> Nothing
      Right ls -> let name    = extractName ls
                      info    = extractInfo ls
                      uri     = extractUri ls   
                      rtpMap  = collateRtpMap ls
                      fmtMap  = collateFmtMap ls
                      streams = collateStreams ls rtpMap fmtMap
                  in Just $! SsnD name info uri streams []
  where
    extractName :: [SdpLine] -> String
    extractName ls = maybe "" (\(Name s) -> s) $ find (\x -> case x of Name _ -> True; _ -> False) ls

    extractInfo :: [SdpLine] -> String
    extractInfo ls = maybe "" (\(Info s) -> s) $ find (\x -> case x of Info _ -> True; _ -> False) ls
        
    extractUri :: [SdpLine] -> Maybe URI
    extractUri ls = maybe (Nothing) (\(Uri u) -> Just u) $ find (\x -> case x of Uri _ -> True; _ -> False;) ls
      
    collateRtpMap :: [SdpLine] -> [(FormatId, RtpParams)]
    collateRtpMap ls = 
      let select x = case x of RtpP _ _ _ _ -> True; _ -> False
          index (RtpP idx enc clock params) = (idx, RtpParams idx enc clock params)
      in avpRtpParams ++ (map index $ filter select ls)

    collateFmtMap :: [SdpLine] -> FmtMap
    collateFmtMap ls = 
      let select x = case x of FmtP _ _ -> True; _ -> False
          index (FmtP idx s) = (idx, s)
      in map index $ filter select ls

    collateStreams :: [SdpLine] -> RtpMap -> FmtMap -> [StreamDescription]
    collateStreams ls rtpMap fmtMap = let sls = extractStreamLines ls 
                                      in catMaybes $ map (extractStream rtpMap fmtMap) sls
                    
-- | Finds and groups all of the sdp lines about individual media streams  
extractStreamLines :: [SdpLine] -> [[SdpLine]]
extractStreamLines ls = extract ls []
  where
    extract :: [SdpLine] -> [[SdpLine]] -> [[SdpLine]]
    extract (h:t) acc
      | isMediaLine h = let s = h : (takeWhile (not . isMediaLine) t) 
                        in extract t (s : acc)
      | otherwise = extract t acc
    extract [] acc = reverse acc

-- | Creates a StreamDescription record out of a set of SDP lines.  
extractStream :: RtpMap -> FmtMap -> [SdpLine] -> Maybe StreamDescription
extractStream rtpMap fmtMap ls = 
  let controlUri = getControlUri ls
      attribs = getAttribs ls
  in do
    (mType, ports, protocol, formatIds) <- getMediaStream ls
    return $ StrD { streamFormatIds    = formatIds,
                    streamType         = mType,
                    streamPorts        = ports,
                    streamProtocol     = protocol,
                    streamControlUri   = controlUri,
                    streamParams       = rtpParams formatIds,
                    streamAttributes   = attribs,
                    streamFormatParams = formatParams formatIds }
  where
    getMediaStream :: [SdpLine] -> Maybe (MediaType, PortList, Protocol, [FormatId])
    getMediaStream ls = do 
      (Media mType ports protocol formats) <- find isMediaLine ls
      return (mType, ports, protocol, formats)
    
    getControlUri :: [SdpLine] -> Maybe String
    getControlUri ls = do 
      (ControlUri u) <- find isControlUri ls
      return u
      
    -- | Extracts all the RTP parameters releveant to this stream from the
    --   supplied RTP map
    rtpParams :: [FormatId] -> RtpMap
    rtpParams = catMaybes . (map (index rtpMap))
    
    formatParams :: [FormatId] -> FmtMap
    formatParams = catMaybes . (map (index fmtMap))
      
    index :: [(FormatId, a)] -> FormatId -> Maybe (FormatId, a)
    index as i = maybe Nothing (\a -> Just (i,a)) $ lookup i as
      
    getAttribs :: [SdpLine] -> [(String, String)]
    getAttribs = (catMaybes . (map extractAttribs))
             
-- ----------------------------------------------------------------------------
-- Line recognizers
-- ----------------------------------------------------------------------------

extractAttribs l = case l of Attribute n v -> Just (n,v); _ -> Nothing;
isControlUri l   = case l of ControlUri _ -> True; _ -> False;
isMediaLine l    = case l of Media _ _ _ _ -> True; _ -> False;
isAttribute l    = case l of Attribute _ _ -> True; _ -> False;
isRtpP l         = case l of RtpP _ _ _ _ -> True; _ -> False;
isFormatP l      = case l of FmtP _ _ -> True; _ -> False;

-- ----------------------------------------------------------------------------
-- Static data
-- ----------------------------------------------------------------------------
                 
-- | The list of RTP/AVP formats defined in the AVP RFC
avpRtpParams :: [(FormatId, RtpParams)]
avpRtpParams = [ 
    ( 0, RtpParams  0 "PCMU"  8000 ""),
    ( 3, RtpParams  3 "GSM"   8000 ""),
    ( 4, RtpParams  4 "G723"  8000 ""),
    ( 5, RtpParams  5 "DVI4"  8000 ""),
    ( 6, RtpParams  6 "DVI4" 16000 ""), 
    ( 7, RtpParams  7 "LPC"   8000 ""),
    ( 8, RtpParams  8 "PCMA"  8000 ""),
    ( 9, RtpParams  9 "G722"  8000 ""),
    (10, RtpParams 10 "L16"  44100 ""),
    (11, RtpParams 11 "L16"  44100 "2")
  ]
    
-- ----------------------------------------------------------------------------
-- Low-level parsers for SDP lines
-- ----------------------------------------------------------------------------
    
sdpLines = many $ do { l <- sdpLine; many endOfLine; return l }

sdpLine = version <|> originator <|> name <|> info <|> Sdp.uri <|> email <|> 
          connectionData <|> timing <|> repeatTiming <|> mediaDescription <|> 
          attribute <|> bandwidth

version = do 
  char 'v'
  char '='
  n <- decimalInteger
  return $! Version n
  
originator = do 
  char 'o'
  char '='
  userName   <- manyTill anyChar space
  sessionId  <- decimalInteger
  space
  sessionVer <- decimalInteger
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
  start <- decimalInteger
  space
  stop <- decimalInteger
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
  i <- decimalInteger
  scale <- (char 'd' >> return 86400) <|> 
           (char 'h' >> return 3600) <|> 
           (char 'm' >> return 60) <|> 
           (char 's' >> return 1) <|> 
           (eof >> return 1) <|> 
           (lookAhead space >> return 1) <?> "timespan unit"
  return $! (i * scale)
  
bandwidth = do
  char 'b'
  char '='
  s <- many1 (noneOf ":")
  char ':'
  i <- decimalInteger
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
    port <- decimalInteger
    c <- addressParam <|> return Nothing
    let ports = reverse $ unfoldr (unpack port) $ maybe 1 (fromIntegral) c
    space
    p <- protocol
    space 
    fmt <- many1 $ do x <- decimalInteger
                      many space
                      return (fromIntegral x)
    return $! Media mt ports p fmt
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
    "control" -> many1 (noneOf "\r\n") >>= \u -> return $ ControlUri u
    _ -> do v <- many (noneOf ":\r\n")
            return $! Attribute s v

rtpMap = do
    fmtId <- decimalInteger
    space
    encoding <- many1 (noneOf "/ \t")
    char '/'
    clock <- decimalInteger
    args <- args <|> return ""
    return $ RtpP (fromIntegral fmtId) encoding (fromIntegral clock) args
  where
    args = do 
      char '/'
      s <- many1 (noneOf "\r\n")
      return s 

formatp = do 
  fmtId <- decimalInteger
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
  n <- decimalInteger
  return $ Just (fromIntegral n) 
  
addressFamily = do
  s <- many1 (noneOf " \t\r\n")
  case s of 
    "IP4" -> return AF_INET
    "IP6" -> return AF_INET6
    _ -> fail "Address Type"

endOfLine = many1 (oneOf "\r\n")

address = ip4Addr <|> ip6Addr <|> hostName

ip4Addr = do
  a <- decimalInteger
  char '.'
  b <- decimalInteger
  char '.'
  c <- decimalInteger
  char '.'
  d <- decimalInteger
  return $ Addr $ ((fromIntegral a) `shiftL` 24) .|. 
                  ((fromIntegral b) `shiftL` 16) .|. 
                  ((fromIntegral c) `shiftL` 8) .|. 
                   (fromIntegral d)

ip6Addr = fail "6"

hostName = do
  s <- manyTill anyChar newline 
  return $ HostName s 

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

-- | The main collection of unit tests for this module
unitTests = TestList [rfcLineTests, rfcSession]

-- | Helper function for unit testing individual line parsers
testParseLine :: String -> SdpLine
testParseLine = fromRight . Text.Parsec.parse sdpLine ""


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
  "RepeatUnits" ~: (RepeatTiming 604800 20 [600,90000])                                  ~=? testParseLine "r=7d 20s 10m 25h",
  "Media"       ~: (Media Video [49170,49171] Rtp [31])                                  ~=? testParseLine "m=video 49170/2 RTP/AVP 31",
  "Media Map"   ~: (Media Audio [49230] Rtp [96])                                        ~=? testParseLine "m=audio 49230 RTP/AVP 96",
  "RtpMap"      ~: (RtpP 99 "h263-1998" 90000 "")                                        ~=? testParseLine "a=rtpmap:99 h263-1998/90000",
  "FormatP"     ~: (FmtP 99 "dummy text")                                                ~=? testParseLine "a=fmtp:99 dummy text",
  "ControlURI"  ~: (ControlUri "streamId=2")                                             ~=? testParseLine "a=control:streamId=2",
  "Attribute"   ~: (Attribute "name" "value")                                            ~=? testParseLine "a=name:value",
  "AttributeNV" ~: (Attribute "attribute" "")                                            ~=? testParseLine "a=attribute"
  ]
  
-- | A test case based (loosely) on the examples presented in RFC 4566
rfcSession = TestCase $
  let text = "v=0\r\n"                                              ++ 
             "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\r\n"   ++
             "s=SDP Seminar\r\n"                                    ++ 
             "i=A Seminar on the session description protocol\r\n"  ++
             "u=http://www.example.com/seminars/sdp.pdf\r\n"        ++
             "e=j.doe@example.com (Jane Doe)\r\n"                   ++
             "c=IN IP4 224.2.17.12/127\r\n"                         ++
             "t=2873397496 2873404696\r\n"                          ++
             "a=recvonly\r\n"                                       ++
             "m=audio 49170 RTP/AVP 0\r\n"                          ++ 
             "m=video 51372/2 RTP/AVP 97 98 99\r\n"                 ++
             "a=rtpmap:99 h263-1998/90000\r\n"                      ++
             "a=fmtp:99 1234567890ABCDEF\r\n"                       ++
             "a=control:streamid=1\r\n"                             ++
             "a=name:value"
      audioStream = StrD { 
                      streamType         = Audio,
                      streamPorts        = [49170],
                      streamProtocol     = Rtp,
                      streamFormatIds    = [0],
                      streamControlUri   = Nothing,
                      streamParams       = [(0, RtpParams 0 "PCMU" 8000 "")],
                      streamFormatParams = [],
                      streamAttributes   = [] }
      videoStream = StrD { 
                      streamType         = Video,
                      streamPorts        = [51372,51373],
                      streamProtocol     = Rtp,
                      streamFormatIds    = [97, 98, 99],
                      streamControlUri   = Just "streamid=1",
                      streamParams       = [(99, RtpParams 99 "h263-1998" 90000 "")],
                      streamFormatParams = [(99, "1234567890ABCDEF")],
                      streamAttributes   = [("name","value")] }
      expected = SsnD "SDP Seminar"
                      "A Seminar on the session description protocol"
                      (parseURI "http://www.example.com/seminars/sdp.pdf")
                      [audioStream, videoStream]
                      []
  in assertEqual "Session Description" expected $ fromJust (Sdp.parse $ Utf8.fromString text) 
