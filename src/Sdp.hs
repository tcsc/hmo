{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Sdp where 

import qualified Data.ByteString as B
import Data.Bits
import Data.Char
import Data.List
import Data.Either.Utils
import Data.Maybe
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

data SdpLine = Version Integer
             | Originator String Integer Integer Family Address
             | Name String
             | Info String
             | Uri URI
             | Email String
             | ConnectionData Family Address (Maybe Int) (Maybe Int)
             | Phone
             | Bandwidth BandwidthMode Int
             | Timing Integer Integer
             | RepeatTiming Integer Integer [Integer]
             | Media MediaType [Int] Protocol [Int]
             | RtpMap Int String Int String
             | FormatP Int String
             | Attribute String
             deriving(Show, Eq)
             
data SessionDescription = SD 
  
parseSdp :: B.ByteString -> Maybe SessionDescription
parseSdp s = Nothing

sdpLines = many sdpLine

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
  return $! Originator userName sessionId sessionVer family addr

name = do 
  char 's'
  char '='
  s <- many1 anyChar
  return $! Name s
  
info = do 
  char 'i'
  char '='
  s <- many1 anyChar
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
    return $! Media mt ports p fmt
  where 
    unpack p 0 = Nothing
    unpack p n = let n' = n-1 in Just (fromIntegral p + n', n')

attribute = do 
  char 'a'
  char '='
  s <- many1 (noneOf ":")
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
    return $ RtpMap (fromIntegral fmtId) encoding (fromIntegral clock) args
  where
    args = do 
      char '/'
      s <- many1 (noneOf "\r\n")
      return s 

formatp = do 
  fmtId <- integer
  space 
  s <- many1 (noneOf "\r\n")
  return $ FormatP (fromIntegral fmtId) s

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
rfcTests = TestList [
  "Version"     ~: (Version 0)                                                           ~=? testParseLine "v=0",
  "Originator"  ~: (Originator "jdoe" 2890844526 2890842807 AF_INET (Addr 0x0a2f1005))   ~=? testParseLine "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5",
  "Name"        ~: (Name "SDP Seminar")                                                  ~=? testParseLine "s=SDP Seminar",
  "Uri"         ~: (Uri (fromJust $ parseURI "http://www.example.com/seminars/sdp.pdf")) ~=? testParseLine "u=http://www.example.com/seminars/sdp.pdf",
  "Info"        ~: (Info "A Seminar on the session description protocol")                ~=? testParseLine "i=A Seminar on the session description protocol", 
  "Email"       ~: (Email "j.doe@example.com (Jane Doe)")                                ~=? testParseLine "e=j.doe@example.com (Jane Doe)",
  "Timeing"     ~: (Timing 2873397496 2873404696)                                        ~=? testParseLine "t=2873397496 2873404696",
  "Repeat"      ~: (RepeatTiming 604800 3600 [0,90000])                                  ~=? testParseLine "r=604800 3600 0 90000",
  "RepeatUnits" ~: (RepeatTiming 604800 3600 [0,90000])                                  ~=? testParseLine "r=7d 1h 0 25h",
  "Media"       ~: (Media Video [49170,49171] Rtp [31])                                  ~=? testParseLine "m=video 49170/2 RTP/AVP 31",
  "Media Map"   ~: (Media Audio [49230] Rtp [96,97,98])                                  ~=? testParseLine "m=audio 49230 RTP/AVP 96 97 98",
  "RtpMap"      ~: (RtpMap 99 "h263-1998" 90000 "")                                      ~=? testParseLine "a=rtpmap:99 h263-1998/90000",
  "FormatP"     ~: (FormatP 99 "dummy text")                                             ~=? testParseLine "a=fmtp:99 dummy text"
  ]

unitTests = TestList [rfcTests]
