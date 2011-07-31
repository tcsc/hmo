module SessionDescription (
  SessionDescription (..),
  StreamDescription (..),
  RtpParams (..),
  MediaType (..),
  Protocol (..),
  FormatId,
  PortList,
  RtpMap,
  FmtMap,
  streamEncodings,
  showEncoding
) where

import qualified Data.Map as Map
import Data.Char
import Network.URI

data MediaType = Audio | Video | Text | Application | Message
                 deriving (Eq, Show)
                 
data Protocol = Udp | Rtp | SecureRtp | Other String
                deriving (Eq, Show)

type PortList = [Int]
type FormatId = Int
type RtpMap = [(Int, RtpParams)]
type FmtMap = [(Int, String)]

data RtpParams = RtpParams { 
    rtpFormat   :: !Int,
    rtpEncoding :: !String,
    rtpClock    :: !Integer,
    rtpParams   :: !String
 } deriving (Eq, Show)

data StreamDescription = StrD {
   streamType         :: !MediaType,
   streamPorts        :: ![Int],
   streamProtocol     :: !Protocol,
   streamFormatIds    :: ![FormatId],
   streamControlUri   :: !(Maybe String),
   streamParams       :: !RtpMap,
   streamFormatParams :: !FmtMap,
   streamAttributes   :: ![(String,String)]
 } deriving (Eq, Show)

-- | A parsed description of a media session, enumerating all of the streams
--   and media types that make up a combined media presentation.
data SessionDescription = SsnD {
    sessionName       :: !String,
    sessionInfo       :: !String,
    sessionUri        :: !(Maybe URI),
    sessionStreams    :: ![StreamDescription],
    sessionAttributes :: ![(String,String)]
  } deriving (Eq, Show)

streamEncodings :: StreamDescription -> [String]
streamEncodings stream = 
  let majorType = streamType stream
      params = streamParams stream
  in map (\(_,enc) -> showEncoding majorType enc) params

data TxMode = Unicast | Multicast
              deriving (Eq, Show)

data TransportMode = Play | Record
                     deriving (Eq, Show)

data TransportSpec = Transport {
    transMode     :: !TransportMode,
    transTxMode   :: !TxMode,
    transSrcPorts :: !PortList
  } deriving (Show)

------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------  

showEncoding :: MediaType -> RtpParams -> String     
showEncoding majorType params = let minorType = rtpEncoding params
                                in (map toLower (show majorType)) ++ "/" ++ minorType
