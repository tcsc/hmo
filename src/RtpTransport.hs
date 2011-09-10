module RtpTransport (
   TransportSpec (..),
   TransportParam (..),
   TransportParams,
   Protocol (..),
   PacketHandler (..),
   LowerTransport (..),
   TransportMode (..),
   Profile (..),
   RtpTransport (..),
   RtpHandler,
   RtcpHandler,
   isModeParameter,
   isInterleaved
  ) where

import Data.ByteString
import Rtp

data TransportMode = Receive | Record | Play deriving (Eq, Show)
data Protocol = RTP | SRTP deriving (Eq, Show)
data LowerTransport = UDP | TCP deriving (Eq, Show)
data Profile = AVP deriving (Eq, Show)

data TransportParam = Unicast
                    | Multicast
                    | Destination String
                    | Interleaved [Integer]
                    | Append
                    | TTL Integer
                    | Port [Integer]
                    | ClientPort [Integer] 
                    | ServerPort [Integer]
                    | SyncSource Integer
                    | Layers Integer
                    | Mode TransportMode 
                    deriving (Eq, Show)

type TransportParams = [TransportParam]

data TransportSpec = TransportSpec {
    transportProtocol       :: Protocol,
    transportProfile        :: Profile,
    transportLowerTransport :: LowerTransport,
    transportParams         :: TransportParams
  } deriving (Eq)

newtype PacketHandler = PacketHandler (ByteString -> IO())
type RtpHandler = PacketHandler
type RtcpHandler = PacketHandler

instance Show PacketHandler where
  show _ = "<packet handler>"

-- | A record that defines a communication protocol between an RTP sender or 
--   receiver and an underlying RTP transport mechanism.
data RtpTransport = RtpTransport {
    transportReg     :: (RtpHandler, RtcpHandler) -> IO (),
    transportTxRtp   :: RtpHandler,
    transportTxRtcp  :: RtcpHandler,
    transportDestroy :: IO (),
    transportGetSpec :: TransportSpec,
    transportDisplay :: String
  }

instance Show RtpTransport where
  show = transportDisplay

isModeParameter p = case p of (Mode _) -> True; _ -> False
isInterleaved p = case p of (Interleaved _) -> True; _ -> False