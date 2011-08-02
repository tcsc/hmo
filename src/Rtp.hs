module Rtp where

import qualified Data.ByteString as B
import Data.Int
import Data.Word

data RtpPacket = RTP {
    rtpSeq       :: Word16,
    rtpTimestamp :: Word32,
    rtpSyncSrc   :: Word32,
    rtpPaymoad   :: B.ByteString
  }
  deriving (Show)

data RtcpPacket = SR
                | TR
                deriving (Show)
