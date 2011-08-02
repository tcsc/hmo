module RtpTransport where

import Rtp

type RtpHandler = (RtpPacket -> IO())
type RtcpHandler = [RtcpPacket] -> IO ()

-- | A record that defines a communication protocol between an RTP sender or 
--   receiver and an underlying RTP transport mechanism.
data RtpTransport = Transport {
    transportReg    :: (RtpHandler, RtcpHandler) -> IO (),
    transportTxRtp  :: RtpHandler,
    transportTxRtcp :: RtcpHandler,
    transportStop   :: IO ()
  }
