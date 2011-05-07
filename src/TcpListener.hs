-----------------------------------------------------------------------------
--
-- Module      :  TcpListener
-- Copyright   :  2011 Trent Clarke. All rights reserved.
-- License     :  AllRightsReserved
--
-- Maintainer  :  Trent Clarke
-- Stability   :
-- Portability :
--
-- | Implements a TCP listener based on the worker pool model
--
-----------------------------------------------------------------------------

module TcpListener (
    TcpListener,
    newListener,
    stopListener,
    bind
) where

import Prelude hiding (catch)
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Exception(bracket, bracketOnError, catch, finally)
import Network
import Network.BSD
import Network.Socket as Socket
import System.Log.Logger

import ThreadManager

type ConnectionHandler = Socket -> IO ()
data State = State Socket ConnectionHandler

-- | An exported handle to a listener
data TcpListener = MkListener ThreadManager

newListener :: IO (TcpListener)
newListener = do
    threadManager <- newThreadManager
    return $ MkListener threadManager

-- | Binds to a local address and starts a listener thread
bind :: TcpListener -> Family -> HostAddress -> PortNumber -> ConnectionHandler -> IO Socket
bind (MkListener threadManager) family address port handler = do 
    infoLog $ "binding to port " ++ (show port)
    s <- createSocket family address port
    spawnThread threadManager (listenThreadMain s handler) Ignore
    return s
  where
    listenThreadMain :: Socket -> ConnectionHandler -> IO ()
    listenThreadMain socket handler = do {
        infoLog "Starting listen loop";
        (loop socket handler) `finally` (cleanup socket);
      } `catch` \(e :: ExitGracefully) -> do { 
        infoLog "Caught exit signal";
        return () ;
      } 
              
    loop ::  Socket -> ConnectionHandler -> IO ()
    loop socket handler = do
      (client, address) <- Socket.accept socket
      forkIO $ handler client
      loop socket handler
      
    cleanup :: Socket -> IO ()
    cleanup socket = do {
      infoLog "Closing socket";
      sClose socket;
    }

stopListener :: TcpListener -> IO ()
stopListener (MkListener threadManager) = do
  infoLog "Stopping TCP listener"
  stopThreadManager threadManager
  infoLog "TCP listener stopped"
      
createSocket :: Family -> HostAddress -> PortNumber -> IO Socket
createSocket addrFamily addr port = do
  protocol <- getProtocolNumber "tcp"
  bracketOnError (socket addrFamily Stream protocol)
                 (sClose)
                 (\sock -> do bindSocket sock (SockAddrInet port addr)
                              listen sock maxListenQueue
                              return sock)
                              
debugLog :: String -> IO ()
debugLog = debugM "tcp"

infoLog :: String -> IO ()
infoLog = infoM "tcp"
