{-# LANGUAGE ScopedTypeVariables  #-}

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
import Control.Exception(IOException, bracket, bracketOnError, try, catch, finally)
import Control.Monad.Error
import Network
import Network.BSD
import Network.Socket as Socket

import qualified Logger as Log
import ThreadManager

type ConnectionHandler = Socket -> IO ()
data State = State Socket ConnectionHandler

data ListenErr = BindFailed
               | UnexpectedFailure

instance Error ListenErr where
  noMsg = UnexpectedFailure
  strMsg s = UnexpectedFailure

-- | An exported handle to a listener
data TcpListener = MkListener (ThreadManager Socket ListenErr)

newListener :: IO (TcpListener)
newListener = do
    threadManager <- newThreadManager
    return $ MkListener threadManager

-- | Binds to a local address and starts a listener thread
bind :: TcpListener -> Family -> HostAddress -> PortNumber -> ConnectionHandler -> IO () 
bind (MkListener threadManager) family address port handler = do  
    spawnThread threadManager (initListener family address port) (listenThreadMain handler) cleanupListener Ignore
    return ()
  where
    initListener :: Family -> HostAddress -> PortNumber -> ErrorT ListenErr IO (Socket, ThreadSignal)
    initListener family address port = do
      rval <- liftIO $ do { socket <- createSocket family address port;
                            return (Right socket); } 
                       `catch` \(e :: IOException) -> return (Left BindFailed)
      case rval of 
        Right s -> return (s, signal s)
        Left _  -> throwError BindFailed

    listenThreadMain :: ConnectionHandler -> Socket -> IO ()
    listenThreadMain handler socket = do
        infoLog "Starting listen loop";
        loop socket handler
      where              
        loop ::  Socket -> ConnectionHandler -> IO ()
        loop socket handler = do (client, address) <- Socket.accept socket
                                 forkIO $ handler client
                                 loop socket handler
                              `catch` \(e :: IOException) -> do 
                                 debugLog "Exception raised during accept. Probably just the exit signal."
                                 return ()

    signal :: Socket -> IO ()
    signal s = do infoLog "Closing socket in response to shutdown signal"
                  sClose s

    cleanupListener :: Socket -> IO ()
    cleanupListener socket = do
      infoLog "Cleaning up TCP listener"
      sClose socket
      infoLog "Socket closed"

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
                 (\sock -> do infoLog $ "binding to port " ++ (show port)
                              bindSocket sock (SockAddrInet port addr)
                              infoLog $ "starting to listen on port " ++ (show port)
                              listen sock maxListenQueue
                              return sock)

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
                              
debugLog :: String -> IO ()
debugLog = Log.debug "  tcp"

infoLog :: String -> IO ()
infoLog = Log.info "  tcp"

errorLog :: String -> IO ()
errorLog = Log.err "  tcp"
