module WorkerTypes (
  ReplyVar (..),
  WkrMsg (..),
  WkrQ,
  MessageReply (..)
) where 

import Control.Concurrent.STM

-- | Defines an "operation complete" signal as well as providing a mechanism
--   where a message handler can return a result to the original caler
type ReplyVar reply = TMVar reply

-- | Defines a set of messages that the worker threads can process, including
--   a conduit for the threads' actal IPC messages
data WkrMsg msg reply = ExitWorker
                      | Handle msg
                      | HandleAndReply msg (ReplyVar reply)

-- | 
type WkrQ msg reply = TChan (WkrMsg msg reply)

-- | A synonym for Maybe used for communicating responses from a message handler
data MessageReply a = NoReply
                    | Reply a

instance Functor MessageReply where 
  fmap _ NoReply   = NoReply
  fmap f (Reply a) = Reply (f a)

instance Monad MessageReply where
  (Reply x) >>= k = k x
  NoReply >>= _ = NoReply

  (Reply _) >> k = k
  NoReply >> _ = NoReply

  return = Reply   

-- | Defines a function that sets up a worker thread. Use this to initialise any
--   thread - specific data (e.g. database handles, etc) if necessary
type WorkerSetup state = IO state

-- | Defines a function for tearing down resources created during thread startup
type WorkerTeardown state = state -> IO ()

-- | Defines a function type that the worker pool uses to handle messages
type MessageHandler msg   -- ^ The range of possible messages
                    reply -- ^ The range of possible replies
                    state -- ^ An opaque per-thread state block for the owner
                    = msg -> state -> IO (MessageReply reply, state)
