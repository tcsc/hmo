{-# LANGUAGE CPP #-}
module Signals where

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#else
import System.Posix.Signals
#endif

installInterruptHandler :: IO () -> IO ()

#if defined(mingw32_HOST_OS)
installInterruptHandler h = do  
    installHandler (Catch $ thunk h)
    return ()
  where thunk f e = case e of 
                     ControlC -> do f
                     _ -> return ()
#else
installInterruptHandler h = installHandler sigINT (Catch h) Nothing
#endif

