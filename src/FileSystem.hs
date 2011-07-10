module FileSystem(
  FileSystem,
  FsResult,
  FsError(..),
  newFileSystem,
  register,
  unregister,
  stat,
  runFs
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad.Error
import Data.List
import qualified Data.Map as Map

type Fs a = Map.Map String a

type FsVar a = TVar (Fs a)

data FileSystem a = FS (FsVar a)

data FsError = AlreadyExists
             | NotFound
             | UnidentifiedError
             deriving (Show, Eq)

instance Error FsError where 
  strMsg x = UnidentifiedError
    
type FsResult a = ErrorT FsError STM a

runFs :: FsResult a -> IO (Either FsError a)
runFs = atomically . runErrorT

newFileSystem :: STM (FileSystem a)
newFileSystem = do
  fsVar <- newTVar $! Map.empty
  return (FS fsVar)
    
register :: [(String, a)] -> FileSystem a -> FsResult ()
register paths (FS fsVar) = do 
    fs <- lift $ readTVar fsVar
    fs' <- foldM reg fs paths
    lift $ writeTVar fsVar fs'
  where
    reg :: (Fs a) -> (String, a) -> FsResult (Fs a)
    reg fs (p,e) = do 
      if p `Map.member` fs
        then throwError AlreadyExists
        else return $! Map.insert p e fs
  
unregister :: [String] -> (FileSystem a) -> STM ()
unregister paths (FS fsVar) = do 
  fs <- readTVar fsVar
  writeTVar fsVar $ foldl' (\m s -> Map.delete s m) fs paths 

stat :: String -> (FileSystem a) -> FsResult a
stat s (FS fsVar) = do
  fs <- lift $ readTVar fsVar
  maybe (throwError NotFound) (return) $ Map.lookup s fs
  
