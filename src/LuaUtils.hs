module LuaUtils(
  LuaResult,
  LuaResultIO,
  LuaValue (..),
  LuaError(..),
  LuaTable,
  collectResults,
  mapTable, 
  bracketField, 
  bracketGlobal, 
  bracketStack,
  exec,
  loadFile,
  loadField,
  number,
  boolean,
  peekValue,
  pushValue,
  string,
  maybeString,
  tableField,
  unitTests) where

import Control.Exception
import Control.Monad.Error  
import Foreign.C
import Scripting.Lua
import qualified Data.Map as Map
import Test.HUnit

import System.Log.Logger

type LuaTable = Map.Map LuaValue LuaValue

data LuaValue = LString String
              | LInt LuaInteger
              | LNum LuaNumber
              | LTable LuaTable
              | LBool Bool
              | LNil
              | LUserData
              | LLightUserData
              | LFunction
              | LThread
  deriving(Show, Eq, Ord)

data LuaError = SyntaxError String
              | RuntimeError String
              | NotFound String
              | UntypedError String
              deriving(Eq, Show)
                 
instance Error LuaError where
   noMsg    = UntypedError "A script error"
   strMsg s = UntypedError s

type LuaResult a = Either LuaError a
type LuaResultIO a = ErrorT LuaError IO a

exec :: LuaState -> IO Int -> LuaResultIO ()
exec lua f = do 
  rval <- liftIO f
  case rval of 
    0 -> return ()
    _ -> getError lua RuntimeError

execChain :: LuaState -> IO Int -> LuaResultIO a -> LuaResultIO a
execChain lua f fnext = do 
  rval <- liftIO $ f
  case rval of 
    0 -> fnext
    _ -> getError lua RuntimeError

getError :: LuaState -> (String -> LuaError) -> LuaResultIO a
getError lua constructor = do 
  err <- liftIO $ peek lua (-1)
  let errText = case err of
                  Just s -> s
                  Nothing -> "Unexpected error"
  liftIO $ pop lua 1
  throwError $ constructor errText
    
-- | Loads a lua file into the supplied lua state, executes it andreports any 
--   syntax or execution errors
loadFile :: LuaState -> String -> LuaResultIO ()
loadFile lua fileName = do
  rl <- liftIO $ loadfile lua fileName
  case rl of 
    0 -> do 
      exec lua (call lua 0 0)
    _ -> getError lua SyntaxError
                                       
peekValue :: LuaState -> Int -> IO LuaValue 
peekValue lua idx = do
  t <- ltype lua idx 
  case t of 
    TBOOLEAN       -> toboolean lua idx >>= \v -> return $ LBool v 
    TSTRING        -> tostring lua idx  >>= \v -> return $ LString v	 
    TTABLE         -> readTable lua idx >>= \t -> return $ LTable t
    TNUMBER        -> tonumber lua idx  >>= \v -> return $ LNum v
    TNIL           -> return LNil
    TLIGHTUSERDATA -> return LLightUserData 
    TFUNCTION	     -> return LFunction
    TUSERDATA	     -> return LUserData 
    TTHREAD        -> return LThread
    
string :: LuaValue -> Maybe String
string (LString s) = Just s
string _ = Nothing

maybeString :: LuaValue -> Maybe String
maybeString LNil = Just []
maybeString (LString s) = Just s
maybeString _ = Nothing

number :: LuaValue -> Maybe LuaNumber
number (LNum n) = Just n
number _ = Nothing

boolean :: LuaValue -> Maybe Bool
boolean (LBool b) = Just b
boolean _ = Nothing

pushValue :: LuaState -> LuaValue -> IO ()
pushValue lua (LBool b)   = pushboolean lua b
pushValue lua (LString s) = pushstring lua s
pushValue lua (LNum n)    = pushnumber lua n
pushValue lua LNil        = pushnil lua
  
-- | Collects the results from an invocation 
collectResults :: LuaState -> Int -> IO [LuaValue]
collectResults lua count = do r <- collect lua count []
                              pop lua count
                              return r
  where 
    collect :: LuaState -> Int -> [LuaValue] -> IO [LuaValue]
    collect l i acc
      | i == 0    = return $ reverse acc
      | otherwise = do v <- peekValue l (-i)
                       collect l (i-1) (v : acc) 
  
-- | Reads a lua table, copying it into haskell-space as a map of LuaValues
readTable :: LuaState -> Int -> IO LuaTable 
readTable lua idx = do 
    let m = Map.empty
    isTable <- istable lua idx
    case isTable of
      False -> return m
      True -> do 
        let idx' = if idx < 0 then idx - 1 else idx
        pushnil lua
        traverse lua idx' m
  where 
    traverse :: LuaState -> Int -> LuaTable -> IO LuaTable
    traverse l i table = do
      x <- next l i -- leaves the key at -2 and the value at -1
      case x of 
        False -> return table
        True -> do k <- peekValue l (-2)
                   v <- peekValue l (-1)
                   pop lua 1
                   let table' = k `seq` v `seq` Map.insert k v table
                   traverse l i table'

tableField :: LuaValue -> LuaTable -> Maybe LuaValue
tableField = Map.lookup

-- | Maps the values of a Lua table into a list using the supplied function.
--   Assumes that the table of interest is on the top of the Lua Interface 
--   stack
mapTable :: StackValue v => LuaState -> (v -> b) -> IO [b]
mapTable lua f = do 
    isTable <- istable lua (-1)
    case isTable of
      False -> return []
      True -> do { pushnil lua; traverse lua f []; }
  where 
    -- Assumes that the table of interest is at the top of the stack
    traverse :: StackValue a => LuaState -> (a -> b) -> [b] -> IO [b]
    traverse l fn acc = do
      x <- next l (-2) -- leaves the key at -2 and the value at -1
      case x of 
        False -> return (reverse acc)
        True -> do mval <- peek l (-1) 
                   case mval of                         
                     Nothing -> return [] 
                     Just val -> do let acc' = (fn val) : acc
                                    pop l 1
                                    traverse l fn acc'

loadField :: LuaState -> Int -> String -> LuaResultIO ()
loadField lua index name = do 
  isNil <- liftIO $ do getfield lua index name
                       isnil lua (-1)
  if isNil then throwError (NotFound name)
           else return ()
  
-- | Pushes an item onto the stack, executes an action and pops the original
--   item back off the stack 
bracketStack :: LuaState -> (IO b) -> LuaResultIO a -> LuaResultIO a
bracketStack lua open fn = do 
  rval <- liftIO $ bracket_ (open) (pop lua 1) (runErrorT fn)
  case rval of
    Left e -> throwError e
    Right x -> return x
    
-- | Looks up a field in the (assumed) tabe on the top of the stack, executes 
--   an action and automatically pops the field back off the stack at the end.
bracketField :: LuaState -> Int -> String -> LuaResultIO a -> LuaResultIO a
bracketField lua index name action = 
  bracketStack lua (getfield lua index name) (do
    isNil <- liftIO $ isnil lua (-1)
    if isNil then throwError (NotFound name)
             else action)
                                         
bracketGlobal :: LuaState -> String -> LuaResultIO a -> LuaResultIO a
bracketGlobal lua name action = 
  bracketStack lua (getglobal lua name) (do
    isNil <- liftIO $ isnil lua (-1)
    if isNil then throwError (NotFound name)
             else action)


debugLog :: String -> IO ()
debugLog = debugM "lua"

infoLog :: String -> IO ()
infoLog = infoM "lua"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

testReadTable = TestCase(do
  bracket newstate close (\lua -> do
    loadstring lua "test = {\"alpha\", \"beta\", \"gamma\", subtable = {42, 1729}, \"delta\"}" ""
    call lua 0 0
    let expected = Map.fromList [ (LNum 1, LString "alpha"),
                                  (LNum 2, LString "beta"),
                                  (LNum 3, LString "gamma"),
                                  (LString "subtable", LTable $ 
                                    Map.fromList [(LNum 1, LNum 42), 
                                                  (LNum 2, LNum 1729)]),
                                  (LNum 4, LString "delta") ]
    getglobal lua "test"
    -- add some padding to make sure we're not just reading the top of the
    -- stack
    pushnil lua 
    value <- readTable lua (-2)
    assertEqual "map" expected value))

testPeekNotAtTopOfStack = TestCase(do
  bracket newstate close (\lua -> do
    pushstring lua "expected"
    pushstring lua "padding"
    value <- peekValue lua (-2)
    assertEqual "String" (LString "expected") value))

testPeekString = TestCase(do
  bracket newstate close (\lua -> do
    pushstring lua "testing"
    value <- peekValue lua (-1)
    assertEqual "String" (LString "testing") value))

testPeekBool = TestCase(do
  bracket newstate close (\lua -> do
    
    pushboolean lua False
    vF <- peekValue lua (-1)
    assertEqual "Bool: False" (LBool False) vF
    
    pushboolean lua True
    vT <- peekValue lua (-1)
    assertEqual "Bool: True" (LBool True) vT))


testPeekValue = TestList [ testPeekNotAtTopOfStack, testPeekString, testPeekBool ]

unitTests = TestList [testPeekValue, testReadTable]