{-# LANGUAGE FlexibleContexts #-}

module Authentication (
  AuthInfo,
  AuthContext,
  AuthFailure (..),
  AuthResult,
  AuthResultIO,
  parseAuthHeader,
  checkCreds,
  authUser,
  newAuthContext,
  refreshAuthContext,
  contextIsStale,
  genAuthHeaders,
) where
  
import Codec.Binary.Base64.String
import Control.Monad.Error
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Digest.OpenSSL.MD5
import qualified Data.Hex as Hex
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Utils
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Random
import Test.HUnit
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import CommonTypes
import Parsec

data AuthInfo = Basic String String
              | Digest [DigestElement]
              deriving (Show, Eq)

data QualityOfProtection = QopAuth
                         | QopAuthInt
                         | QopOther String
                         | QopNone
                         deriving (Show, Eq)

data DigestElement = DigRealm String
                   | DigNonce String
                   | DigUserName String
                   | DigClientNonce String
                   | DigResponse String
                   | DigURI String
                   | DigNonceCount Integer
                   | DigOpaque String 
                   | DigQop QualityOfProtection
                   | DigUnsupported String String
                   deriving (Show, Eq)
                         
data AuthFailure = UnsupportedMechanism
                 | ProtocolFailure
                 | Stale
                 | MalformedAuthRequest
                 | BadCredentials
                 | UnsupportedQoP
                 | MissingAuthorisation
                 | NoSuchUser
                 deriving (Read, Show, Eq)
                 
data AuthContext = AuthContext {
  ctxRealm    :: !String,
  ctxNonce    :: !String,
  ctxExpiry   :: POSIXTime,
  ctxLifespan :: !Int,
  ctxRng      :: !StdGen
}

instance Error AuthFailure where
  strMsg x = read x

type AuthResult = Either AuthFailure
type AuthResultIO = ErrorT AuthFailure IO

newAuthContext :: String -> Int -> IO AuthContext
newAuthContext realm lifeSpan = do 
  now <- getPOSIXTime 
  let rng = mkStdGen $ (truncate . (* 1000) . toRational) now
  refreshAuthContext $ AuthContext realm "" 0 lifeSpan rng

contextIsStale :: AuthContext -> IO Bool
contextIsStale ctx = do now <- getPOSIXTime
                        return $ now > (ctxExpiry ctx)

refreshAuthContext :: AuthContext -> IO AuthContext
refreshAuthContext context = do 
    now <- getPOSIXTime
    return $ refreshCtx now context 
  where
    refreshCtx now ctx = let rng = (ctxRng ctx)
                             lifespan = (ctxLifespan ctx)
                             (rng',nonce) = randomString rng 32
                             expiry = (now + fromIntegral lifespan)
                          in ctx { ctxNonce = nonce, ctxExpiry = expiry, ctxRng = rng' }

genAuthHeaders :: AuthContext -> (AuthContext, [String])
genAuthHeaders ctx = let (ctx', digest) = genDigestHeader ctx
                     in (ctx', [digest])
                                           
parseAuthHeader :: String -> Maybe AuthInfo
parseAuthHeader s = 
  let (name, payload) = cleave (== ' ') s
  in case (map toLower name) of
      "basic" -> parseBasic payload
      "digest" -> parseDigest payload
      _ -> Nothing
      
authUser :: AuthInfo -> String
authUser (Basic name _) = name
authUser (Digest es) = maybe "" (\(DigUserName n) -> n) $ find isDigestUser es  

-- | Check the credentials of 
checkCreds :: String -> AuthInfo -> UserInfo -> AuthContext -> AuthResult Bool
checkCreds _ (Basic _ pwd) uid _ = return (pwd == userPassword uid)
checkCreds method (Digest es) (User _ uid pwd) ctx = do
    uri      <- getElement isDigestURI unpackURI
    response <- getElement isDigestResponse unpackResponse
    nonce    <- getElement isDigestNonce unpackNonce
    cnonce   <- getElement isDigestCNonce unpackCNonce
    nc       <- getElement isDigestNonceCount unpackNonceCount
    qop      <- getQop
    
    let a1 = (hash . construct) [uid, (ctxRealm ctx), pwd]
    let a2 = (hash . construct) [method,uri]

    let expected = (hash . construct) $ 
          if qop == QopNone 
            then [a1, nonce, a2]
            else [a1, nonce, printf "%08x" nc, cnonce, fmtQop qop, a2]
                                                 
    if (response /= expected) then throwError BadCredentials
                              else if nonce /= (ctxNonce ctx)
                                then throwError Stale 
                                else return True
  where
    getQop :: AuthResult QualityOfProtection
    getQop = do 
      case find isDigestQop es of
        Nothing -> return QopNone
        Just (DigQop qop) -> case qop of 
                               QopOther _ -> throwError UnsupportedQoP 
                               q -> return q
                            
    getElement :: (DigestElement -> Bool) -> (DigestElement -> a) -> AuthResult a
    getElement idtor extractor = maybe (Left MalformedAuthRequest) (Right . extractor) $ find idtor es

    
parseBasic p =
  let (name,pwd) = (cleave (== ':') . decode) p
  in if (name == []) || (pwd == []) 
        then Nothing
        else Just $ Basic name pwd 

-- | generates an 'n' character long random string for use with the
--   authentication
randomString gen n = foldl' rchar (gen,[]) [0..(n-1)] 
  where rchar :: RandomGen g => (g,[Char]) -> a -> (g,[Char]) 
        rchar (g,cs) _ = let (c, g') = randomR ('a','z') g 
                         in (g', c : cs)

-- ----------------------------------------------------------------------------
-- Digest authentication
-- ----------------------------------------------------------------------------

genDigestHeader :: AuthContext -> (AuthContext, String)
genDigestHeader ctx = 
  let realm = ctxRealm ctx
      nonce = ctxNonce ctx
      rng = ctxRng ctx
      (rng',opaque) = randomString rng 64
      header = "Digest " ++
                "realm=\"" ++ realm ++ "\", " ++
                "nonce=\"" ++ nonce ++ "\", " ++
                "opaque=\"" ++ opaque ++ "\", " ++ 
                "qop=auth"
  in (ctx {ctxRng = rng'}, header)
                        

isDigestUser :: DigestElement -> Bool
isDigestUser (DigUserName _) = True
isDigestUser _ = False

isDigestResponse :: DigestElement -> Bool
isDigestResponse (DigResponse _) = True
isDigestResponse _ = False

isDigestURI :: DigestElement -> Bool
isDigestURI (DigURI _) = True
isDigestURI _ = False

isDigestQop :: DigestElement -> Bool
isDigestQop (DigQop _) = True
isDigestQop _ = False

isDigestNonce :: DigestElement -> Bool
isDigestNonce (DigNonce _) = True
isDigestNonce _ = False

isDigestCNonce :: DigestElement -> Bool
isDigestCNonce (DigClientNonce _) = True
isDigestCNonce _ = False

isDigestNonceCount :: DigestElement -> Bool
isDigestNonceCount (DigNonceCount _) = True
isDigestNonceCount _ = False


unpackURI (DigURI u) = u
unpackResponse (DigResponse r) = r
unpackNonce (DigNonce n) = n
unpackCNonce (DigClientNonce n) = n
unpackNonceCount (DigNonceCount nc) = nc

parseDigest p = case parse digest "" p of 
                  Right items -> Just $ Digest items
                  Left _ -> Nothing

digest = many $ do l <- digestElement
                   optional $ do {char ','; spaces; }
                   return l

digestElement = realm <|> nonce <|> cnonce <|> nonceCount <|> userName <|> 
                response <|> digUri <|> opaque <|> qop <|> unsupported
                
userName = stringValue "username" DigUserName
realm    = stringValue "realm" DigRealm
nonce    = stringValue "nonce" DigNonce
cnonce   = stringValue "cnonce" DigClientNonce
digUri   = stringValue "uri" DigURI
opaque   = stringValue "opaque" DigOpaque

unsupported = do n <- many $ satisfy (/= '=')
                 char '='
                 v <- quotedString <|> authToken
                 return $ DigUnsupported n v

nonceCount = do try $ string "nc"
                char '='
                n <- hexInteger
                return $ DigNonceCount n
                
response = do { try $ string "response";
                char '=';
                s <- optionallyQuoted $ many1 hexDigit;
                return $ DigResponse s; }

qop = do try $ string "qop"
         char '='
         s <- optionallyQuoted authToken
         let q = case map toLower s of 
                   "auth" -> QopAuth
                   "auth-int" -> QopAuthInt
                   _ -> QopOther s
         return $ DigQop $! q 
         
construct :: [String] -> String 
construct = intercalate ":"

hash :: String -> String
hash = (md5sum . B.pack)

hashConstruct :: [String] -> String 
hashConstruct =  hash . construct

-- ----------------------------------------------------------------------------
-- Utilities
-- ----------------------------------------------------------------------------

fmtQop :: QualityOfProtection -> String
fmtQop qop = case qop of
              QopAuth -> "auth"
              QopAuthInt -> "auth-int"
              QopOther s -> s

optionallyQuoted p = do q <- optionMaybe $ char '\"';
                        v <- p
                        if isNothing q then return () 
                                       else do { char '\"'; return (); };
                        return v  
  
stringValue name cons = do try $ string name
                           char '='
                           v <- quotedString
                           return $ cons v
          
quotedString = do char '\"'
                  s <- many $ satisfy (/= '\"')
                  char '\"'
                  return s
                  
authToken = many1 letter
                  
-- | Cuts an list in two on the first instance of a predicate returning true,
--   consuming the pivot element in the process.
cleave :: (a -> Bool) -> [a] -> ([a], [a])
cleave p s = let (x,xs) = break p s
             in (x,safeTail xs)
  where 
    safeTail :: [a] -> [a]
    safeTail (x:xs) = xs
    safeTail [] = []

-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------

digestTestHeader = "Digest username=\"Mufasa\", " ++
                    "realm=\"testrealm@host.com\", " ++
                    "nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", " ++
                    "uri=\"/dir/index.html\", " ++
                    "qop=auth, " ++
                    "nc=00000001, " ++
                    "cnonce=\"0a4f113b\", " ++
                    "response=\"6629fae49393a05397450978507c4ef1\", " ++
                    "opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""

digestTextExpected = Digest [ DigUserName "Mufasa", 
                              DigRealm "testrealm@host.com",
                              DigNonce "dcd98b7102dd2f0e8b11d0f600bfb0c093",
                              DigURI "/dir/index.html",
                              DigQop QopAuth,
                              DigNonceCount 1,
                              DigClientNonce "0a4f113b",
                              DigResponse "6629fae49393a05397450978507c4ef1",
                              DigOpaque "5ccc069c403ebaf9f0171e9517f40e41" ]

testAuthentication :: Assertion
testAuthentication = do
    let uid = User 0 "Mufasa" "narfity narf" -- "Circle Of Life"
    let realm = "testrealm@host.com"
    ctx <- makeContext realm
    assertEqual "Authentication Failed" (Right True) $ checkCreds "GET" digestTextExpected uid $! ctx
  where 
    makeContext r = do 
      ctx <- newAuthContext r 10
      return ctx { ctxNonce = "dcd98b7102dd2f0e8b11d0f600bfb0c093" } 

basicTests = TestList [
  "Basic Parsing" ~: Just (Basic "Aladdin" "open sesame") ~=? parseAuthHeader "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==",
  "Digest Parsing" ~: Just digestTextExpected ~=? parseAuthHeader digestTestHeader,
  TestCase testAuthentication]
  
unitTests = TestList [basicTests]