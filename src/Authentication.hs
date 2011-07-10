module Authentication (
  AuthInfo,
  parseAuthHeader,
  checkCreds,
  authUser,
) where

import Codec.Binary.Base64.String
import Control.Monad.Error
import Data.Char
import Test.HUnit  

import CommonTypes

data AuthInfo = Basic String String
{-              | Digest { 
                  digRealm :: !String,
                  digNonce :: !String,
                  digUid :: !String,
                  digCNonce :: !String,
                  digResponse :: !String,
                  digUri :: !String,
                  digNonceCount :: !String,
                  digQop :: !String,
                  digAuthZId :: !String,
                  digState :: !DigestState 
                } -}
                deriving (Show)
                         
data AuthFailure = UnsupportedMechanism
                 | ProtocolFailure
                 | Mismatch
                 deriving (Read, Show, Eq)

instance Error AuthFailure where
  strMsg x = read x

type AuthResult = Either AuthFailure

parseAuthHeader :: String -> Maybe AuthInfo
parseAuthHeader s = 
  let (name, payload) = split ' ' s
  in case (map toLower name) of
      "basic" -> parseBasic payload
      _ -> Nothing
      
authUser :: AuthInfo -> String
authUser (Basic name _) = name

checkCreds :: AuthInfo -> UserInfo -> Bool
checkCreds (Basic _ pwd) uid = (pwd == userPassword uid)

  
parseBasic p =
  let (name,pwd) = (split ':' . decode) p
  in if (name == []) || (pwd == []) 
        then Nothing
        else Just $ Basic name pwd
  
split :: (Eq a) => a -> [a] -> ([a],[a])
split c s = split' c s []
 where
    split' :: (Eq a) => a -> [a] -> [a] -> ([a],[a])
    split' _ [] acc = (acc, [])
    split' c (h:t) acc 
      | c == h    = (reverse acc, t)
      | otherwise = split' c (t) (h : acc) 

{-
newAuthInfo :: String -> String -> IO (Maybe AuthInfo)
newAuthInfo "DIGEST-MD5" realm = do
  g <- newStdGen 
  let n = hex . map chr . take 16 $ (randomRs (0,255) g :: [Int])
  return $ Just (newDigest realm n)
newAuthInfo _ _ = return Nothing

newDigest :: String -> String -> AuthInfo
newDigest realm nonce = Digest realm nonce "" "" "" "" "" "" "" Uninitialised

challenge :: AuthInfo -> String
challenge (Digest realm nonce _ _ _ _ _ _ _ _) =
  "realm=\"" ++ realm ++ "\"," ++
  "nonce=\"" ++ nonce ++ "\"," ++
  "qop=\"auth\"," ++
  "charset=\"utf-8\"," ++
  "algorithm=\"md5-sess\""
-}

-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------
{-
basicTests = TestList [
  "Basic Parsing" ~: Just (Basic "Aladdin" "open sesame") ~=? parseAuthHeader "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
]

unitTests = TestList [basicTests]

-}