module Authentication where
  
{-
data AuthenticationInfo = Digest { digRealm :: !String,
                                   digNonce :: !String,
                                   digUid :: !String,
                                   digCNonce :: !String,
                                   digResponse :: !String,
                                   digUri :: !String,
                                   digNonceCount :: !String,
                                   digQop :: !String,
                                   digAuthZId :: !String,
                                   digState :: !DigestState 
                                   }
                          | Basic String
                         deriving (Show)
                         
data AuthenticationFailure = UnsupportedMechanism
                           | ProtocolFailure
                           | Mismatch
                           deriving (Read, Show, Eq)

instance Error AuthFailure where
  strMsg x = read x

type AuthenticationResult = Either AuthenticationFailure

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