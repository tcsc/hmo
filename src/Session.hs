module Session(
  Session,
  MediaStream,
  newSession,
  deleteSession
) where 

import CommonTypes
import Service
import qualified Sdp as Sdp

data SsMsg = M
data SsRpy = R
data Session = Session (Service SsMsg SsRpy ())

data MediaStream = Str

newSession :: Sdp.Description -> UserId -> IO Session
newSession desc uid = do 
    svc <- newService setup handle teardown
    return $ Session svc
  where 
    setup svc = return ()
    teardown () = return ()
    handle msg state = return (R, state)


deleteSession :: Session -> IO ()
deleteSession (Session svc) = stopService svc