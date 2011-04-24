module UnitTests where

import Test.HUnit  

import CaseInsensitiveString
import Headers
import Multimap
import Rtsp
import RtspConnection
import SessionManager

allTests = TestList [
  CaseInsensitiveString.unitTests,
  Headers.unitTests,
  Multimap.unitTests,
  Rtsp.unitTests, 
  RtspConnection.unitTests,
  SessionManager.unitTests]
