module UnitTests where

import Test.HUnit  

import CaseInsensitiveString
import Headers
import LuaUtils
import Multimap
import Rtsp
import RtspService
import Sdp
import SessionManager

allTests = TestList [
  CaseInsensitiveString.unitTests,
  Headers.unitTests,
  LuaUtils.unitTests,
  Multimap.unitTests,
  Rtsp.unitTests, 
  RtspService.unitTests,
  SessionManager.unitTests,
  Sdp.unitTests]
