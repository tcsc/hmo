module UnitTests where

import Test.HUnit  

import CaseInsensitiveString
import Rtsp
import RtspConnection

allTests = TestList [
  CaseInsensitiveString.unitTests,
  Rtsp.unitTests, 
  RtspConnection.unitTests]
