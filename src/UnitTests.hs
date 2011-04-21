module UnitTests where

import Test.HUnit  

import Rtsp
import RtspConnection

allTests = TestList [Rtsp.unitTests, RtspConnection.unitTests]