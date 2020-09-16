module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.List (isInfixOf)
import Data.Word (Word8)
import Sound.RtMidi (Api (..), closePort, compiledApis, createInput, createOutput, currentApi, findPort, sendMessage, setCallback, openPort, openVirtualPort)
import Sound.RtMidi.Report (Report, buildReport)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

incIORef :: IORef Int -> IO ()
incIORef = flip modifyIORef succ

readerCallback :: IORef Int -> Double -> [Word8] -> IO ()
readerCallback countRef _ msg = incIORef countRef

testVirtualReadWrite :: Api -> TestTree
testVirtualReadWrite api = testCase ("virtual read write with " <> show api) $ do
  let expectedCount = 3
      -- a simple note-on message
      message = [0x90, 0x51, 0x7f]
      portName = "rtmidi-test-port"
      -- 100 ms delay in us
      delayUs = 100000
  countRef <- newIORef 0
  -- Create reader with callback
  inDev <- createInput api "rtmidi-test-input" 100
  inApi <- currentApi inDev
  inApi @?= api
  setCallback inDev (readerCallback countRef)
  openVirtualPort inDev portName
  -- Create writer and connect to reader virtual port
  outDev <- createOutput api "rtmidi-test-output"
  outApi <- currentApi outDev
  outApi @?= api
  maybePortNum <- findPort outDev (isInfixOf portName)
  let portNum = maybe (error "Could not find port") id maybePortNum
  openPort outDev portNum portName
  -- Send messages
  replicateM_ expectedCount (sendMessage outDev message)
  -- Sleep a bit to ensure message delivery
  threadDelay delayUs
  -- Close writer
  closePort outDev
  -- Close reader
  closePort inDev
  -- Verify number of messages received
  actualCount <- readIORef countRef
  actualCount @?= expectedCount

testReport :: TestTree
testReport = testCase "report" $ do
  report <- buildReport
  pure ()

main :: IO ()
main = do
  apis <- compiledApis
  let rwTests = fmap testVirtualReadWrite (filter (/= DummyApi) apis)
      rwGroup = testGroup "R/W" rwTests
  defaultMain (testGroup "RtMidi" [testReport, rwGroup])
