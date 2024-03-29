module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, unless, when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Sound.RtMidi (Api (..), apiDisplayName, apiName, closePort, compiledApiByName, compiledApis, createInput, createOutput, currentApi, findPort, openPort, openVirtualPort, sendMessage, setCallback)
import Sound.RtMidi.Report (Report, buildReport)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

incIORef :: IORef Int -> IO ()
incIORef = flip modifyIORef succ

-- A simple note-on message
exampleMessage :: VS.Vector Word8
exampleMessage = VS.fromList [0x90, 0x51, 0x7f]

readerCallback :: IORef Int -> Double -> VS.Vector Word8 -> IO ()
readerCallback countRef _ msg = do
  msg @?= exampleMessage
  incIORef countRef

testApiName :: TestTree
testApiName = testCase "apiName" $ do
  actualName1 <- apiName UnspecifiedApi
  actualName1 @?= "unspecified"
  actualName2 <- apiName DummyApi
  actualName2 @?= "dummy"

testApiDisplayName :: TestTree
testApiDisplayName = testCase "apiDisplayName" $ do
  actualName1 <- apiDisplayName UnspecifiedApi
  actualName1 @?= "Unknown"
  actualName2 <- apiDisplayName DummyApi
  actualName2 @?= "Dummy"

testCompiledApiByName :: TestTree
testCompiledApiByName = testCase "compiledApiByName" $ do
  actualApi1 <- compiledApiByName "unspecified"
  actualApi1 @?= UnspecifiedApi
  actualApi2 <- compiledApiByName "dummy"
  actualApi2 @?= UnspecifiedApi
  actualApi3 <- compiledApiByName "invalid"
  actualApi3 @?= UnspecifiedApi

testBuildReport :: TestTree
testBuildReport = testCase "buildReport" $ do
  -- We could test more but we mostly just want to make sure we can run 'buildReport'.
  _ <- buildReport
  pure ()

testVirtualReadWrite :: Api -> TestTree
testVirtualReadWrite api = testCase ("virtual read write with " <> show api) $ do
  let expectedCount = 3
      portName = "rtmidi-test-port"
      -- 100 ms delay in us
      delayUs = 100000
  -- First a check of api name
  name <- apiName api
  actualApi <- compiledApiByName name
  actualApi @?= api
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
  let portNum = fromMaybe (error "Could not find port") maybePortNum
  openPort outDev portNum portName
  -- Send messages
  replicateM_ expectedCount (sendMessage outDev exampleMessage)
  -- Sleep a bit to ensure message delivery
  threadDelay delayUs
  -- Close writer
  closePort outDev
  -- Close reader
  closePort inDev
  -- Verify number of messages received
  actualCount <- readIORef countRef
  actualCount @?= expectedCount

main :: IO ()
main = do
  apis <- compiledApis
  let rwTests = fmap testVirtualReadWrite (filter (/= DummyApi) apis)
      rwGroup = testGroup "R/W" rwTests
  defaultMain $
    testGroup
      "RtMidi"
      [ testApiName
      , testApiDisplayName
      , testCompiledApiByName
      , testBuildReport
      , rwGroup
      ]
