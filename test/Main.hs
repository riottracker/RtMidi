module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.List (find, isInfixOf)
import Data.Word (Word8)
import Sound.RtMidi (closeDevice, closePort, defaultInput, defaultOutput, findPort, sendMessage, setCallback, openPort, openVirtualPort)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

incIORef :: IORef Int -> IO ()
incIORef = flip modifyIORef succ

readerCallback :: IORef Int -> Double -> [Word8] -> IO ()
readerCallback countRef _ msg = incIORef countRef

testVirtualReadWrite :: TestTree
testVirtualReadWrite = testCase "virtual read write" $ do
  let expectedCount = 3
      -- a simple note-on message
      message = [0x90, 0x51, 0x7f]
      portName = "rtmidi-test-port"
      -- 0.01 second delay in us
      delayUs = 10000
  countRef <- newIORef 0
  -- Create reader with callback
  inDev <- defaultInput
  setCallback inDev (readerCallback countRef)
  openVirtualPort inDev portName
  -- Create writer and connect to reader virtual port
  outDev <- defaultOutput
  maybePortNum <- findPort outDev (isInfixOf portName)
  let portNum = maybe (error "Could not find port") id maybePortNum
  openPort outDev portNum portName
  -- Send messages
  replicateM_ expectedCount (sendMessage outDev message)
  -- Sleep a bit to ensure message delivery
  threadDelay delayUs
  -- Close writer
  closePort outDev
  closeDevice outDev
  -- Close reader
  closePort inDev
  closeDevice inDev
  -- Verify number of messages received
  actualCount <- readIORef countRef
  actualCount @?= expectedCount

main :: IO ()
main = defaultMain (testGroup "RtMidi" [testVirtualReadWrite])
