-- | Interface to RtMidi
module Sound.RtMidi
  ( InputDevice
  , OutputDevice
  , IsDevice (getDeviceType)
  , DeviceType (..)
  , Api (..)
  , Error (..)
  , ready
  , compiledApis
  , openPort
  , openVirtualPort
  , closePort
  , portCount
  , portName
  , defaultInput
  , createInput
  , setCallback
  , cancelCallback
  , ignoreTypes
  , getMessage
  , getMessageSized
  , defaultOutput
  , createOutput
  , sendMessage
  , closeDevice
  , currentApi
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, Storable (..), alloca, allocaArray, nullPtr, peekArray, with, withArrayLen)
import Foreign.C (CDouble (..), CInt (..), CSize, CString, CUChar (..), peekCString, withCString)
import Sound.RtMidi.Foreign

-- The default message size (in bytes) expected from 'getMessage'
-- Mostly just needs to be bigger than the max MIDI message size, which is 3 bytes
-- However, sysex messages can be quite large, so you might have to use the 'getMessageSized' variant.
defaultMessageSize :: Int
defaultMessageSize = 4

-- | Allows us to discriminate in/out functions in generic contexts
data DeviceType =
    InputDeviceType
  | OutputDeviceType
  deriving (Eq, Show)

newtype Device = Device { unDevice :: Ptr Wrapper }
  deriving (Eq, Show)

-- | Generalizes 'InputDevice' and 'OutputDevice' for use in common functions
class IsDevice d where
  toDevice :: d -> Device
  getDeviceType :: d -> DeviceType

toDevicePtr :: IsDevice d => d -> Ptr Wrapper
toDevicePtr = unDevice . toDevice

-- | A handle to a device to be used for input
newtype InputDevice = InputDevice { unInputDevice :: Device }
  deriving (Eq, Show)

instance IsDevice InputDevice where
  toDevice = unInputDevice
  getDeviceType _ = InputDeviceType

-- | A handle to a device to be used for input
newtype OutputDevice = OutputDevice { unOutputDevice :: Device }
  deriving (Eq, Show)

instance IsDevice OutputDevice where
  toDevice = unOutputDevice
  getDeviceType _ = OutputDeviceType

-- | Enum of RtMidi-supported APIs
data Api
  = UnspecifiedApi
  | CoreMidiApi
  | AlsaApi
  | JackApi
  | MultimediaApi
  | KernelStreamingApi
  | DummyApi
  deriving (Eq, Show)

instance Enum Api where
  fromEnum UnspecifiedApi = 0
  fromEnum CoreMidiApi = 1
  fromEnum AlsaApi = 2
  fromEnum JackApi = 3
  fromEnum MultimediaApi = 4
  fromEnum DummyApi = 5
  toEnum 0 = UnspecifiedApi
  toEnum 1 = CoreMidiApi
  toEnum 2 = AlsaApi
  toEnum 3 = JackApi
  toEnum 4 = MultimediaApi
  toEnum 5 = DummyApi

-- | Check if a device is ok
ready :: IsDevice d => d -> IO Bool
ready = fmap ok . peek  . toDevicePtr

-- | An internal RtMidi error
newtype Error = Error String deriving (Eq, Show)
instance Exception Error

-- Detects and throws internal errors
guardError :: Ptr Wrapper -> IO ()
guardError dptr = do
  w <- peek dptr
  unless (ok w) $ do
    e <- peekCString (msg w)
    throwIO (Error e)

-- | A static function to determine MIDI 'Api's built in.
compiledApis :: IO [Api]
compiledApis = do
  n <- fmap fromIntegral (rtmidi_get_compiled_api nullPtr)
  as <- allocaArray n $ flip with $ \ptr -> do
    rtmidi_get_compiled_api ptr
    x <- peek ptr
    peekArray n x
  pure (map (toEnum . fromEnum) as)

-- | Open a MIDI connection
openPort :: IsDevice d
         => d
         -> Int          -- ^ port number
         -> String       -- ^ name for the application port that is used
         -> IO ()
openPort d n name = do
  let dptr = toDevicePtr d
  withCString name (rtmidi_open_port dptr (toEnum n))
  guardError dptr

-- | This function creates a virtual MIDI output port to which other software applications can connect.
--
-- This type of functionality is currently only supported by the Macintosh OS X, Linux ALSA and JACK APIs
-- (the function does nothing with the other APIs).
openVirtualPort :: IsDevice d => d -> String -> IO ()
openVirtualPort d name = do
  let dptr = toDevicePtr d
  withCString name (rtmidi_open_virtual_port dptr)
  guardError dptr

-- | Close an open MIDI connection (if one exists).
closePort :: IsDevice d => d -> IO ()
closePort d = do
  let dptr = toDevicePtr d
  rtmidi_close_port dptr
  guardError dptr

-- | Return the number of MIDI ports available to the 'Device'.
portCount :: IsDevice d => d -> IO Int
portCount d = do
  let dptr = toDevicePtr d
  x <- rtmidi_get_port_count dptr
  guardError dptr
  pure (fromIntegral x)

-- | Return a string identifier for the specified MIDI port number.
--
-- An empty string is returned if an invalid port specifier is provided.
portName :: IsDevice d => d -> Int -> IO String
portName d n = do
  let dptr = toDevicePtr d
  x <- rtmidi_get_port_name dptr (toEnum n)
  guardError dptr
  peekCString x

-- | Default constructor for a 'Device' to use for input.
defaultInput :: IO InputDevice
defaultInput = do
  dptr <- rtmidi_in_create_default
  guardError dptr
  pure (InputDevice (Device dptr))

-- | Create a new 'Device' to use for input.
createInput :: Api        -- ^ API to use
            -> String     -- ^ client name
            -> Int        -- ^ size of the MIDI input queue
            -> IO InputDevice
createInput api clientName queueSizeLimit = do
  dptr <- withCString clientName (\str -> rtmidi_in_create (toEnum $ fromEnum api) str (toEnum queueSizeLimit))
  guardError dptr
  pure (InputDevice (Device dptr))

foreign import ccall "wrapper"
  wrap :: (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()) -> IO (FunPtr (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()))

proxy :: (Double -> [Word8] -> IO ()) -> (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ())
proxy f (CDouble t) d s _ = peekArray (fromIntegral s) d >>= \a -> f t (coerce a)

-- | Set a callback function to be invoked for incoming MIDI messages.
--
-- The callback function will be called whenever an incoming MIDI message is received.
-- While not absolutely necessary, it is best to set the callback function before opening a MIDI port to avoid leaving
-- some messages in the queue.
setCallback :: InputDevice
            -> (Double -> [Word8] -> IO ())  -- ^ Function that takes a timestamp and a MIDI message as arguments
            -> IO ()
setCallback d c = do
  let dptr = toDevicePtr d
  f <- wrap (proxy c)
  rtmidi_in_set_callback dptr f nullPtr
  guardError dptr

-- | Cancel use of the current callback function (if one exists).
--
-- Subsequent incoming MIDI messages will be written to the queue and can be retrieved with the `getMessage` function.
cancelCallback :: InputDevice -> IO ()
cancelCallback d = do
  let dptr = toDevicePtr d
  rtmidi_in_cancel_callback dptr
  guardError dptr

-- | Specify whether certain MIDI message types should be queued or ignored during input.
--
-- By default, MIDI timing and active sensing messages are ignored during message input because of their
-- relative high data rates. MIDI sysex messages are ignored by default as well.
-- Variable values of `true` imply that the respective message type will be ignored.
ignoreTypes :: InputDevice
            -> Bool       -- ^ SysEx messages
            -> Bool       -- ^ Time messages
            -> Bool       -- ^ Sense messages
            -> IO ()
ignoreTypes = rtmidi_in_ignore_types . toDevicePtr

-- | Variant of 'getMessage' that allows you to set message buffer size (typically for large sysex messages).
getMessageSized :: InputDevice -> Int -> IO (Double, [Word8])
getMessageSized d n = alloca $ \s -> allocaArray n $ flip with $ \m -> do
  let dptr = toDevicePtr d
  poke s (fromIntegral n)
  CDouble timestamp <- rtmidi_in_get_message dptr m s
  guardError dptr
  size <- peek s
  message <-
    case size of
      0 -> pure []
      _ -> do
        x <- peek m
        y <- peekArray (fromIntegral size) x
        pure (coerce y)
  pure (timestamp, message)

-- | Return data bytes for the next available MIDI message in the input queue and the event delta-time in seconds.
--
-- This function returns immediately whether a new message is available or not.
-- A valid message is indicated by whether the list contains any elements.
-- Note that large sysex messages will be silently dropped! Use 'getMessageSized' or use a callback to get these safely.
getMessage :: InputDevice -> IO (Double, [Word8])
getMessage d = getMessageSized d defaultMessageSize

-- | Default constructor for a 'Device' to use for output.
defaultOutput :: IO OutputDevice
defaultOutput = do
  dptr <- rtmidi_out_create_default
  guardError dptr
  pure (OutputDevice (Device dptr))

-- | Create a new 'Device' to use for output.
createOutput :: Api        -- ^ API to use
             -> String     -- ^ client name
             -> IO OutputDevice
createOutput api clientName = do
  dptr <- withCString clientName (rtmidi_out_create (toEnum (fromEnum api)))
  guardError dptr
  pure (OutputDevice (Device dptr))

-- | Immediately send a single message out an open MIDI output port.
sendMessage :: OutputDevice -> [Word8] -> IO ()
sendMessage d m = withArrayLen m $ \n ptr -> do
  let dptr = toDevicePtr d
  rtmidi_out_send_message dptr (coerce ptr) (fromIntegral n)
  guardError dptr

-- | If a MIDI connection is still open, it will be closed
closeDevice :: IsDevice d => d -> IO ()
closeDevice d =
  let dptr = toDevicePtr d
  in case getDeviceType d of
    InputDeviceType -> rtmidi_in_free dptr
    OutputDeviceType -> rtmidi_out_free dptr

-- | Returns the specifier for the MIDI 'Api' in use
currentApi :: IsDevice d => d -> IO Api
currentApi d = do
  let dptr = toDevicePtr d
  res <-
    case getDeviceType d of
      InputDeviceType -> rtmidi_in_get_current_api dptr
      OutputDeviceType -> rtmidi_out_get_current_api dptr
  guardError dptr
  pure (toEnum (fromEnum res))
