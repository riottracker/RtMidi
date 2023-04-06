{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Interface to RtMidi
module Sound.RtMidi
  ( InputDevice
  , OutputDevice
  , IsDevice (getDeviceType)
  , DeviceType (..)
  , Api (..)
  , Error (..)
  , apiName
  , apiDisplayName
  , compiledApiByName
  , ready
  , compiledApis
  , openPort
  , openVirtualPort
  , closePort
  , portCount
  , portName
  , listPorts
  , findPort
  , defaultInput
  , createInput
  , setCallback
  , setUnsafeCallback
  , setForeignCallback
  , cancelCallback
  , ignoreTypes
  , getMessage
  , getMessageSized
  , getMessageMutable
  , defaultOutput
  , createOutput
  , sendMessage
  , sendUnsafeMessage
  , currentApi
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception, throwIO)
import Control.Monad (unless, void)
import Data.Coerce (coerce)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, Storable (..), alloca, allocaArray, nullPtr, peekArray)
import Foreign.C (CDouble (..), CInt (..), CSize, CString, CUChar (..), peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import GHC.Generics (Generic)
import Sound.RtMidi.Foreign
  ( Api (..)
  , Wrapper (..)
  , fromApi
  , rtmidi_api_display_name
  , rtmidi_api_name
  , rtmidi_close_port
  , rtmidi_compiled_api_by_name
  , rtmidi_get_compiled_api
  , rtmidi_get_port_count
  , rtmidi_get_port_name
  , rtmidi_in_cancel_callback
  , rtmidi_in_create
  , rtmidi_in_create_default
  , rtmidi_in_free
  , rtmidi_in_get_current_api
  , rtmidi_in_get_message
  , rtmidi_in_ignore_types
  , rtmidi_in_set_callback
  , rtmidi_open_port
  , rtmidi_open_virtual_port
  , rtmidi_out_create
  , rtmidi_out_create_default
  , rtmidi_out_free
  , rtmidi_out_get_current_api
  , rtmidi_out_send_message
  , toApi
  )

-- The default message size (in bytes) expected from 'getMessage'
-- Mostly just needs to be bigger than the max MIDI message size, which is 3 bytes
-- However, sysex messages can be quite large, so you might have to use the 'getMessageSized' variant.
defaultMessageSize :: Int
defaultMessageSize = 4

-- | Allows us to discriminate in/out functions in generic contexts
data DeviceType
  = InputDeviceType
  | OutputDeviceType
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

newtype Device = Device {unDevice :: ForeignPtr Wrapper}
  deriving stock (Eq, Show)

-- | Generalizes 'InputDevice' and 'OutputDevice' for use in common functions
class IsDevice d where
  toDevice :: d -> Device
  getDeviceType :: d -> DeviceType

-- | A handle to a device to be used for input
newtype InputDevice = InputDevice {unInputDevice :: Device}
  deriving stock (Eq, Show)

instance IsDevice InputDevice where
  toDevice = unInputDevice
  getDeviceType _ = InputDeviceType

newInputDevice :: Ptr Wrapper -> IO InputDevice
newInputDevice = fmap (InputDevice . Device) . newForeignPtr rtmidi_in_free

-- | A handle to a device to be used for input
newtype OutputDevice = OutputDevice {unOutputDevice :: Device}
  deriving stock (Eq, Show)

instance IsDevice OutputDevice where
  toDevice = unOutputDevice
  getDeviceType _ = OutputDeviceType

newOutputDevice :: Ptr Wrapper -> IO OutputDevice
newOutputDevice = fmap (OutputDevice . Device) . newForeignPtr rtmidi_out_free

-- | An internal RtMidi error
newtype Error = Error {unError :: String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Exception Error

-- Detects and throws internal errors
guardError :: Ptr Wrapper -> IO ()
guardError dptr = do
  w <- peek dptr
  unless (ok w) $ do
    e <- peekCString (msg w)
    throwIO (Error e)

-- Operate on the underlying device ptr
withDevicePtrUnguarded :: IsDevice d => d -> (Ptr Wrapper -> IO a) -> IO a
withDevicePtrUnguarded = withForeignPtr . unDevice . toDevice

-- Operate on the underlying device ptr and guard for errors
withDevicePtr :: IsDevice d => d -> (Ptr Wrapper -> IO a) -> IO a
withDevicePtr d f = withDevicePtrUnguarded d (\dptr -> f dptr <* guardError dptr)

-- | Get the display name for the given 'Api'.
apiDisplayName :: Api -> IO String
apiDisplayName api = rtmidi_api_display_name (fromApi api) >>= peekCString

-- | Get the internal name for the given 'Api'.
apiName :: Api -> IO String
apiName api = rtmidi_api_name (fromApi api) >>= peekCString

-- | Lookup a compiled 'Api' by name.
compiledApiByName :: String -> IO Api
compiledApiByName name = fmap toApi (withCString name rtmidi_compiled_api_by_name)

-- | Check if a device is ok
ready :: IsDevice d => d -> IO Bool
ready d = withDevicePtr d (fmap ok . peek)

-- | A static function to determine MIDI 'Api's built in.
compiledApis :: IO [Api]
compiledApis = do
  n <- fmap fromIntegral (rtmidi_get_compiled_api nullPtr 0)
  as <- allocaArray n $ \ptr -> do
    rtmidi_get_compiled_api ptr (fromIntegral n)
    peekArray n ptr
  pure (map toApi as)

-- | Open a MIDI connection
openPort
  :: IsDevice d
  => d
  -> Int
  -- ^ port number
  -> String
  -- ^ name for the application port that is used
  -> IO ()
openPort d n name = withDevicePtr d $ \dptr ->
  withCString name (rtmidi_open_port dptr (toEnum n))

-- | This function creates a virtual MIDI output port to which other software applications can connect.
--
-- This type of functionality is currently only supported by the Macintosh OS X, Linux ALSA and JACK APIs
-- (the function does nothing with the other APIs).
openVirtualPort :: IsDevice d => d -> String -> IO ()
openVirtualPort d name = withDevicePtr d $ \dptr -> do
  withCString name (rtmidi_open_virtual_port dptr)

-- | Close an open MIDI connection (if one exists).
closePort :: IsDevice d => d -> IO ()
closePort d = withDevicePtr d rtmidi_close_port

-- | Return the number of MIDI ports available to the 'Device'.
portCount :: IsDevice d => d -> IO Int
portCount d = withDevicePtr d $ \dptr -> do
  x <- rtmidi_get_port_count dptr
  pure (fromIntegral x)

-- | Return a string identifier for the specified MIDI port number.
--
-- 'Nothing' is returned if an invalid port specifier is provided.
portName :: IsDevice d => d -> Int -> IO (Maybe String)
portName d n = withDevicePtrUnguarded d $ \dptr -> do
  x <- rtmidi_get_port_name dptr (toEnum n)
  guardError dptr
  s <- peekCString x
  case s of
    [] -> pure Nothing
    _ -> pure (Just s)

-- | Convenience function to list ports.
--
-- Note that the underlying library does not offer an "atomic" interface for this
-- so results may be inconsistent if you connect/disconnect ports during this call.
listPorts :: IsDevice d => d -> IO [(Int, String)]
listPorts d = portCount d >>= go [] 0
 where
  go acc i c =
    if i >= c
      then pure (reverse acc)
      else do
        mn <- portName d i
        let acc' = maybe acc (\n -> (i, n) : acc) mn
        go acc' (succ i) c

-- | Convenience function to lookup the first port satisfying the predicate.
--
-- You may want to find an exact name:
--
-- > findPort d (== name)
--
-- Or you may want to match part of a name:
--
-- > findPort d (isInfixOf name)
--
-- Note that if you are performing many lookups, it's better to use 'listPorts' and
-- do the lookups yourself (see the caveats there too).
findPort :: IsDevice d => d -> (String -> Bool) -> IO (Maybe Int)
findPort d f = portCount d >>= go 0
 where
  go i c =
    if i >= c
      then pure Nothing
      else do
        mn <- portName d i
        case mn of
          Just n | f n -> pure (Just i)
          _ -> go (succ i) c

-- | Default constructor for a 'Device' to use for input.
defaultInput :: IO InputDevice
defaultInput = do
  dptr <- rtmidi_in_create_default
  guardError dptr
  newInputDevice dptr

-- | Create a new 'Device' to use for input.
createInput
  :: Api
  -- ^ API to use
  -> String
  -- ^ client name
  -> Int
  -- ^ size of the MIDI input queue
  -> IO InputDevice
createInput api clientName queueSizeLimit = do
  dptr <- withCString clientName (\str -> rtmidi_in_create (fromApi api) str (fromIntegral queueSizeLimit))
  guardError dptr
  newInputDevice dptr

foreign import ccall "wrapper"
  mkCallbackPointer :: (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()) -> IO (FunPtr (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()))

adaptCallbackCTypes :: (Double -> VS.Vector Word8 -> IO ()) -> (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ())
adaptCallbackCTypes !f (CDouble !t) m s _ = do
  buf <- VS.generateM (fromIntegral s) (peekElemOff (coerce m))
  f t buf

adaptUnsafeCallbackCTypes :: (Double -> Ptr Word8 -> Int -> IO ()) -> (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ())
adaptUnsafeCallbackCTypes !f (CDouble !t) m s _ = f t (coerce m) (fromIntegral s)

-- | Set a callback function to be invoked for incoming MIDI messages.
--
-- The callback function will be called whenever an incoming MIDI message is received.
-- While not absolutely necessary, it is best to set the callback function before opening a MIDI port to avoid leaving
-- some messages in the queue.
setCallback
  :: InputDevice
  -> (Double -> VS.Vector Word8 -> IO ())
  -- ^ Function that takes a timestamp and a MIDI message as arguments
  -> IO ()
setCallback d c = withDevicePtr d $ \dptr -> do
  f <- mkCallbackPointer (adaptCallbackCTypes c)
  rtmidi_in_set_callback dptr f nullPtr

-- | A variant of 'setCallback' that takes a raw pointer and length. It is unsafe to share or reference the pointer beyond the
-- scope of the callback, as the RtMidi-owned memory it references may have been changed or freed.
setUnsafeCallback
  :: InputDevice
  -> (Double -> Ptr Word8 -> Int -> IO ())
  -> IO ()
setUnsafeCallback d c = withDevicePtr d $ \dptr -> do
  f <- mkCallbackPointer (adaptUnsafeCallbackCTypes c)
  rtmidi_in_set_callback dptr f nullPtr

-- | Set a /foreign/ callback function to be invoked for incoming MIDI messages.
--
-- This variant allows you to set the callback to a C function pointer so we're not forced
-- to enter a Haskell wrapper every invocation.
setForeignCallback
  :: InputDevice
  -> FunPtr (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ())
  -> Ptr ()
  -- ^ Pointer to context that will be passed into the callback
  -> IO ()
setForeignCallback d f ctx = withDevicePtr d $ \dptr ->
  rtmidi_in_set_callback dptr f ctx

-- | Cancel use of the current callback function (if one exists).
--
-- Subsequent incoming MIDI messages will be written to the queue and can be retrieved with the `getMessage` function.
cancelCallback :: InputDevice -> IO ()
cancelCallback d = withDevicePtr d rtmidi_in_cancel_callback

-- | Specify whether certain MIDI message types should be queued or ignored during input.
--
-- By default, MIDI timing and active sensing messages are ignored during message input because of their
-- relative high data rates. MIDI sysex messages are ignored by default as well.
-- Variable values of 'True' imply that the respective message type will be ignored.
ignoreTypes
  :: InputDevice
  -> Bool
  -- ^ SysEx messages
  -> Bool
  -- ^ Time messages
  -> Bool
  -- ^ Sense messages
  -> IO ()
ignoreTypes d x y z = withDevicePtrUnguarded d (\dptr -> rtmidi_in_ignore_types dptr x y z)

-- | Variant of 'getMessage' that allows you to fill a shared buffer, returning timestamp and size.
getMessageMutable :: InputDevice -> VSM.IOVector Word8 -> IO (Double, Int)
getMessageMutable d buf = alloca $ \s -> VSM.unsafeWith buf $ \m -> withDevicePtrUnguarded d $ \dptr -> do
  poke s (fromIntegral (VSM.length buf))
  CDouble !timestamp <- rtmidi_in_get_message dptr (coerce m) s
  guardError dptr
  csize <- peek s
  let !size = fromIntegral csize
  pure (timestamp, size)

-- | Variant of 'getMessage' that allows you to set message buffer size (typically for large sysex messages).
getMessageSized :: InputDevice -> Int -> IO (Double, VS.Vector Word8)
getMessageSized d n = do
  buf <- VSM.new n
  (!timestamp, !size) <- getMessageMutable d buf
  vec <- VSM.unsafeWith buf (VS.generateM size . peekElemOff)
  pure (timestamp, vec)

-- | Return data bytes for the next available MIDI message in the input queue and the event delta-time in seconds.
--
-- This function returns immediately whether a new message is available or not.
-- A valid message is indicated by whether the list contains any elements.
-- Note that large sysex messages will be silently dropped! Use 'getMessageSized' or use a callback to get these safely.
getMessage :: InputDevice -> IO (Double, VS.Vector Word8)
getMessage d = getMessageSized d defaultMessageSize

-- | Default constructor for a 'Device' to use for output.
defaultOutput :: IO OutputDevice
defaultOutput = do
  dptr <- rtmidi_out_create_default
  guardError dptr
  newOutputDevice dptr

-- | Create a new 'Device' to use for output.
createOutput
  :: Api
  -- ^ API to use
  -> String
  -- ^ client name
  -> IO OutputDevice
createOutput api clientName = do
  dptr <- withCString clientName (rtmidi_out_create (fromApi api))
  guardError dptr
  newOutputDevice dptr

-- | Immediately send a single message out an open MIDI output port.
sendMessage :: OutputDevice -> VS.Vector Word8 -> IO ()
sendMessage d buf = VS.unsafeWith buf (\ptr -> sendUnsafeMessage d ptr (VS.length buf))

-- | A variant of 'sendMessage' that allows reading directly from pinned memory.
sendUnsafeMessage :: OutputDevice -> Ptr Word8 -> Int -> IO ()
sendUnsafeMessage d ptr size =
  withDevicePtr d $ \dptr -> void (rtmidi_out_send_message dptr (coerce ptr) (fromIntegral size))

-- | Returns the specifier for the MIDI 'Api' in use
currentApi :: IsDevice d => d -> IO Api
currentApi d = withDevicePtr d $ \dptr -> do
  res <-
    case getDeviceType d of
      InputDeviceType -> rtmidi_in_get_current_api dptr
      OutputDeviceType -> rtmidi_out_get_current_api dptr
  pure (toApi res)
