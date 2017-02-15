{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtMidi (
      Device(..)
    , Api(..)
    , input
    , defaultInput
    , output
    , defaultOutput
    , portCount
    , portName
    , currentApi
--    , compiledApis
    ) where

import Control.Monad
import Foreign
import Foreign.C
import Foreign.C.String


data Device = Input (Ptr ()) | Output (Ptr ())

device :: Device -> Ptr ()
device d = case d of
             Input x -> x
             Output x -> x

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
  fromEnum KernelStreamingApi = 5
  fromEnum DummyApi = 6
  toEnum 0 = UnspecifiedApi
  toEnum 1 = CoreMidiApi
  toEnum 2 = AlsaApi
  toEnum 3 = JackApi
  toEnum 4 = MultimediaApi
  toEnum 5 = KernelStreamingApi
  toEnum 6 = DummyApi

data ErrorType
  = Warning
  | DebugWarning
  | UnspecifiedError
  | NoDevicesFound
  | InvalidDevice
  | MemoryError
  | InvalidParameter
  | InvalidUse
  | DriverError
  | SystemError
  | ThreadError
  deriving (Eq, Show)


foreign import ccall "rtmidi_c.h rtmidi_sizeof_rtmidi_api"
   rtmidi_sizeof_rtmidi_api :: IO CInt


foreign import ccall "rtmidi_c.h rtmidi_get_compiled_api"
   rtmidi_get_compiled_api :: (Ptr CInt) -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_error"
   rtmidi_error :: CInt -> CString -> IO ()


foreign import ccall "rtmidi_c.h rtmidi_open_port"
   rtmidi_open_port :: Ptr () -> CInt -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_open_virtual_port"
   rtmidi_open_virtual_port :: Ptr () -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_close_port"
   rtmidi_close_port :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_get_port_count"
   rtmidi_get_port_count :: Ptr () -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_get_port_name"
   rtmidi_get_port_name :: Ptr () -> CInt -> IO CString


foreign import ccall "rtmidi_c.h rtmidi_in_create_default"
   rtmidi_in_create_default :: IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_in_create"
   rtmidi_in_create :: CInt -> CString -> CInt -> IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_in_free"
   rtmidi_in_free :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_get_current_api"
   rtmidi_in_get_current_api :: Ptr () -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_in_set_callback"
   rtmidi_in_set_callback :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_cancel_callback"
   rtmidi_in_cancel_callback :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_ignore_types"
   rtmidi_in_ignore_types :: Ptr () -> Bool -> Bool -> Bool -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_get_message"
   rtmidi_in_get_message :: Ptr () -> Ptr CString -> CSize -> IO CDouble

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
   rtmidi_out_create_default :: IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_out_create"
   rtmidi_out_create :: CInt -> CString -> IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_out_free"
   rtmidi_out_free :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_out_get_current_api"
   rtmidi_out_get_current_api :: Ptr () -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_out_send_message"
   rtmidi_out_send_message :: Ptr () -> CString -> CInt -> IO CInt


apiSize :: IO Int
apiSize = fromEnum <$> rtmidi_sizeof_rtmidi_api


--compiledApis :: IO [Api]
--compiledApis = fmap (map (toEnum . fromEnum)) $
--   alloca $ \ptr -> do
--   n <- rtmidi_get_compiled_api nullPtr
--   a <- rtmidi_get_compiled_api ptr
--   case fromIntegral a of
--   putStrLn $ "n: " ++ show n ++ " a:" ++ show a
--   peekArray (fromIntegral n) ptr


-- TODO: rtmidi_error

openPort :: Device -> Int -> String -> IO ()
openPort d n name = withCString name $ rtmidi_open_port (device d) (toEnum n)

openVirtualPort :: Device -> String -> IO ()
openVirtualPort d name = withCString name $ rtmidi_open_virtual_port (device d)

closePort :: Device -> IO ()
closePort d = rtmidi_close_port $ device d

portCount :: Device -> IO Int
portCount d = fromIntegral <$> (rtmidi_get_port_count $ device d)

portName :: Device -> Int -> IO String
portName d n = peekCString =<< rtmidi_get_port_name (device d) (toEnum n)


defaultInput :: IO Device
defaultInput = Input <$> rtmidi_in_create_default

input :: Api -> String -> Int -> IO Device
input api clientName queueSizeLimit = Input <$>
   (withCString clientName $ \str -> rtmidi_in_create (toEnum $ fromEnum api) str (toEnum queueSizeLimit))

-- TODO: rtmidi_in_free
-- TODO: callbacks
-- TODO: message filter
-- TODO: rtmidi_in_get_message

defaultOutput :: IO Device
defaultOutput = Output <$> rtmidi_out_create_default

output :: Api -> String -> IO Device
output api clientName = Output <$> (withCString clientName $ rtmidi_out_create (toEnum (fromEnum api)))

-- TODO: rtmidi_out_free
-- TODO: rtmidi_out_send_message

currentApi :: Device -> IO Api
currentApi d = (toEnum . fromEnum) <$>
   case d of
      Input x -> rtmidi_in_get_current_api x
      Output x -> rtmidi_out_get_current_api x

