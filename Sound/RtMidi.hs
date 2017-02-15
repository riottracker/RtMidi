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
    ) where

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

foreign import ccall "rtmidi_c.h rtmidi_in_create_default"
   rtmidi_in_create_default :: IO (Ptr ())

defaultInput :: IO Device
defaultInput = fmap Input rtmidi_in_create_default

foreign import ccall "rtmidi_c.h rtmidi_in_create"
   rtmidi_in_create :: CInt -> CString -> CInt -> IO (Ptr ())

input :: Api -> String -> Int -> IO Device
input api clientName queueSizeLimit = fmap Input $ withCString clientName (\x -> rtmidi_in_create (toEnum (fromEnum api)) x (toEnum queueSizeLimit))

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
   rtmidi_out_create_default :: IO (Ptr ())

defaultOutput :: IO Device
defaultOutput = fmap Output rtmidi_out_create_default

foreign import ccall "rtmidi_c.h rtmidi_out_create"
   rtmidi_out_create :: CInt -> CString -> IO (Ptr ())

output :: Api -> String -> IO Device
output api clientName = fmap Output $ withCString clientName (\x -> rtmidi_out_create (toEnum (fromEnum api)) x)

foreign import ccall "rtmidi_c.h rtmidi_get_port_count"
   rtmidi_get_port_count :: Ptr () -> IO CInt

portCount :: Device -> IO Int
portCount d = fmap fromIntegral $ rtmidi_get_port_count $ device d

foreign import ccall "rtmidi_c.h rtmidi_get_port_name"
   rtmidi_get_port_name :: Ptr () -> CInt -> IO CString

portName :: Device -> Int -> IO String
portName d n = peekCString =<< rtmidi_get_port_name (device d) (toEnum n)

foreign import ccall "rtmidi_c.h rtmidi_in_get_current_api"
   rtmidi_in_get_current_api :: Ptr () -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_out_get_current_api"
   rtmidi_out_get_current_api :: Ptr () -> IO CInt

currentApi :: Device -> IO Api
currentApi d = fmap (toEnum . fromEnum) $
                  case d of
                     Input x -> rtmidi_in_get_current_api x
                     Output x -> rtmidi_out_get_current_api x

