{-# LANGUAGE EmptyDataDecls #-}

module Sound.RtMidi where

import Foreign
import Foreign.C
import Foreign.C.String


data Device
type Input = Ptr Device
type Output = Ptr Device

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
   defaultInput :: IO Input

foreign import ccall "rtmidi_c.h rtmidi_in_create"
   rtmidi_in_create :: CInt -> CString -> CInt -> IO Input

input :: Api -> String -> Int -> IO Input
input api clientName queueSizeLimit = withCString clientName (\x -> rtmidi_in_create (toEnum (fromEnum api)) x (toEnum queueSizeLimit))

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
   defaultOutput :: IO Output

foreign import ccall "rtmidi_c.h rtmidi_out_create"
   rtmidi_out_create :: CInt -> CString -> IO Output

output :: Api -> String -> IO Output
output api clientName = withCString clientName (\x -> rtmidi_out_create (toEnum (fromEnum api)) x)

foreign import ccall "rtmidi_c.h rtmidi_get_port_count"
   rtmidi_get_port_count :: Ptr Device -> IO CInt

portCount :: Ptr Device -> IO Int
portCount device = fmap fromIntegral (rtmidi_get_port_count device)


