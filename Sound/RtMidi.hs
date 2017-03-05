{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtMidi (
      Device(..)
    , Api(..)
    , apiSize
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
    , defaultOutput
    , createOutput
    , sendMessage
    , closeInput
    , closeOutput
    , currentApi
    ) where

import Control.Monad
import Foreign
import Foreign.C
import Foreign.C.String

data Device = Input (Ptr ()) | Output (Ptr ())

device :: Device -> Ptr ()
device (Input x) = x
device (Output x) = x

toInput :: Device -> Ptr ()
toInput (Input x) = x

toOutput :: Device -> Ptr ()
toOutput (Output x) = x

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
   rtmidi_get_compiled_api :: Ptr (Ptr CInt) -> IO CInt

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
   rtmidi_in_set_callback :: Ptr () -> FunPtr (CDouble -> Ptr CUChar -> Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_cancel_callback"
   rtmidi_in_cancel_callback :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_ignore_types"
   rtmidi_in_ignore_types :: Ptr () -> Bool -> Bool -> Bool -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_get_message"
   rtmidi_in_get_message :: Ptr () -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CDouble

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
   rtmidi_out_create_default :: IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_out_create"
   rtmidi_out_create :: CInt -> CString -> IO (Ptr ())

foreign import ccall "rtmidi_c.h rtmidi_out_free"
   rtmidi_out_free :: Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_out_get_current_api"
   rtmidi_out_get_current_api :: Ptr () -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_out_send_message"
   rtmidi_out_send_message :: Ptr () -> Ptr CUChar -> CInt -> IO CInt


apiSize :: IO Int
apiSize = fromEnum <$> rtmidi_sizeof_rtmidi_api

-- TODO: error handling
compiledApis :: IO [Api]
compiledApis = fmap (map (toEnum . fromEnum)) $ do
   n <- fromIntegral <$> rtmidi_get_compiled_api nullPtr
   allocaArray n $ flip with $ \ptr -> do
      rtmidi_get_compiled_api ptr
      peekArray n =<< peek ptr

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

createInput :: Api -> String -> Int -> IO Device
createInput api clientName queueSizeLimit = Input <$>
   (withCString clientName $ \str -> rtmidi_in_create (toEnum $ fromEnum api) str (toEnum queueSizeLimit))

foreign import ccall "wrapper"
  wrap :: (CDouble -> Ptr CUChar -> Ptr () -> IO ()) -> IO (FunPtr (CDouble -> Ptr CUChar -> Ptr () -> IO ()))

setCallback :: Device -> (CDouble -> Ptr CUChar -> IO ()) -> IO ()
setCallback d c = flip (rtmidi_in_set_callback (toInput d)) nullPtr =<< (wrap $ ((const .).) c)

cancelCallback :: Device -> IO ()
cancelCallback d = rtmidi_in_cancel_callback (toInput d)

ignoreTypes :: Device -> Bool -> Bool -> Bool -> IO ()
ignoreTypes d sysex time sense = rtmidi_in_ignore_types (toInput d) sysex time sense

-- TODO: error handling
getMessage :: Device -> IO ([CUChar], Double)
getMessage d = alloca $ \m -> alloca $ \s -> do
   timestamp <- rtmidi_in_get_message (toInput d) m s
   size <- peek s
   message <- peekArray (fromIntegral size) =<< peek m
   return (message, toEnum $ fromEnum timestamp)

defaultOutput :: IO Device
defaultOutput = Output <$> rtmidi_out_create_default

createOutput :: Api -> String -> IO Device
createOutput api clientName = Output <$>
   (withCString clientName $ rtmidi_out_create (toEnum (fromEnum api)))

-- TODO: error handling
sendMessage :: Device -> [CUChar] -> IO ()
sendMessage d m = withArrayLen m $
   \n ptr -> rtmidi_out_send_message (toOutput d) ptr (fromIntegral n) >> return ()

closeInput (Input x) = rtmidi_in_free x

closeOutput (Output x) = rtmidi_out_free x

currentApi :: Device -> IO Api
currentApi d = (toEnum . fromEnum) <$>
   case d of
      Input x -> rtmidi_in_get_current_api x
      Output x -> rtmidi_out_get_current_api x

