{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI defs for RtMidi
module Sound.RtMidi.Foreign
  ( Wrapper (..)
  , Api (..)
  , ApiInternal
  , toApi
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
  ) where

#include "rtmidi_c.h"

import Control.DeepSeq (NFData)
import Foreign (FunPtr, Ptr, Storable (..))
import Foreign.C (CDouble (..), CInt (..), CString, CSize, CUChar, CUInt (..))
import GHC.Generics (Generic)

data Wrapper = Wrapper
  { ptr :: !(Ptr ())
  , dat :: !(Ptr ())
  , ok  :: !Bool
  , msg :: !CString
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Storable Wrapper where
  sizeOf _ = #{size struct RtMidiWrapper}
  alignment _ = #{alignment struct RtMidiWrapper}
  poke ptr (Wrapper a b c d) = do
    #{poke struct RtMidiWrapper, ptr} ptr a
    #{poke struct RtMidiWrapper, data} ptr b
    #{poke struct RtMidiWrapper, ok} ptr c
    #{poke struct RtMidiWrapper, msg} ptr d
  peek ptr = do
    a <- #{peek struct RtMidiWrapper, ptr} ptr
    b <- #{peek struct RtMidiWrapper, data} ptr
    c <- #{peek struct RtMidiWrapper, ok} ptr
    d <- #{peek struct RtMidiWrapper, msg} ptr
    pure (Wrapper a b c d)

-- | Enum of RtMidi-supported APIs
data Api
  = UnspecifiedApi
  | CoreMidiApi
  | AlsaApi
  | JackApi
  | MultimediaApi
  | DummyApi
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- A parameter we'll be de/serializing from the 'Api' enum.
newtype ApiInternal = ApiInternal { unApiInternal :: CInt } deriving newtype (Storable)

toApi :: ApiInternal -> Api
toApi = toEnum . fromIntegral . unApiInternal

fromApi :: Api -> ApiInternal
fromApi = ApiInternal . fromIntegral . fromEnum

foreign import ccall "rtmidi_c.h rtmidi_api_display_name"
  rtmidi_api_display_name :: ApiInternal -> IO CString

foreign import ccall "rtmidi_c.h rtmidi_api_name"
  rtmidi_api_name :: ApiInternal -> IO CString

foreign import ccall "rtmidi_c.h rtmidi_close_port"
  rtmidi_close_port :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_compiled_api_by_name"
  rtmidi_compiled_api_by_name :: CString -> IO ApiInternal

foreign import ccall "rtmidi_c.h rtmidi_get_compiled_api"
  rtmidi_get_compiled_api :: Ptr ApiInternal -> CUInt -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_get_port_count"
  rtmidi_get_port_count :: Ptr Wrapper -> IO CUInt

foreign import ccall "rtmidi_c.h rtmidi_get_port_name"
  rtmidi_get_port_name :: Ptr Wrapper -> CUInt -> CString -> Ptr CInt -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_in_cancel_callback"
  rtmidi_in_cancel_callback :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_create"
  rtmidi_in_create :: ApiInternal -> CString -> CUInt -> IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_in_create_default"
  rtmidi_in_create_default :: IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h &rtmidi_in_free"
  rtmidi_in_free :: FunPtr (Ptr Wrapper -> IO ())

foreign import ccall "rtmidi_c.h rtmidi_in_get_current_api"
  rtmidi_in_get_current_api :: Ptr Wrapper -> IO ApiInternal

foreign import ccall "rtmidi_c.h rtmidi_in_get_message"
  rtmidi_in_get_message :: Ptr Wrapper -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CDouble

foreign import ccall "rtmidi_c.h rtmidi_in_ignore_types"
  rtmidi_in_ignore_types :: Ptr Wrapper -> Bool -> Bool -> Bool -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_set_callback"
  rtmidi_in_set_callback :: Ptr Wrapper -> FunPtr (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_open_port"
  rtmidi_open_port :: Ptr Wrapper -> CUInt -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_open_virtual_port"
  rtmidi_open_virtual_port :: Ptr Wrapper -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_out_create"
  rtmidi_out_create :: ApiInternal -> CString -> IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
  rtmidi_out_create_default :: IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h &rtmidi_out_free"
  rtmidi_out_free :: FunPtr (Ptr Wrapper -> IO ())

foreign import ccall "rtmidi_c.h rtmidi_out_get_current_api"
  rtmidi_out_get_current_api :: Ptr Wrapper -> IO ApiInternal

foreign import ccall "rtmidi_c.h rtmidi_out_send_message"
  rtmidi_out_send_message :: Ptr Wrapper -> Ptr CUChar -> CInt -> IO CInt
