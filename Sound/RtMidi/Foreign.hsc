{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI defs for RtMidi
module Sound.RtMidi.Foreign
  ( Wrapper (..)
  , rtmidi_close_port
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

import Foreign (FunPtr, Ptr, Storable (..))
import Foreign.C (CDouble (..), CInt (..), CString, CSize, CUChar)

data Wrapper = Wrapper
  { ptr :: !(Ptr ())
  , dat :: !(Ptr ())
  , ok  :: !Bool
  , msg :: !CString
  } deriving (Eq, Show)

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

foreign import ccall "rtmidi_c.h rtmidi_close_port"
  rtmidi_close_port :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_get_compiled_api"
  rtmidi_get_compiled_api :: Ptr (Ptr CInt) -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_get_port_count"
  rtmidi_get_port_count :: Ptr Wrapper -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_get_port_name"
  rtmidi_get_port_name :: Ptr Wrapper -> CInt -> IO CString

foreign import ccall "rtmidi_c.h rtmidi_in_cancel_callback"
  rtmidi_in_cancel_callback :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_create"
  rtmidi_in_create :: CInt -> CString -> CInt -> IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_in_create_default"
  rtmidi_in_create_default :: IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_in_free"
  rtmidi_in_free :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_get_current_api"
  rtmidi_in_get_current_api :: Ptr Wrapper -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_in_get_message"
  rtmidi_in_get_message :: Ptr Wrapper -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CDouble

foreign import ccall "rtmidi_c.h rtmidi_in_ignore_types"
  rtmidi_in_ignore_types :: Ptr Wrapper -> Bool -> Bool -> Bool -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_in_set_callback"
  rtmidi_in_set_callback :: Ptr Wrapper -> FunPtr (CDouble -> Ptr CUChar -> CInt -> Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_open_port"
  rtmidi_open_port :: Ptr Wrapper -> CInt -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_open_virtual_port"
  rtmidi_open_virtual_port :: Ptr Wrapper -> CString -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_out_create"
  rtmidi_out_create :: CInt -> CString -> IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_out_create_default"
  rtmidi_out_create_default :: IO (Ptr Wrapper)

foreign import ccall "rtmidi_c.h rtmidi_out_free"
  rtmidi_out_free :: Ptr Wrapper -> IO ()

foreign import ccall "rtmidi_c.h rtmidi_out_get_current_api"
  rtmidi_out_get_current_api :: Ptr Wrapper -> IO CInt

foreign import ccall "rtmidi_c.h rtmidi_out_send_message"
  rtmidi_out_send_message :: Ptr Wrapper -> Ptr CUChar -> CInt -> IO CInt
