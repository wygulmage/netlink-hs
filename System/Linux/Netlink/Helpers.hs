module System.Linux.Netlink.Helpers
where

import Data.Serialize.Get
import Data.Serialize.Put

import Data.Word


--
-- Helpers
--
p8 :: Word8 -> Put
p8  = putWord8
p16 :: Word16 -> Put
p16 = putWord16host
p32 :: Word32 -> Put
p32 = putWord32host

g8 :: Get Word8
g8  = getWord8
g16:: Get Word16
g16 = getWord16host
g32 :: Get Word32
g32 = getWord32host


