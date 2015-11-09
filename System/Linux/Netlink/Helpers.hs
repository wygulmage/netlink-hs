{-|
Module      : System.Linux.Netlink.Helpers
Description : Internal short names for Get and Put functions
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module System.Linux.Netlink.Helpers
where

import Data.Serialize.Get
import Data.Serialize.Put

import Data.Word


--
-- Helpers
--
-- | 'Put' 'Word8'
p8 :: Word8 -> Put
p8  = putWord8
-- | 'Put' 'Word16'
p16 :: Word16 -> Put
p16 = putWord16host
-- | 'Put' 'Word32'
p32 :: Word32 -> Put
p32 = putWord32host

-- | 'Get' 'Word8'
g8 :: Get Word8
g8  = getWord8
-- | 'Get' 'Word16'
g16:: Get Word16
g16 = getWord16host
-- | 'Get' 'Word32'
g32 :: Get Word32
g32 = getWord32host


