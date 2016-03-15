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

-- |Indent a String by 2 " "s for output
indent :: String -> String
indent = unlines . map ("  " ++) . lines

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
-- | 'Put' 'Word64'
p64 :: Word64 -> Put
p64 = putWord64host

-- | 'Get' 'Word8'
g8 :: Get Word8
g8  = getWord8
-- | 'Get' 'Word16'
g16:: Get Word16
g16 = getWord16host
-- | 'Get' 'Word32'
g32 :: Get Word32
g32 = getWord32host
-- | 'Get' 'Word64'
g64 :: Get Word64
g64 = getWord64host
