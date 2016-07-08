{-# LANGUAGE CPP #-}
{-|
Module      : System.Linux.Netlink.Route.LinkStat
Description : The implementation for netlinks route linkstat portion
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module System.Linux.Netlink.Route.LinkStat
  ( LinkStat
  , getLinkStat64
  , putLinkStat64
  , getLinkStat32
  , putLinkStat32
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)

import System.Linux.Netlink.Helpers (g64, p64, g32, p32)
import Data.Word (Word64)

-- |Data Structure for rtnl link stats
data LinkStat = LinkStat
  { rxPackets         :: Word64
  , txPackets         :: Word64
  , rxBytes           :: Word64
  , txBytes           :: Word64
  , rxErrors          :: Word64
  , txErrors          :: Word64
  , rxDropped         :: Word64
  , txDropped         :: Word64
  , multicast         :: Word64
  , collisions        :: Word64

  , rxLengthErrors    :: Word64
  , rxOverErrors      :: Word64
  , rxCRCErrors       :: Word64
  , rxFrameErrors     :: Word64
  , rxFifoErrors      :: Word64
  , rxMissedErrors    :: Word64

  , txAbortedErrors   :: Word64
  , txCarrierErrors   :: Word64
  , txFifoErrors      :: Word64
  , txHeartbeatErrors :: Word64
  , txWindowErrors    :: Word64

  , rxCompressed      :: Word64
  , txCompressed      :: Word64
  } deriving (Eq, Show)


-- |Get a 'LinkStat' object from a 64bit C struct
getLinkStat64 :: Get LinkStat
getLinkStat64 = getLinkStat g64

-- |Put a 'LinkStat' object into a 64bit C struct
putLinkStat64 :: LinkStat -> Put
putLinkStat64 = putLinkStat p64

-- |Get a 'LinkStat' object from a 32bit C struct
getLinkStat32 :: Get LinkStat
getLinkStat32 = getLinkStat g32

-- |Put a 'LinkStat' object into a 32bit C struct
putLinkStat32 :: LinkStat -> Put
putLinkStat32 = putLinkStat p32



-- Internal helper functions:
getLinkStat :: Integral a => Get a -> Get LinkStat
getLinkStat get = do
  rxp <- fromIntegral <$> get
  txp <- fromIntegral <$> get
  rxb <- fromIntegral <$> get
  txb <- fromIntegral <$> get
  rxe <- fromIntegral <$> get
  txe <- fromIntegral <$> get
  rxd <- fromIntegral <$> get
  txd <- fromIntegral <$> get
  mul <- fromIntegral <$> get
  col <- fromIntegral <$> get

  rxle <- fromIntegral <$> get
  rxoe <- fromIntegral <$> get
  rxce <- fromIntegral <$> get
  rxfe <- fromIntegral <$> get
  rxfi <- fromIntegral <$> get
  rxme <- fromIntegral <$> get

  txae <- fromIntegral <$> get
  txce <- fromIntegral <$> get
  txfi <- fromIntegral <$> get
  txhe <- fromIntegral <$> get
  txwe <- fromIntegral <$> get

  rxco <- fromIntegral <$> get
  txco <- fromIntegral <$> get
  return (LinkStat rxp txp rxb txb rxe
                   txe rxd txd mul col
                   rxle rxoe rxce rxfe
                   rxfi rxme txae txce
                   txfi txhe txwe rxco
                   txco)

putLinkStat :: Num a => (a -> Put) -> LinkStat -> Put
putLinkStat put msg = do
  put . fromIntegral $ rxPackets msg
  put . fromIntegral $ txPackets msg
  put . fromIntegral $ rxBytes msg
  put . fromIntegral $ txBytes msg
  put . fromIntegral $ rxErrors msg
  put . fromIntegral $ txErrors msg
  put . fromIntegral $ rxDropped msg
  put . fromIntegral $ txDropped msg
  put . fromIntegral $ multicast msg
  put . fromIntegral $ collisions msg

  put . fromIntegral $ rxLengthErrors msg
  put . fromIntegral $ rxOverErrors msg
  put . fromIntegral $ rxCRCErrors msg
  put . fromIntegral $ rxFrameErrors msg
  put . fromIntegral $ rxFifoErrors msg
  put . fromIntegral $ rxMissedErrors msg

  put . fromIntegral $ txAbortedErrors msg
  put . fromIntegral $ txCarrierErrors msg
  put . fromIntegral $ txFifoErrors msg
  put . fromIntegral $ txHeartbeatErrors msg
  put . fromIntegral $ txWindowErrors msg

  put . fromIntegral $ rxCompressed msg
  put . fromIntegral $ txCompressed msg

