{-# LANGUAGE CPP #-}
{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of NL80211
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h
-}
module System.Linux.Netlink.GeNetlink.NL80211
  ( NL80211Socket
  , NL80211Packet
  , getWifiEIDs

  , makeNL80211Socket
  , joinMulticastByName
  , queryOne
  , query
  , getInterfaceList
  , getScanResults
  , getConnectedWifi
  , getWifiAttributes
  , getPaket
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad (liftM, liftM2, join)
import Control.Monad.Loops (whileM)
import Data.Bits ((.|.))
import Data.ByteString.Char8 (unpack)
import Data.Maybe (mapMaybe)
import Data.Serialize.Get (runGet, getByteString, getWord8 ,getWord32host, isEmpty, Get)
import Data.Serialize.Put (runPut, putWord32host)
import Data.Word (Word32, Word16, Word8)

import qualified Data.ByteString as BS
import qualified Data.Map as M (empty, lookup, fromList, member)



import System.Linux.Netlink.Constants
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control
import System.Linux.Netlink.GeNetlink.NL80211.Constants
import System.Linux.Netlink hiding (makeSocket, queryOne, query, recvOne)
import qualified System.Linux.Netlink as I (queryOne, query, recvOne)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
data NL80211Socket = NLS NetlinkSocket Word16

-- |typedef for messages send by this mdoule
type NL80211Packet = GenlPacket NoData

-- |typedef for lazyness
type ByteString = BS.ByteString --the name would just annoy me

-- |'Get' the EID Attributes from a buffer
getWifiEIDs :: Get Attributes
getWifiEIDs = M.fromList <$> whileM (not <$> isEmpty) getWifiEID

-- |'Get' an EID attribute from a buffer
getWifiEID :: Get (Int, ByteString)
getWifiEID = do
  ty  <- fromIntegral <$> getWord8
  len <- fromIntegral <$> getWord8
  val <- getByteString len
  return (ty, val)

getRight :: Show a => Either a b -> b
getRight (Right x) = x
getRight (Left err) = error $show err


-- |Create a 'NL80211Socket' this opens a genetlink socket and gets the family id
makeNL80211Socket :: IO NL80211Socket
makeNL80211Socket = do
  sock <- makeSocket
  fid <- getFamilyId sock "nl80211"
  return $NLS sock fid


-- |Join a nl80211 multicast group by name
joinMulticastByName :: NL80211Socket -> String -> IO ()
joinMulticastByName (NLS sock _) name = do
  (_, m)<-  getFamilyWithMulticasts sock "nl80211"
  let gid = getMulticast name m
  case gid of
    Nothing -> error $"Could not find \"" ++ name  ++ "\" multicast group"
    Just x -> joinMulticastGroup sock x


getRequestPacket :: Word16 -> Word8 -> Bool -> Attributes -> NL80211Packet
getRequestPacket fid cmd dump attrs =
  let header = Header (fromIntegral fid) flags 0 0
      geheader = GenlHeader cmd 0 in
    Packet header (GenlData geheader NoData) attrs
  where flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST


-- |queryOne for NL80211 (see 'System.Linux.Netlink.queryOne')
queryOne :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO NL80211Packet
queryOne (NLS sock fid) cmd dump attrs = I.queryOne sock packet
  where packet = getRequestPacket fid cmd dump attrs

-- |query for NL80211 (see 'System.Linux.Netlink.query')
query :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO [NL80211Packet]
query (NLS sock fid) cmd dump attrs = I.query sock packet
  where packet = getRequestPacket fid cmd dump attrs


parseInterface :: (ByteString, ByteString) -> (String, Word32)
parseInterface (name, ifindex) =
  (init $unpack name, getRight $runGet getWord32host ifindex)


-- |Get the list of interfaces currently managed by NL80211
getInterfaceList :: NL80211Socket -> IO [(String, Word32)]
getInterfaceList sock = do
  interfaces <- query sock eNL80211_CMD_GET_INTERFACE True M.empty
  return $mapMaybe (liftM parseInterface . toTuple) interfaces
  where toTuple (Packet _ _ attrs) = liftM2 (,) (name attrs) (findex attrs)
        toTuple (ErrorMsg{}) = error "Something stupid happened"
        toTuple (DoneMsg _) = Nothing
        name = M.lookup eNL80211_ATTR_IFNAME
        findex = M.lookup eNL80211_ATTR_IFINDEX


{- |get scan results

In testing this could be a big chunk of data when a scan just happened
or be pretty much only the currently connected wifi.

For more information about how this is structured look into kernel source
or just try it out.
-}
getScanResults
  :: NL80211Socket
  -> Word32 -- ^The id of the interface for which this should be looked up
  -> IO [NL80211Packet]
getScanResults sock ifindex = query sock eNL80211_CMD_GET_SCAN True attrs
  where attrs = M.fromList [(eNL80211_ATTR_IFINDEX, runPut $putWord32host ifindex)]

{- |Get the information about the currently connected wifi(s).

This would technically work for multiple connected wifis, but since we only get
information about one interface this should only ever be emtpy on a singleton list.

For more information about how this is structured look into kernel soruce
or just try it out.
-}
getConnectedWifi
  :: NL80211Socket
  -> Word32 -- ^The id of the interface for which this should be looked up
  -> IO [NL80211Packet]
getConnectedWifi sock ifindex = filter isConn <$> getScanResults sock ifindex
  where isConn (Packet _ _ attrs) = hasConn $M.lookup eNL80211_ATTR_BSS attrs
        isConn (ErrorMsg{}) = error "Something stupid happened"
        isConn (DoneMsg _) = False
        hasConn Nothing = False
        hasConn (Just attrs) = M.member eNL80211_BSS_STATUS $getRight $runGet getAttributes attrs


-- |Get the EID attributes from a 'NL80211Packet' (for example from 'getConnectedWifi'
getWifiAttributes :: NL80211Packet -> Maybe Attributes
getWifiAttributes (Packet _ _ attrs) =
  getRight <$> runGet getWifiEIDs <$> eids
  where bssattrs = getRight <$> runGet getAttributes <$> M.lookup eNL80211_ATTR_BSS attrs
        eids = join $liftM (M.lookup eNL80211_BSS_INFORMATION_ELEMENTS) bssattrs
getWifiAttributes (ErrorMsg{}) = error "Something stupid happened"
getWifiAttributes (DoneMsg _) = Nothing


-- |NL80211 version of 'System.Linux.Netlink.recvOne'
getPaket :: NL80211Socket -> IO [NL80211Packet]
getPaket (NLS sock _) = I.recvOne sock
