{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|
Module      : System.Linux.Netlink.Route
Description : The implementation for netlinks route family
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides wrappers for functionality provided by the netlink route family
-}
module System.Linux.Netlink.Route
    (
      Packet

    , getRoutePackets
    , Message(..)
    
    , getLinkAddress
    , getLinkBroadcast
    , getLinkName
    , getLinkMTU
    , getLinkQDisc
    , getLinkTXQLen

    , putLinkAddress
    , putLinkBroadcast
    , putLinkName
    , putLinkMTU
    , putLinkQDisc
    , putLinkTXQLen
    ) where

import Prelude hiding (length, lookup, init)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.ByteString.Char8 (ByteString, append, init, pack, unpack)
import Data.Char (chr, ord)
import Data.Map (insert, lookup)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word32)

import System.Linux.Netlink.Constants
import System.Linux.Netlink
import System.Linux.Netlink.Helpers

-- |The static data for route messages
data Message = NLinkMsg
    {
      interfaceType  :: LinkType
    , interfaceIndex :: Word32
    , interfaceFlags :: Word32
    }
             | NAddrMsg
    {
      addrFamily         :: AddressFamily
    , addrMaskLength     :: Word8
    , addrFlags          :: Word8
    , addrScope          :: Word8
    , addrInterfaceIndex :: Word32
    } deriving (Eq, Show)

instance Convertable Message where
  getGet = getMessage
  getPut = putMessage

-- |Typedef for route messages
type RoutePacket = Packet Message

--
-- Generic functions
--



--TODO maybe this should be changed
--
-- New generic stuffs
--

getMessage :: MessageType -> Get Message
getMessage msgtype | msgtype == eRTM_NEWLINK = getMessageLink
                   | msgtype == eRTM_GETLINK = getMessageLink
                   | msgtype == eRTM_DELLINK = getMessageLink
                   | msgtype == eRTM_NEWADDR = getMessageAddr
                   | msgtype == eRTM_GETADDR = getMessageAddr
                   | msgtype == eRTM_DELADDR = getMessageAddr
                   | otherwise               =
                       error $ "Can't decode message " ++ show msgtype

getMessageLink :: Get Message
getMessageLink = do
    skip 2
    ty    <- fromIntegral <$> g16
    idx   <- g32
    flags <- g32
    skip 4
    return $ NLinkMsg ty idx flags

getMessageAddr :: Get Message
getMessageAddr = do
    fam <- fromIntegral <$> g8
    maskLen <- g8
    flags <- g8
    scope <- fromIntegral <$> g8
    idx <- g32
    return $ NAddrMsg fam maskLen flags scope idx

putMessage :: Message -> Put
putMessage (NLinkMsg ty idx flags) = do
    p8 eAF_UNSPEC >> p8 0
    p16 (fromIntegral ty)
    p32 idx
    p32 flags
    p32 0xFFFFFFFF
putMessage (NAddrMsg fam maskLen flags scope idx) = do
    p8 (fromIntegral fam)
    p8 maskLen
    p8 flags
    p8 (fromIntegral scope)
    p32 idx

-- |'Get' a route message or an error
getRoutePackets :: ByteString -> Either String [RoutePacket]
getRoutePackets = getPackets

-- |typedef for utility functions
type AttributeReader a = Attributes -> Maybe a

-- |typedef for utility functions
type AttributeWriter a = a -> Attributes -> Attributes

--
-- Link message attributes
--
type LinkAddress = (Word8, Word8, Word8, Word8, Word8, Word8)

-- |get L2 address from netlink attributes
getLinkAddress :: AttributeReader LinkAddress
getLinkAddress attrs = decodeMAC <$> lookup eIFLA_ADDRESS attrs

-- |set L2 address on netlink attributes
putLinkAddress :: AttributeWriter LinkAddress
putLinkAddress addr = insert eIFLA_ADDRESS (encodeMAC addr)

-- |get L2 broadcast address from netlink attributes
getLinkBroadcast :: AttributeReader LinkAddress
getLinkBroadcast attrs = decodeMAC <$> lookup eIFLA_BROADCAST attrs

-- |set L2 broadcast address on netlink attributes
putLinkBroadcast :: AttributeWriter LinkAddress
putLinkBroadcast addr = insert eIFLA_BROADCAST (encodeMAC addr)

-- |get interface name from netlink attributes
getLinkName :: AttributeReader String
getLinkName attrs = getString <$> lookup eIFLA_IFNAME attrs

-- |set interface name on netlink attributes
putLinkName :: AttributeWriter String
putLinkName ifname = insert eIFLA_IFNAME (putString ifname)

-- |get mtu from netlink attributes
getLinkMTU :: AttributeReader Word32
getLinkMTU attrs = get32 =<< lookup eIFLA_MTU attrs

-- |set mtu on netlink attributes
putLinkMTU :: AttributeWriter Word32
putLinkMTU mtu = insert eIFLA_MTU (put32 mtu)

-- TODO: IFLA_LINK - need to understand what it does

-- |I actually have no idea what QDisc is
getLinkQDisc :: AttributeReader String
getLinkQDisc attrs = getString <$> lookup eIFLA_QDISC attrs

-- |I actually have no idea what QDisc is
putLinkQDisc :: AttributeWriter String
putLinkQDisc disc = insert eIFLA_QDISC (putString disc)

-- TODO: IFLA_STATS - bloody huge message, will deal with it later.

-- TODO: IFLA_{COST,PRIORITY,MASTER,WIRELESS,PROTINFO} - need to
-- understand what they do.

-- |I should look this up
getLinkTXQLen :: AttributeReader Word32
getLinkTXQLen attrs = get32 =<< lookup eIFLA_TXQLEN attrs

-- |I should look this up
putLinkTXQLen :: AttributeWriter Word32
putLinkTXQLen len = insert eIFLA_TXQLEN (put32 len)

-- TODO: IFLA_{MAP,WEIGHT} - need to figure out

-- TODO: IFLA_{LINKMODE,LINKINFO} - see Documentation/networking/operstates.txt

-- TODO: IFLA_{NET_NS_PID,IFALIAS} - need to figure out

--
-- Helpers
--

decodeMAC :: ByteString -> LinkAddress
decodeMAC = tuplify . map (fromIntegral . ord) . unpack
  where tuplify [a,b,c,d,e,f] = (a,b,c,d,e,f)
        tuplify _ = error "Bad encoded MAC"

encodeMAC :: LinkAddress -> ByteString
encodeMAC = pack . map (chr . fromIntegral) . listify
  where listify (a,b,c,d,e,f) = [a,b,c,d,e,f]

getString :: ByteString -> String
getString b = unpack (init b)

putString :: String -> ByteString
putString s = append (pack s) "\0"

get32 :: ByteString -> Maybe Word32
get32 bs = case runGet getWord32host bs of
    Left  _ -> Nothing
    Right w -> Just w

put32 :: Word32 -> ByteString
put32 w = runPut (putWord32host w)
