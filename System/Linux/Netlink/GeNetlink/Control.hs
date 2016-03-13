{-# LANGUAGE CPP #-}
{-|
Module      : System.Linux.Netlink.GeNetlink.Control
Description : This module implements the control protocol of genetlink
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides utility functions and datatypes for the genetlink control
protocol.
This has to be used by implementations of netlink families based on genetlink
to lookup their current id, since that is determined at runtime.
-}

module System.Linux.Netlink.GeNetlink.Control
  ( CtrlAttribute(..)
  , CtrlAttrMcastGroup(..)
  , CtrlPacket(..)
  , CTRLPacket
  , ctrlPacketFromGenl
  , CtrlAttrOpData(..)

  , ctrlPackettoGenl
  , getFamilyId
  , getFamilyIdS
  , getFamilyWithMulticasts
  , getFamilyWithMulticastsS
  , getMulticastGroups
  , getMulticast
  , getFamilie
  , getFamilies
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Bits ((.|.))
import Data.Serialize.Get
import Data.Serialize.Put
import Data.List (intercalate)
import Data.Map (fromList, lookup, toList, Map)
import Data.ByteString (ByteString, append, empty)
import Data.ByteString.Char8 (pack, unpack)
import Data.Word (Word16, Word32)
import Data.Maybe (fromMaybe, mapMaybe)

import Prelude hiding (lookup)

import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Constants
import System.Linux.Netlink.Constants
import System.Linux.Netlink

-- |Datatype for multicast groups as returned by the control protocol
data CtrlAttrMcastGroup = CAMG {grpName :: String, grpId :: Word32 } deriving (Eq, Show)
-- |Datatype for AttrOpData as returned by the control protocol
data CtrlAttrOpData = CAO {opId :: Word32, opFlags :: Word32 } deriving (Eq, Show)

-- |Attributes defined by the control family
data CtrlAttribute =
  CTRL_ATTR_UNSPEC       ByteString |
  CTRL_ATTR_FAMILY_ID    Word16 |
  CTRL_ATTR_FAMILY_NAME  String |
  CTRL_ATTR_VERSION      Word32 |
  CTRL_ATTR_HDRSIZE      Word32 |
  CTRL_ATTR_MAXATTR      Word32 |
  CTRL_ATTR_OPS          [CtrlAttrOpData] |
  CTRL_ATTR_MCAST_GROUPS [CtrlAttrMcastGroup] |
  CTRL_ATTR_UNKNOWN      Int ByteString
  deriving (Eq, Show)


-- |Typesafe control packet
data CtrlPacket = CtrlPacket
    {
      ctrlHeader     :: Header
    , ctrlGeHeader   :: GenlHeader
    , ctrlAttributes :: [CtrlAttribute]
    } deriving (Eq)


instance Show CtrlPacket where
  show packet = 
    show (ctrlHeader packet) ++ '\n':show (ctrlGeHeader packet) ++
    "Attrs:\n" ++ intercalate "\n" (map show (ctrlAttributes packet))


-- |typedef for control messages
type CTRLPacket = GenlPacket NoData

-- TODO fix error handling for nested arguments

--
-- Start ctrl utility
--

getW16 :: ByteString -> Maybe Word16
getW16 x = e2M (runGet getWord16host x)

getW32 :: ByteString -> Maybe Word32
getW32 x = e2M (runGet getWord32host x)

e2M :: Either a b -> Maybe b
e2M (Right x) = Just x
e2M _ = Nothing

getMcastGroupAttr :: (Int, ByteString) -> Maybe CtrlAttrMcastGroup
getMcastGroupAttr (_, x) = do
  attrs <- e2M $runGet getAttributes x
  name <- lookup eCTRL_ATTR_MCAST_GRP_NAME attrs
  fid  <- lookup eCTRL_ATTR_MCAST_GRP_ID attrs
  -- This init is ok because the name will always have the \0
  CAMG (init . unpack $ name) <$> getW32 fid

getMcastGroupAttrs :: ByteString -> Maybe [CtrlAttrMcastGroup]
getMcastGroupAttrs x = case runGet getAttributes x of
  (Right y) -> sequence $map getMcastGroupAttr $toList y
  _ -> Nothing

getOpAttr :: (Int, ByteString) -> Maybe CtrlAttrOpData
getOpAttr (_, x) = do
  attrs <- e2M $runGet getAttributes x
  oid <- getW32 =<< lookup eCTRL_ATTR_OP_ID attrs
  ofl <- getW32 =<< lookup eCTRL_ATTR_OP_FLAGS attrs
  return $ CAO oid ofl

getOpAttrs :: ByteString -> Maybe [CtrlAttrOpData]
getOpAttrs x = case runGet getAttributes x of
  (Right y) -> sequence . map getOpAttr $ toList y
  _ -> Nothing

getAttribute :: (Int, ByteString) -> CtrlAttribute
getAttribute (i, x) = fromMaybe (CTRL_ATTR_UNKNOWN i x) $makeAttribute i x

makeAttribute :: Int -> ByteString -> Maybe CtrlAttribute
makeAttribute i x
  | i == eCTRL_ATTR_UNSPEC = Just $CTRL_ATTR_UNSPEC x
  | i == eCTRL_ATTR_FAMILY_ID = fmap CTRL_ATTR_FAMILY_ID $getW16 x
  | i == eCTRL_ATTR_FAMILY_NAME = Just . CTRL_ATTR_FAMILY_NAME . init $unpack x
  | i == eCTRL_ATTR_VERSION = fmap CTRL_ATTR_VERSION $getW32 x
  | i == eCTRL_ATTR_HDRSIZE = fmap CTRL_ATTR_HDRSIZE $getW32 x
  | i == eCTRL_ATTR_MAXATTR = fmap CTRL_ATTR_MAXATTR $getW32 x
  | i == eCTRL_ATTR_OPS = fmap CTRL_ATTR_OPS $getOpAttrs x
  | i == eCTRL_ATTR_MCAST_GROUPS = fmap CTRL_ATTR_MCAST_GROUPS $getMcastGroupAttrs x
  | otherwise = Nothing


ctrlAttributesFromAttributes :: Map Int ByteString -> [CtrlAttribute]
ctrlAttributesFromAttributes = map getAttribute . toList

-- |Convert "normal" 'Packet's into typesafe 'CtrlPacket's
ctrlPacketFromGenl :: CTRLPacket -> Maybe CtrlPacket
ctrlPacketFromGenl (Packet h g attrs) = Just (CtrlPacket h (genlDataHeader g) a)
  where a = ctrlAttributesFromAttributes attrs
ctrlPacketFromGenl _ = Nothing


putW16 :: Word16 -> ByteString
putW16 x = runPut (putWord16host x)


putW32 :: Word32 -> ByteString
putW32 x = runPut (putWord32host x)


-- AttrOps and McastGroup are broken, but generally we shouldn't send these anyway
cATA :: CtrlAttribute -> (Int, ByteString)
cATA (CTRL_ATTR_UNSPEC       x) = (eCTRL_ATTR_UNSPEC      , x)
cATA (CTRL_ATTR_FAMILY_ID    x) = (eCTRL_ATTR_FAMILY_ID   , putW16 x)
cATA (CTRL_ATTR_FAMILY_NAME  x) = (eCTRL_ATTR_FAMILY_NAME , pack (x ++ "\n"))
cATA (CTRL_ATTR_VERSION      x) = (eCTRL_ATTR_VERSION     , putW32 x)
cATA (CTRL_ATTR_HDRSIZE      x) = (eCTRL_ATTR_HDRSIZE     , putW32 x)
cATA (CTRL_ATTR_MAXATTR      x) = (eCTRL_ATTR_MAXATTR     , putW32 x)
cATA (CTRL_ATTR_OPS          _) = (eCTRL_ATTR_OPS         , empty)
cATA (CTRL_ATTR_MCAST_GROUPS _) = (eCTRL_ATTR_MCAST_GROUPS, empty)
cATA (CTRL_ATTR_UNKNOWN    i x) = (i                      , x)


ctrlAttributesToAttribute :: CtrlAttribute -> (Int, ByteString)
ctrlAttributesToAttribute = cATA


-- |Convert the typesafe 'CtrPacket' into a 'CTRLPacket' so it can be sent
ctrlPackettoGenl :: CtrlPacket -> CTRLPacket
ctrlPackettoGenl (CtrlPacket h g attrs)= Packet h (GenlData g NoData) a
  where a = fromList $map ctrlAttributesToAttribute attrs


--TODO maybe set request id more sensible?
familyMcastRequest :: Word16 -> CTRLPacket
familyMcastRequest fid = let
  header = Header 16 fNLM_F_REQUEST 42 0
  geheader = GenlHeader eCTRL_CMD_GETFAMILY 0
  attrs = fromList [(eCTRL_ATTR_FAMILY_ID, runPut $putWord16host fid)] in
    Packet header (GenlData geheader NoData) attrs

familyIdRequest :: String -> CTRLPacket
familyIdRequest name = let
  header = Header 16 fNLM_F_REQUEST 33 0
  geheader = GenlHeader eCTRL_CMD_GETFAMILY 0
  attrs = fromList [(eCTRL_ATTR_FAMILY_NAME, pack name `append` pack "\0")] in
    Packet header (GenlData geheader NoData) attrs

-- |A safe version of 'getFamilyId'
getFamilyIdS :: NetlinkSocket -> String -> IO (Maybe Word16)
getFamilyIdS s m = do
  may <- getFamilyWithMulticastsS s m
  return $fmap fst may

-- |A safe version of 'getFamilyWithMulticasts'
getFamilyWithMulticastsS :: NetlinkSocket -> String -> IO (Maybe (Word16, [CtrlAttrMcastGroup]))
getFamilyWithMulticastsS s m = do
  packet <- queryOne s (familyIdRequest m)
  let ctrl = ctrlPacketFromGenl packet
  return $ makeTupl . ctrlAttributes <$> ctrl
  where getIdFromList (CTRL_ATTR_FAMILY_ID x:_) = x
        getIdFromList (_:xs) = getIdFromList xs
        getIdFromList [] = -1
        makeTupl attrs = (getIdFromList attrs, getMCFromList attrs)

-- |Get the id for a netlink family by name
getFamilyId :: NetlinkSocket -> String -> IO Word16
getFamilyId = fmap (fmap fst) . getFamilyWithMulticasts

-- |get the id and multicast groups of a netlink family by name
getFamilyWithMulticasts :: NetlinkSocket -> String -> IO (Word16, [CtrlAttrMcastGroup])
getFamilyWithMulticasts s m = do
  may <- getFamilyWithMulticastsS s m
  return $fromMaybe (error "Could not find family") may


getFamilie :: NetlinkSocket -> String -> IO (Maybe CtrlPacket)
getFamilie sock name =
  ctrlPacketFromGenl <$> queryOne sock (familyIdRequest name)

getFamilies :: NetlinkSocket -> IO [CtrlPacket]
getFamilies sock = do
  mapMaybe ctrlPacketFromGenl <$> query sock familiesRequest
  where familiesRequest = let header = Header 16 (fNLM_F_REQUEST .|. fNLM_F_ROOT .|. fNLM_F_MATCH) 33 0
                              geheader = GenlHeader eCTRL_CMD_GETFAMILY 0
                              attrs = fromList [] in
                            Packet header (GenlData geheader NoData) attrs


-- |get the mutlicast groups of a netlink family by id
getMulticastGroups :: NetlinkSocket -> Word16 -> IO [CtrlAttrMcastGroup]
getMulticastGroups sock fid = do
  packet <- queryOne sock (familyMcastRequest fid)
  let (CtrlPacket _ _ attrs) = fromMaybe (error "Got infalid family id for request") . ctrlPacketFromGenl $packet
  return $getMCFromList attrs

getMCFromList :: [CtrlAttribute] -> [CtrlAttrMcastGroup]
getMCFromList (CTRL_ATTR_MCAST_GROUPS x:_) = x
getMCFromList (_:xs) = getMCFromList xs
getMCFromList [] = []

-- |Get id of multicast group by name
getMulticast :: String -> [CtrlAttrMcastGroup] -> Maybe Word32
getMulticast _ [] = Nothing
getMulticast name (CAMG gname gid:xs) = if name == gname
   then Just gid
   else getMulticast name xs
