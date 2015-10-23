{-# LANGUAGE CPP #-}
module System.Linux.Netlink.GeNetlink.Control
(getFamilyId, CtrlAttribute(..), CtrlAttrMcastGroup(..), CtrlPacket(..),
CTRLPacket, ctrlPacketFromGenl, CtrlAttrOpData(..), ctrlPackettoGenl,
getFamilyWithMulticasts, getMulticastGroups, getMulticast)
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*))
#endif

import Data.Serialize.Get
import Data.Serialize.Put
import Data.Map (fromList, lookup, toList, Map)
import Control.Monad (liftM, liftM2, join)
import Data.ByteString (ByteString, append, empty)
import Data.ByteString.Char8 (pack, unpack)
import Data.Word (Word16, Word32)
import Data.Maybe (fromMaybe)

import Prelude hiding (lookup)

import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Constants
import System.Linux.Netlink.Constants
import System.Linux.Netlink


data CtrlAttrMcastGroup = CAMG {grpName :: String, grpId :: Word32 } deriving (Eq, Show)
data CtrlAttrOpData = CAO {opId :: Word32, opFlags :: Word32 } deriving (Eq, Show)

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


data CtrlPacket = CtrlPacket
    {
      ctrlHeader     :: Header
    , ctrlGeHeader   :: GenlHeader
    , ctrlAttributes :: [CtrlAttribute]
    } deriving (Eq, Show)


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
getMcastGroupAttr (_, x) = 
  let name = join $liftM (lookup (eCTRL_ATTR_MCAST_GRP_NAME :: Int)) attrs
      fid  = join $liftM (lookup (eCTRL_ATTR_MCAST_GRP_ID :: Int)) attrs in
    liftM2 CAMG (fmap (init . unpack) name) (getW32 =<< fid)
  where attrs = e2M $runGet getAttributes x

getMcastGroupAttrs :: ByteString -> Maybe [CtrlAttrMcastGroup]
getMcastGroupAttrs x = case runGet getAttributes x of
  (Right y) -> sequence $map getMcastGroupAttr $toList y
  _ -> Nothing

getOpAttr :: (Int, ByteString) -> Maybe CtrlAttrOpData
getOpAttr (_, x) =
  let oid = join $liftM (lookup (eCTRL_ATTR_OP_ID :: Int)) attrs
      ofl  = join $liftM (lookup (eCTRL_ATTR_OP_FLAGS :: Int)) attrs in
    liftM2 CAO (getW32 =<< oid) (getW32 =<< ofl)
  where attrs = e2M $runGet getAttributes x

getOpAttrs :: ByteString -> Maybe [CtrlAttrOpData]
getOpAttrs x = case runGet getAttributes x of
  (Right y) -> sequence $map getOpAttr $toList y
  _ -> Nothing

getAttribute :: (Int, ByteString) -> CtrlAttribute
getAttribute (i, x) = fromMaybe (CTRL_ATTR_UNKNOWN i x) $makeAttribute i x

makeAttribute :: Int -> ByteString -> Maybe CtrlAttribute
makeAttribute i x
  | i == eCTRL_ATTR_UNSPEC = Just $CTRL_ATTR_UNSPEC x
  | i == eCTRL_ATTR_FAMILY_ID = fmap CTRL_ATTR_FAMILY_ID $getW16 x
  | i == eCTRL_ATTR_FAMILY_NAME = Just $CTRL_ATTR_FAMILY_NAME $unpack x
  | i == eCTRL_ATTR_VERSION = fmap CTRL_ATTR_VERSION $getW32 x
  | i == eCTRL_ATTR_HDRSIZE = fmap CTRL_ATTR_HDRSIZE $getW32 x
  | i == eCTRL_ATTR_MAXATTR = fmap CTRL_ATTR_MAXATTR $getW32 x
  | i == eCTRL_ATTR_OPS = fmap CTRL_ATTR_OPS $getOpAttrs x
  | i == eCTRL_ATTR_MCAST_GROUPS = fmap CTRL_ATTR_MCAST_GROUPS $getMcastGroupAttrs x
  | otherwise = Nothing


ctrlAttributesFromAttributes :: Map Int ByteString -> [CtrlAttribute]
ctrlAttributesFromAttributes = map getAttribute . toList

ctrlPacketFromGenl :: CTRLPacket -> CtrlPacket
ctrlPacketFromGenl (Packet h g attrs) = CtrlPacket h (genlDataHeader g) a
  where a = ctrlAttributesFromAttributes attrs
ctrlPacketFromGenl _ = undefined

putW16 :: Word16 -> ByteString
putW16 x = runPut (putWord16host x)

putW32 :: Word32 -> ByteString
putW32 x = runPut (putWord32host x)

--TODO maybe add those two, but generally we shouldn't send these anyway
cATA :: CtrlAttribute -> (Int, ByteString)
cATA (CTRL_ATTR_UNSPEC       x) = (eCTRL_ATTR_UNSPEC      , x)
cATA (CTRL_ATTR_FAMILY_ID    x) = (eCTRL_ATTR_FAMILY_ID   , putW16 x)
cATA (CTRL_ATTR_FAMILY_NAME  x) = (eCTRL_ATTR_FAMILY_NAME , pack x)
cATA (CTRL_ATTR_VERSION      x) = (eCTRL_ATTR_VERSION     , putW32 x)
cATA (CTRL_ATTR_HDRSIZE      x) = (eCTRL_ATTR_HDRSIZE     , putW32 x)
cATA (CTRL_ATTR_MAXATTR      x) = (eCTRL_ATTR_MAXATTR     , putW32 x)
cATA (CTRL_ATTR_OPS          _) = (eCTRL_ATTR_OPS         , empty)
cATA (CTRL_ATTR_MCAST_GROUPS _) = (eCTRL_ATTR_MCAST_GROUPS, empty)
cATA (CTRL_ATTR_UNKNOWN    i x) = (i                      , x)

ctrlAttributesToAttribute :: CtrlAttribute -> (Int, ByteString)
ctrlAttributesToAttribute = cATA


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

getFamilyId :: NetlinkSocket -> String -> IO Word16
getFamilyId s m = fmap fst (getFamilyWithMulticasts s m)

getFamilyWithMulticasts :: NetlinkSocket -> String -> IO (Word16, [CtrlAttrMcastGroup])
getFamilyWithMulticasts sock name = do
  (CtrlPacket _ _ attrs) <- ctrlPacketFromGenl <$> queryOne sock (familyIdRequest name)
  return (getIdFromList attrs, getMCFromList attrs)
  where getIdFromList (CTRL_ATTR_FAMILY_ID x:_) = x
        getIdFromList (_:xs) = getIdFromList xs
        getIdFromList [] = -1

getMulticastGroups :: NetlinkSocket -> Word16 -> IO [CtrlAttrMcastGroup]
getMulticastGroups sock fid = do
  (CtrlPacket _ _ attrs) <- ctrlPacketFromGenl <$> queryOne sock (familyMcastRequest fid)
  return $getMCFromList attrs

getMCFromList :: [CtrlAttribute] -> [CtrlAttrMcastGroup]
getMCFromList (CTRL_ATTR_MCAST_GROUPS x:_) = x
getMCFromList (_:xs) = getMCFromList xs
getMCFromList [] = []

getMulticast :: String -> [CtrlAttrMcastGroup] -> Maybe Word32
getMulticast _ [] = Nothing
getMulticast name (CAMG gname gid:xs) = if name == gname
   then Just gid
   else getMulticast name xs
