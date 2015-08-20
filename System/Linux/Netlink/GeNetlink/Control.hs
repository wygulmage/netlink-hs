module System.Linux.Netlink.GeNetlink.Control
(getFamilyId, CtrlAttribute(..), CtrlAttrMcastGroup(..), CtrlPacket(..),
CTRLPacket, ctrlPacketFromGenl, CtrlAttrOpData(..))
where


import Data.Serialize.Get
--import Data.Serialize.Put
import Data.Map (fromList, lookup, toList, Map)
import Control.Monad (liftM, liftM2, join)
import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack, unpack)

import Prelude hiding (lookup)

import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.Internal
import System.Linux.Netlink.GeNetlink.Constants


data CtrlAttrMcastGroup = CAMG {grpName :: String, grpId :: Int } deriving (Eq, Show)
data CtrlAttrOpData = CAO {opId :: Int, opFlags :: Int } deriving (Eq, Show)

data CtrlAttribute =
  CTRL_ATTR_UNSPEC ByteString |
  CTRL_ATTR_FAMILY_ID Int |
  CTRL_ATTR_FAMILY_NAME String |
  CTRL_ATTR_VERSION Int |
  CTRL_ATTR_HDRSIZE Int |
  CTRL_ATTR_MAXATTR Int |
  CTRL_ATTR_OPS [CtrlAttrOpData] |
  CTRL_ATTR_MCAST_GROUPS [CtrlAttrMcastGroup] |
  CTRL_ATTR_UNKNOWN Int ByteString
  deriving (Eq, Show)


data CtrlPacket = CtrlPacket
    {
      ctrlHeader     :: Header
    , ctrlGeHeader   :: GenlHeader
    , ctrlAttributes :: [CtrlAttribute]
    } deriving (Eq, Show)


type CTRLPacket = GenlPacket NoData

--
-- Start ctrl utility
--

getInt :: ByteString -> Maybe Int
getInt x = liftM fromIntegral $e2M $runGet getWord16host x

e2M :: Either a b -> Maybe b
e2M (Right x) = Just x
e2M _ = Nothing

getMcastGroupAttr :: (Int, ByteString) -> Maybe CtrlAttrMcastGroup
getMcastGroupAttr (_, x) = 
  let name = join $liftM (lookup (eCTRL_ATTR_MCAST_GRP_NAME :: Int)) attrs
      fid  = join $liftM (lookup (eCTRL_ATTR_MCAST_GRP_ID :: Int)) attrs in
    liftM2 CAMG (liftM (init . unpack) name) (getInt =<< fid)
  where attrs = e2M $runGet getAttributes x

getMcastGroupAttrs :: ByteString -> Maybe [CtrlAttrMcastGroup]
getMcastGroupAttrs x = case runGet getAttributes x of
  (Right y) -> sequence $map getMcastGroupAttr $toList y
  _ -> Nothing

getOpAttr :: (Int, ByteString) -> Maybe CtrlAttrOpData
getOpAttr (_, x) =
  let oid = join $liftM (lookup (eCTRL_ATTR_OP_ID :: Int)) attrs
      ofl  = join $liftM (lookup (eCTRL_ATTR_OP_FLAGS :: Int)) attrs in
    liftM2 CAO (getInt =<< oid) (getInt =<< ofl)
  where attrs = e2M $runGet getAttributes x

getOpAttrs :: ByteString -> Maybe [CtrlAttrOpData]
getOpAttrs x = case runGet getAttributes x of
  (Right y) -> sequence $map getOpAttr $toList y
  _ -> Nothing

getAttribute :: (Int, ByteString) -> CtrlAttribute
getAttribute (i, x) = let a = makeAttribute i x in
    case a of
      Just y  -> y
      Nothing -> CTRL_ATTR_UNKNOWN i x

makeAttribute :: Int -> ByteString -> Maybe CtrlAttribute
makeAttribute i x
  | i == eCTRL_ATTR_UNSPEC = Just $CTRL_ATTR_UNSPEC x
  | i == eCTRL_ATTR_FAMILY_ID = liftM CTRL_ATTR_FAMILY_ID $getInt x
  | i == eCTRL_ATTR_FAMILY_NAME = Just $CTRL_ATTR_FAMILY_NAME $unpack x
  | i == eCTRL_ATTR_VERSION = liftM CTRL_ATTR_VERSION $getInt x
  | i == eCTRL_ATTR_HDRSIZE = liftM CTRL_ATTR_HDRSIZE $getInt x
  | i == eCTRL_ATTR_MAXATTR = liftM CTRL_ATTR_MAXATTR $getInt x
  | i == eCTRL_ATTR_OPS = liftM CTRL_ATTR_OPS $getOpAttrs x
  | i == eCTRL_ATTR_MCAST_GROUPS = liftM CTRL_ATTR_MCAST_GROUPS $getMcastGroupAttrs x
  | otherwise = Nothing

getAttributesFromList :: [(Int, ByteString)] -> [CtrlAttribute]
getAttributesFromList xs = map getAttribute xs

ctrlAttributesFromAttributes :: Map Int ByteString -> [CtrlAttribute]
ctrlAttributesFromAttributes m = getAttributesFromList $toList m


ctrlPacketFromGenl :: CTRLPacket -> CtrlPacket
ctrlPacketFromGenl (GenericPacket h g attrs) = CtrlPacket h (genlDataHeader g) a
  where a = ctrlAttributesFromAttributes attrs
ctrlPacketFromGenl _ = undefined



familyIdRequest :: String -> CTRLPacket
familyIdRequest name = let
  header = Header 16 fNLM_F_REQUEST 33 0
  geheader = GenlHeader eCTRL_CMD_GETFAMILY 0
  attrs = fromList [(eCTRL_ATTR_FAMILY_NAME, pack name `append` pack "\0")] in
    GenericPacket header (GenlData geheader NoData) attrs

getFromPacket :: (GenlPacket b -> a) -> (GenlPacket b) -> a
getFromPacket _ (GenericError _  code _) = error (show code)
getFromPacket f x = f x

getRight :: (Either String a) -> a
getRight (Right x) = x
getRight (Left x)  = error x

getFamilyId :: NetlinkSocket -> String -> IO Int
getFamilyId sock name = do
  packet <- queryOneN sock (familyIdRequest name)
  let fid = getFromPacket get packet
  return (getId fid)
  where getId (Just x) = fromIntegral $getRight (runGet getWord16host x)
        getId Nothing  = -1
        get = \(GenericPacket _ _ a) -> lookup eCTRL_ATTR_FAMILY_ID a

