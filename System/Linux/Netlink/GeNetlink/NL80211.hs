module System.Linux.Netlink.GeNetlink.NL80211
where

--import Control.Applicative ((<$>))
import Data.Word (Word32, Word16, Word8)
import Control.Monad (liftM, liftM2)
import Data.Bits ((.|.))
import Data.Maybe (mapMaybe)
import Data.Serialize.Get
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)
import qualified Data.Map as M (empty, lookup)


import System.Linux.Netlink.C hiding (makeSocket)
import System.Linux.Netlink.Constants
import System.Linux.Netlink.Protocol
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control
import System.Linux.Netlink.GeNetlink.NL80211.Constants
import System.Linux.Netlink.Internal hiding (queryOneN, queryN, query, queryOne)
import qualified System.Linux.Netlink.Internal as I (queryOneN, queryN)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
data NL80211Socket = NLS NetlinkSocket Word16

type NL80211Packet = GenlPacket NoData

makeNL80211Socket :: IO NL80211Socket
makeNL80211Socket = do
  sock <- makeSocket
  fid <- getFamilyId sock "nl80211"
  return $NLS sock fid


joinMulticastByName :: NL80211Socket -> String -> IO ()
joinMulticastByName (NLS sock fid) name = do
  gid <- liftM (getMulticast name) $getMulticastGroups sock fid
  case gid of
    Nothing -> error $"Could not find \"" ++ name  ++ "\" multicast group"
    Just x -> joinMulticastGroup sock x


getRequestPacket :: Word16 -> Word8 -> Bool -> Attributes -> NL80211Packet
getRequestPacket fid cmd dump attrs =
  let header = Header (fromIntegral fid) flags 42 0
      geheader = GenlHeader cmd 0 in
    GenericPacket header (GenlData geheader NoData) attrs
  where flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST

queryOne :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO NL80211Packet
queryOne (NLS sock fid) cmd dump attrs = I.queryOneN sock packet
  where packet = getRequestPacket fid cmd dump attrs

query :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO [NL80211Packet]
query (NLS sock fid) cmd dump attrs = I.queryN sock packet
  where packet = getRequestPacket fid cmd dump attrs


parseInterface :: (ByteString, ByteString) -> (String, Word32)
parseInterface (name, ifindex) =
  (unpack name, getRight $runGet getWord32host ifindex)
  where getRight (Right x) = x
        getRight (Left err) = error err


getInterfaceList :: NL80211Socket -> IO [(String, Word32)]
getInterfaceList sock = do
  interfaces <- query sock eNL80211_CMD_GET_INTERFACE True M.empty
  return $mapMaybe (liftM parseInterface . toTuple) interfaces
  where toTuple (GenericPacket _ _ attrs) = liftM2 (,) (name attrs) (findex attrs)
        toTuple (GenericError{}) = error "Something stupid happened"
        toTuple (GenericDoneMsg _) = Nothing
        name = M.lookup eNL80211_ATTR_IFNAME
        findex = M.lookup eNL80211_ATTR_IFINDEX
