module System.Linux.Netlink.GeNetlink
where

import Control.Applicative ((<$>))
import Control.Monad (when, liftM, liftM2, join)
import Data.Bits (Bits, (.&.))
import Data.ByteString (ByteString, length, append)
import Data.ByteString.Char8 (pack, unpack)
import Data.Map (fromList, lookup, toList, Map)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)


import Prelude hiding (length, lookup)
import qualified Prelude as P (length) 

import System.Linux.Netlink.Protocol
import System.Linux.Netlink.Constants
import System.Linux.Netlink.C hiding (makeSocket)
import System.Linux.Netlink.GeNetlink.Constants

data CtrlAttrMcastGroup = CAMG {grpName :: String, grpId :: Int } deriving (Eq, Show)

data CtrlAttribute =
  CTRL_ATTR_UNSPEC ByteString |
  CTRL_ATTR_FAMILY_ID Int |
  CTRL_ATTR_FAMILY_NAME String |
  CTRL_ATTR_VERSION Int |
  CTRL_ATTR_HDRSIZE Int |
  CTRL_ATTR_MAXATTR Int |
  CTRL_ATTR_OPS ByteString |
  CTRL_ATTR_MCAST_GROUPS [CtrlAttrMcastGroup]
  deriving (Eq, Show)

data CtrlPacket = CtrlPacket
    {
      ctrlHeader     :: Header
    , ctrlGeHeader   :: GenlHeader
    , ctrlAttributes :: [CtrlAttribute]
    } deriving (Eq, Show)

data GenlHeader = GenlHeader
    {
      genlCmd     :: Word8
    , genlVersion :: Word8
    } deriving (Eq, Show)

data GenlError = GenlError
    {
      genlErrCode     :: CInt
    , genlErrPacket   :: GenlPacket
    } deriving (Eq, Show)

data GenlPacket = GenlPacket
    {
      genlPacketHeader :: Header
    , genlHeader       :: GenlHeader
    , geAttributes     :: Attributes
    }
        | GenlErrPacket
    {
      genlPacketHeader :: Header
    , genlErrPacketErr :: GenlError
    }
        | GenlDonePacket
    {
      genlPacketHeader :: Header
    } deriving (Eq, Show)

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

getAttribute :: (Int, ByteString) -> CtrlAttribute
getAttribute (i, x) = let a = makeAttribute i x in
    case a of
      Just y  -> y
      Nothing -> CTRL_ATTR_UNSPEC x

makeAttribute :: Int -> ByteString -> Maybe CtrlAttribute
makeAttribute i x
  | i == eCTRL_ATTR_FAMILY_ID = liftM CTRL_ATTR_FAMILY_ID $getInt x
  | i == eCTRL_ATTR_FAMILY_NAME = Just $CTRL_ATTR_FAMILY_NAME $unpack x
  | i == eCTRL_ATTR_VERSION = liftM CTRL_ATTR_VERSION $getInt x
  | i == eCTRL_ATTR_HDRSIZE = liftM CTRL_ATTR_HDRSIZE $getInt x
  | i == eCTRL_ATTR_MAXATTR = liftM CTRL_ATTR_MAXATTR $getInt x
  | i == eCTRL_ATTR_OPS = Just $CTRL_ATTR_OPS x
  | i == eCTRL_ATTR_MCAST_GROUPS = liftM CTRL_ATTR_MCAST_GROUPS $getMcastGroupAttrs x
  | otherwise = Nothing


getAttributesFromList :: [(Int, ByteString)] -> [CtrlAttribute]
getAttributesFromList xs = map getAttribute xs

ctrlAttributesFromAttributes :: Map Int ByteString -> [CtrlAttribute]
ctrlAttributesFromAttributes m = getAttributesFromList $toList m


ctrlPacketFromGenl :: GenlPacket -> CtrlPacket
ctrlPacketFromGenl (GenlPacket h g attrs) = CtrlPacket h g a
  where a = ctrlAttributesFromAttributes attrs
ctrlPacketFromGenl _ = undefined




getGePacket :: ByteString -> Either String [GenlPacket]
getGePacket = flip getGenericPacket getGePacketInternal


getErr :: Get GenlError
getErr = do
    err <- fromIntegral <$> getWord32host
    packet <- getGePacketInternal
    return $GenlError err packet


getGePacketInternal :: Get GenlPacket
getGePacketInternal = do
    (len, header) <- getHeader
    getGePacketFromType (messageType header) len header


getGePacketFromType :: MessageType -> Int -> Header -> Get GenlPacket
getGePacketFromType t len header
    | t == eNLMSG_ERROR = GenlErrPacket header <$> getErr
    | t == eNLMSG_DONE  = skip 4 >> (return $GenlDonePacket header)
    | otherwise = isolate len $ do
        gehdr <- getGenlHeader
        attrs <- getAttributes
        return $GenlPacket header gehdr attrs


getGenlHeader :: Get GenlHeader
getGenlHeader = do
    cmd <- getWord8
    version <- getWord8
    _ <- getWord16host
    return $GenlHeader cmd version


putGeHeader :: GenlHeader -> Put
putGeHeader gehdr = do
  putWord8 $ genlCmd gehdr
  putWord8 $ genlVersion gehdr
  putWord16host 0


putGePacket :: GenlPacket -> [ByteString]
putGePacket (GenlPacket header geheader attributes) =
  let attrs = runPut $ putAttributes attributes
      gehdr = runPut $ putGeHeader geheader
      hdr   = runPut $ putHeader (length attrs + length gehdr + 16) header
  in [hdr, gehdr, attrs]
putGePacket _ = error "Cannot serialize other this message type"


--
-- Start ctrl utility
--

makeSocket :: IO NetlinkSocket
makeSocket = makeSocketGeneric 16

familyIdRequest :: String -> GenlPacket
familyIdRequest name = let
  header = Header 16 fNLM_F_REQUEST 33 0
  geheader = GenlHeader eCTRL_CMD_GETFAMILY 0
  attrs = fromList [(eCTRL_ATTR_FAMILY_NAME, pack name `append` pack "\0")] in
    GenlPacket header geheader attrs

getFromPacket :: (GenlPacket -> a) -> GenlPacket -> IO a
getFromPacket _ (GenlErrPacket _ (GenlError code _)) = fail (show code)
getFromPacket f x = return $f x

getFamilyId :: NetlinkSocket -> String -> IO Int
getFamilyId sock name = do
  packet <- queryOne sock (putGePacket (familyIdRequest name))
  putStrLn $show $ctrlPacketFromGenl packet
  fid <- getFromPacket (\p -> lookup eCTRL_ATTR_FAMILY_ID (geAttributes p)) packet
  return (getId fid)
  where getId (Just x) = fromIntegral $getRight (runGet getWord16host x)
        getId Nothing  = -1


-- Testing stuff, this is really really bad

getRight :: Either String Word16 -> Word16
getRight (Right x) = x
getRight _ = -1



-- this has to be moved later on
--TODO fix this mess


query :: NetlinkSocket -> [ByteString] -> IO [GenlPacket]
query sock req = do
    sendmsg sock req
    recvMulti sock

queryOne :: NetlinkSocket -> [ByteString] -> IO GenlPacket
queryOne sock req = do
    sendmsg sock req
    pkts <- recvMulti sock
    let len = P.length pkts
    when (len /= 1) $ fail ("Expected one packet, received " ++ show len)
    return $ head pkts

recvMulti :: NetlinkSocket -> IO [GenlPacket]
recvMulti sock = do
    pkts <- recvOne sock
    if isMulti (head pkts)
        then if isDone (last pkts)
             then return $ init pkts
             else (pkts ++) <$> recvMulti sock
        else return pkts
  where
    isMulti = isFlagSet fNLM_F_MULTI . messageFlags . genlPacketHeader
    isDone  = (== eNLMSG_DONE) . messageType . genlPacketHeader

recvOne :: NetlinkSocket -> IO [GenlPacket]
recvOne sock = recvmsg sock bufferSize >>= \b -> do 
    case (getGePacket b) of
      Left err   -> fail err
      Right pkts -> return pkts

isFlagSet :: Bits a => a -> a -> Bool
isFlagSet f v = (f .&. v) == f

bufferSize :: Num a => a
bufferSize = 8192
