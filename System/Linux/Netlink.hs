module System.Linux.Netlink
(   Header(..)
  , Attributes
  , Packet(..)
  , Convertable(..)
  , NetlinkSocket

  , getPacket
  , getAttributes
  , getHeader
  , putHeader
  , putAttributes
  , putPacket
  , getPackets

  , makeSocket
  , makeSocketGeneric
  , closeSocket
  , joinMulticastGroup

  , query
  , queryOne
  , recvOne
)
where

import Control.Applicative ((<$>))
import Control.Monad (when, replicateM_, unless)
import Control.Monad.Loops (whileM)
import Data.Bits (Bits, (.&.))
import qualified Data.ByteString as BS (length)
import Data.ByteString (ByteString)
import Data.Map (Map, fromList, toList)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CInt)

import qualified System.Linux.Netlink.C as C
import System.Linux.Netlink.Helpers
import System.Linux.Netlink.Constants

--Generic protocol stuff


data Header = Header
    {
      messageType   :: MessageType
    , messageFlags  :: Word16
    , messageSeqNum :: Word32
    , messagePID    :: Word32
    } deriving (Eq, Show)


type Attributes = Map Int ByteString


class Convertable a where
  getGet :: MessageType -> Get a
  getPut :: a -> Put


data Packet a = Packet
    {
      packetHeader     :: Header
    , packetCustom     :: a
    , packetAttributes :: Attributes
    }
        | ErrorMsg
    {
      packetHeader     :: Header
    , packetError      :: CInt
    , errorPacket      :: Packet a
    }
        | DoneMsg
    {
      packetHeader     :: Header
    }
    deriving (Eq, Show)

getPacket :: ByteString -> Get a -> Either String [a]
getPacket bytes f = flip runGet bytes $ do
    pkts <- whileM (not <$> isEmpty) f
    isEmpty >>= \e -> unless e $ fail "Incomplete message parse"
    return pkts


getAttributes :: Get Attributes
getAttributes = fromList <$> whileM (not <$> isEmpty) getSingleAttribute

getSingleAttribute :: Get (Int, ByteString)
getSingleAttribute = do
    len <- fromIntegral <$> g16
    ty <- fromIntegral <$> g16
    val <- getByteString (len - 4)
    isEmpty >>= \e -> when (not e && len `mod` 4 /= 0) $ skip (4 - (len `mod` 4))
    return (ty, val)


getHeader :: Get (Int, Header)
getHeader = isolate 16 $ do
      len <- fromIntegral <$> g32
      ty     <- fromIntegral <$> g16
      flags  <- fromIntegral <$> g16
      seqnum <- g32
      pid    <- g32
      return (len - 16, Header ty flags seqnum pid)

putHeader :: Int -> Header -> Put
putHeader len (Header ty flags seqnum pid) = do
    p32 (fromIntegral len)
    p16 (fromIntegral ty)
    p16 (fromIntegral flags)
    p32 seqnum
    p32 pid


putAttributes :: Attributes -> Put
putAttributes = mapM_ putAttr . toList
  where
    putAttr (ty, value) = do
        p16 (fromIntegral $BS.length value + 4)
        p16 (fromIntegral ty)
        putByteString value
        when (BS.length value `mod` 4 /= 0) $replicateM_ (4 - (BS.length value `mod` 4)) (p8 0)

putPacket :: (Convertable a, Eq a, Show a) => Packet a -> [ByteString]
putPacket (Packet header custom attributes) =
  let attrs = runPut $putAttributes attributes
      cus   = runPut $getPut custom
      hdr   = runPut $putHeader (BS.length attrs + BS.length cus + 16) header
  in [hdr, cus, attrs]
putPacket _ = error "Cannot convert this for transmission"


getError :: (Convertable a, Eq a, Show a) => Header -> Get (Packet a)
getError hdr = do
  code <- fromIntegral <$> getWord32host
  packet <- getGenPacket
  return $ErrorMsg hdr code packet

getGenPacketContent :: (Convertable a, Eq a, Show a) => Header -> Get (Packet a)
getGenPacketContent hdr
  | messageType hdr == eNLMSG_DONE  = skip 4 >> return (DoneMsg hdr)
  | messageType hdr == eNLMSG_ERROR = getError hdr
  | otherwise  = do
      msg    <- getGet (messageType hdr)
      attrs  <- getAttributes
      return $ Packet hdr msg attrs

getGenPacket :: (Convertable a, Eq a, Show a) => Get (Packet a)
getGenPacket = do
    (len, header) <- getHeader
    isolate len $ getGenPacketContent header

getPackets :: (Convertable a, Eq a, Show a) => ByteString -> Either String [Packet a]
getPackets bytes = flip runGet bytes $ do
    pkts <- whileM (not <$> isEmpty) getGenPacket
    isEmpty >>= \e -> unless e $ fail "Incomplete message parse"
    return pkts

-- Wrapper for the functions defined in C
newtype NetlinkSocket = NS CInt

makeSocket :: IO NetlinkSocket
makeSocket = NS <$> C.makeSocket

makeSocketGeneric :: Int -> IO NetlinkSocket
makeSocketGeneric = fmap NS . C.makeSocketGeneric

sendmsg :: NetlinkSocket -> [ByteString] -> IO ()
sendmsg (NS fd) = C.sendmsg fd

recvmsg :: NetlinkSocket -> Int -> IO ByteString
recvmsg (NS fd) = C.recvmsg fd

closeSocket :: NetlinkSocket -> IO ()
closeSocket (NS fd) = C.closeSocket fd

joinMulticastGroup :: NetlinkSocket -> Word32 -> IO ()
joinMulticastGroup (NS fd) = C.joinMulticastGroup fd



-- generic query functions
query :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO [Packet a]
query sock req = do
    sendmsg sock (putPacket req)
    recvMulti sock

queryOne :: (Convertable a, Eq a, Show a) => NetlinkSocket -> Packet a -> IO (Packet a)
queryOne sock req = do
    sendmsg sock (putPacket req)
    pkts <- recvMulti sock
    let len = length pkts
    when (len /= 1) $ fail ("Expected one packet, received " ++ show len)
    return $ head pkts

recvMulti :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Packet a]
recvMulti sock = do
    pkts <- recvOne sock
    if isMulti (head pkts)
        then if isDone (last pkts)
             then return $ init pkts
             else (pkts ++) <$> recvMulti sock
        else return pkts
  where
    isMulti = isFlagSet fNLM_F_MULTI . messageFlags . packetHeader
    isDone  = (== eNLMSG_DONE) . messageType . packetHeader

recvOne :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [Packet a]
recvOne sock = recvmsg sock bufferSize >>= \b -> case getPackets b of
    Left err   -> fail err
    Right pkts -> return pkts


isFlagSet :: Bits a => a -> a -> Bool
isFlagSet f v = (f .&. v) == f

bufferSize :: Num a => a
bufferSize = 8192
