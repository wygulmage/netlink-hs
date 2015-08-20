module System.Linux.Netlink.Protocol 
    (
      Header(..)
    , Message(..)
    , Attributes
    , Packet(..)
    , GenericPacket(..)
    , NMessage(..)
    , Convertable(..)
    
    , getPacket
    , putPacket
    , getGenericPacket
    , getAttributes
    , getHeader
    , putHeader
    , putAttributes

    , getNPackets
    , putGenericPacket
    , getGenericPackets
    ) where

import Prelude hiding (length)

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Loops (whileM)
import Data.ByteString (ByteString, length)
import Data.Map (Map, fromList, toList)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CInt)

import System.Linux.Netlink.Constants

data Header = Header
    {
      messageType   :: MessageType
    , messageFlags  :: Word16
    , messageSeqNum :: Word32
    , messagePID    :: Word32
    } deriving (Eq, Show)

data Message = DoneMsg
             | ErrorMsg
    {
      errorCode    :: CInt
    , errorPacket  :: Packet
    }
             | LinkMsg
    {
      interfaceType  :: LinkType
    , interfaceIndex :: Word32
    , interfaceFlags :: Word32
    }
             | AddrMsg
    {
      addrFamily         :: AddressFamily
    , addrMaskLength     :: Word8
    , addrFlags          :: Word8
    , addrScope          :: Word8
    , addrInterfaceIndex :: Word32
    } deriving (Eq, Show)

data NMessage = NLinkMsg
    {
      ninterfaceType  :: LinkType
    , ninterfaceIndex :: Word32
    , ninterfaceFlags :: Word32
    }
             | NAddrMsg
    {
      naddrFamily         :: AddressFamily
    , naddrMaskLength     :: Word8
    , naddrFlags          :: Word8
    , naddrScope          :: Word8
    , naddrInterfaceIndex :: Word32
    } deriving (Eq, Show)

type Attributes = Map Int ByteString

class Convertable a where
  getGet :: MessageType -> Get a
  getPut :: a -> Put

instance Convertable NMessage where
  getGet = getNMessage
  getPut = putNMessage

data GenericPacket a = GenericPacket
    {
      genericPacketHeader     :: Header
    , genericPacketCustom     :: a
    , genericPacketAttributes :: Attributes
    }
        | GenericError
    {
      genericPacketHeader     :: Header
    , genericErrorcode        :: CInt
    , genericErrorPacket      :: GenericPacket a
    }
        | GenericDoneMsg
    {
      genericPacketHeader     :: Header
    }
    deriving (Eq, Show)

type NPacket = GenericPacket NMessage

data Packet = Packet
    {
      packetHeader     :: Header
    , packetMessage    :: Message
    , packetAttributes :: Attributes
    } deriving (Eq, Show)
--
-- Generic functions
--

getGenericPacket :: ByteString -> (Get a) -> Either String [a]
getGenericPacket bytes f = flip runGet bytes $ do
    pkts <- whileM (not <$> isEmpty) f
    isEmpty >>= \e -> when (not e) $ fail "Incomplete message parse"
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
getHeader = do
    isolate 16 $ do
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


--TODO maybe this should be changed
putAttributes :: Attributes -> Put
putAttributes = mapM_ putAttr . toList
  where
    putAttr (ty, value) = do
        p16 (fromIntegral $len + 4)
        p16 (fromIntegral ty)
        putByteString value
        when (len `mod` 4 /= 0) (sequence_ (replicate (4 - (len `mod` 4)) (p8 0)))
        where
          len = (length value)
--
-- Packet decoding
--
getPacket :: ByteString -> Either String [Packet]
getPacket = flip getGenericPacket getPacketInternal


getPacketInternal :: Get Packet
getPacketInternal = do
    (len, header) <- getHeader
    isolate len $ do
        msg    <- getMessage (messageType header)
        attrs <- getAttributes
        return $ Packet header msg attrs

getMessage :: MessageType -> Get Message
getMessage msgtype | msgtype == eNLMSG_DONE  = skip 4 >> return DoneMsg
                   | msgtype == eNLMSG_ERROR = getMessageError
                   | msgtype == eRTM_NEWLINK = getMessageLink
                   | msgtype == eRTM_GETLINK = getMessageLink
                   | msgtype == eRTM_DELLINK = getMessageLink
                   | msgtype == eRTM_NEWADDR = getMessageAddr
                   | msgtype == eRTM_GETADDR = getMessageAddr
                   | msgtype == eRTM_DELADDR = getMessageAddr
                   | otherwise               =
                       error $ "Can't decode message " ++ show msgtype

getMessageError :: Get Message
getMessageError = do
    code <- abs . fromIntegral <$> g32
    pkt  <- getPacketInternal
    return $ ErrorMsg code pkt

getMessageLink :: Get Message
getMessageLink = do
    skip 2
    ty    <- fromIntegral <$> g16
    idx   <- g32
    flags <- g32
    skip 4
    return $ LinkMsg ty idx flags

getMessageAddr :: Get Message
getMessageAddr = do
    fam <- fromIntegral <$> g8
    maskLen <- g8
    flags <- g8
    scope <- fromIntegral <$> g8
    idx <- g32
    return $ AddrMsg fam maskLen flags scope idx
--
-- New generic stuffs
--

getNMessage :: MessageType -> Get NMessage
getNMessage msgtype | msgtype == eRTM_NEWLINK = getNMessageLink
                    | msgtype == eRTM_GETLINK = getNMessageLink
                    | msgtype == eRTM_DELLINK = getNMessageLink
                    | msgtype == eRTM_NEWADDR = getNMessageAddr
                    | msgtype == eRTM_GETADDR = getNMessageAddr
                    | msgtype == eRTM_DELADDR = getNMessageAddr
                    | otherwise               =
                        error $ "Can't decode message " ++ show msgtype

getNMessageLink :: Get NMessage
getNMessageLink = do
    skip 2
    ty    <- fromIntegral <$> g16
    idx   <- g32
    flags <- g32
    skip 4
    return $ NLinkMsg ty idx flags

getNMessageAddr :: Get NMessage
getNMessageAddr = do
    fam <- fromIntegral <$> g8
    maskLen <- g8
    flags <- g8
    scope <- fromIntegral <$> g8
    idx <- g32
    return $ NAddrMsg fam maskLen flags scope idx

putNMessage :: NMessage -> Put
putNMessage (NLinkMsg ty idx flags) = do
    p8 eAF_UNSPEC >> p8 0
    p16 (fromIntegral ty)
    p32 idx
    p32 flags
    p32 0xFFFFFFFF
putNMessage (NAddrMsg fam maskLen flags scope idx) = do
    p8 (fromIntegral fam)
    p8 maskLen
    p8 flags
    p8 (fromIntegral scope)
    p32 idx

putGenericPacket :: (Convertable a, Eq a, Show a) => GenericPacket a -> [ByteString]
putGenericPacket (GenericPacket header custom attributes) =
  let attrs = runPut $putAttributes attributes
      cus   = runPut $getPut custom
      hdr   = runPut $putHeader (length attrs + length cus + 16) header
  in [hdr, cus, attrs]
putGenericPacket _ = error "Cannot convert this for transmission"



getGenPacket :: (Convertable a, Eq a, Show a) => Get (GenericPacket a)
getGenPacket = do
    (len, header) <- getHeader
    isolate len $ do
        msg    <- getGet (messageType header)
        attrs  <- getAttributes
        return $ GenericPacket header msg attrs


getGenericPackets :: (Convertable a, Eq a, Show a) => ByteString -> Either String [GenericPacket a]
getGenericPackets bytes = flip runGet bytes $ do
    pkts <- whileM (not <$> isEmpty) getGenPacket
    isEmpty >>= \e -> when (not e) $ fail "Incomplete message parse"
    return pkts

getNPackets :: ByteString -> Either String [NPacket]
getNPackets = getGenericPackets

--
-- Packet serialization
--
putPacket :: Packet -> [ByteString]
putPacket (Packet header message attributes) =
    let attrs  = runPut $ putAttributes attributes
        msg    = runPut $ putMessage message
        hdr = runPut $ putHeader (length msg + length attrs + 16) header
    in [hdr, msg, attrs]


putMessage :: Message -> Put
putMessage DoneMsg = p32 0
putMessage (LinkMsg ty idx flags) = do
    p8 eAF_UNSPEC >> p8 0
    p16 (fromIntegral ty)
    p32 idx
    p32 flags
    p32 0xFFFFFFFF
putMessage (AddrMsg fam maskLen flags scope idx) = do
    p8 (fromIntegral fam)
    p8 maskLen
    p8 flags
    p8 (fromIntegral scope)
    p32 idx
putMessage _ = error "Can't transmit this message to the kernel."


--
-- Helpers
--
p8 :: Word8 -> Put
p8  = putWord8
p16 :: Word16 -> Put
p16 = putWord16host
p32 :: Word32 -> Put
p32 = putWord32host

g8 :: Get Word8
g8  = getWord8
g16:: Get Word16
g16 = getWord16host
g32 :: Get Word32
g32 = getWord32host
