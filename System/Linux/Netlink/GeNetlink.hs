module System.Linux.Netlink.GeNetlink
where

import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8)



import System.Linux.Netlink.Protocol
import System.Linux.Netlink.C hiding (makeSocket)

data GenlHeader = GenlHeader
    {
      genlCmd     :: Word8
    , genlVersion :: Word8
    } deriving (Eq, Show)

instance Convertable GenlHeader where
  getPut = putGeHeader
  getGet _ = getGenlHeader

data NoData = NoData deriving (Show, Eq)

instance Convertable NoData where
  getPut _ = return ()
  getGet _ = return NoData

data GenlData a = GenlData 
    {
      genlDataHeader :: GenlHeader
    , genlDataData   :: a
    } deriving (Eq, Show)

instance Convertable a => Convertable (GenlData a) where
  getPut (GenlData h a) = putGeHeader h >> getPut a
  getGet t = do
    hdr <- getGenlHeader
    dat <- getGet t
    return $GenlData hdr dat

type GenlPacket a = GenericPacket (GenlData a)



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

makeSocket :: IO NetlinkSocket
makeSocket = makeSocketGeneric 16

