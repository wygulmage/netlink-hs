module System.Linux.Netlink.Internal
    (
      query
    , queryOne

    , queryN
    , queryOneN
      
    , module X
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (Bits, (.&.))

import System.Linux.Netlink.Attributes as X
import System.Linux.Netlink.C as X
import System.Linux.Netlink.Constants as X
import System.Linux.Netlink.Protocol as X

queryN :: (Convertable a, Eq a, Show a) => NetlinkSocket -> GenericPacket a -> IO [GenericPacket a]
queryN sock req = do
    sendmsg sock (putGenericPacket req)
    recvMultiN sock

queryOneN :: (Convertable a, Eq a, Show a) => NetlinkSocket -> GenericPacket a -> IO (GenericPacket a)
queryOneN sock req = do
    sendmsg sock (putGenericPacket req)
    pkts <- recvMultiN sock
    let len = length pkts
    when (len /= 1) $ fail ("Expected one packet, received " ++ show len)
    return $ head pkts

recvMultiN :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [GenericPacket a]
recvMultiN sock = do
    pkts <- recvOneN sock
    if isMulti (head pkts)
        then if isDone (last pkts)
             then return $ init pkts
             else (pkts ++) <$> recvMultiN sock
        else return pkts
  where
    isMulti = isFlagSet fNLM_F_MULTI . messageFlags . genericPacketHeader
    isDone  = (== eNLMSG_DONE) . messageType . genericPacketHeader

recvOneN :: (Convertable a, Eq a, Show a) => NetlinkSocket -> IO [GenericPacket a]
recvOneN sock = recvmsg sock bufferSize >>= \b -> case getGenericPackets b of
    Left err   -> fail err
    Right pkts -> return pkts


--
-- Old Stuff
--


query :: NetlinkSocket -> Packet -> IO [Packet]
query sock req = do
    sendmsg sock (putPacket req)
    recvMulti sock

queryOne :: NetlinkSocket -> Packet -> IO Packet
queryOne sock req = do
    sendmsg sock (putPacket req)
    pkts <- recvMulti sock
    let len = length pkts
    when (len /= 1) $ fail ("Expected one packet, received " ++ show len)
    return $ head pkts

recvMulti :: NetlinkSocket -> IO [Packet]
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

recvOne :: NetlinkSocket -> IO [Packet]
recvOne sock = recvmsg sock bufferSize >>= \b -> case getPacket b of
    Left err   -> fail err
    Right pkts -> return pkts

isFlagSet :: Bits a => a -> a -> Bool
isFlagSet f v = (f .&. v) == f

bufferSize :: Num a => a
bufferSize = 8192
