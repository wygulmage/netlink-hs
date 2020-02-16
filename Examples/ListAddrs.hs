module Main
where

import Data.Bits (Bits(..))

import qualified System.Linux.Netlink as NL
import qualified System.Linux.Netlink.Simple as NLS
import qualified System.Linux.Netlink.Route as NLR
import qualified System.Linux.Netlink.Constants as NLC

queryAddrs :: NLR.RoutePacket
queryAddrs = NL.Packet
    (NL.Header NLC.eRTM_GETADDR (NLC.fNLM_F_ROOT .|. NLC.fNLM_F_MATCH .|. NLC.fNLM_F_REQUEST) 0 0)
    (NLR.NAddrMsg 0 0 0 0 0)
    mempty

handleAddr :: Either String NLR.RoutePacket -> IO ()
handleAddr (Left str) = putStrLn $ "Error decoding packet: " ++ str
handleAddr (Right pkt) = print pkt

main :: IO ()
main = do
    sock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
    NLS.nlSyncMessage sock queryAddrs $ NLS.simpleSerializeCallback handleAddr
