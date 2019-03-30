module Main
where

import Data.Bits (Bits(..))

import qualified System.Linux.Netlink as NL
import qualified System.Linux.Netlink.Simple as NLS
import qualified System.Linux.Netlink.Route as NLR
import qualified System.Linux.Netlink.Constants as NLC

import Data.Serialize

query :: NLR.RoutePacket
query = NL.Packet
    (NL.Header NLC.eRTM_GETLINK (NLC.fNLM_F_ROOT .|. NLC.fNLM_F_MATCH .|. NLC.fNLM_F_REQUEST) 0 0)
    (NLR.NLinkMsg 0 0 0)
    mempty

handlePacket :: Either String NLR.RoutePacket -> IO ()
handlePacket (Left str) = putStrLn $ "Error decoding packet: " ++ str
handlePacket (Right pkt) = case NLR.getLinkName $ NL.packetAttributes pkt of
    Nothing -> putStrLn $ "Got packet without LinkName. This shouldn't happen"
    Just name -> putStrLn name

main :: IO ()
main = do
    sock <- NLS.makeNLHandle (const $ pure ()) =<< NL.makeSocket
    let cb = NLS.NLCallback (pure ()) (handlePacket . runGet NL.getGenPacket)
    NLS.nlPostMessage sock query cb
    NLS.nlWaitCurrent sock
