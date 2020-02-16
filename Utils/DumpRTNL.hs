module Main
where

import Control.Monad (forever)

import System.Linux.Netlink
import System.Linux.Netlink.Route
import System.Linux.Netlink.Constants

import qualified System.Linux.Netlink.Simple as NLS

handleMsg :: Either String RoutePacket -> IO ()
handleMsg (Left str) = putStrLn $ "Error decoding packet: " ++ str
handleMsg (Right pkt) = print pkt


main :: IO ()
main = do
  sock <- makeSocket
  putStrLn "Opened socket"
  joinMulticastGroup sock eRTNLGRP_LINK
  joinMulticastGroup sock eRTNLGRP_NEIGH
  putStrLn "Joined multicast group"
  handle <- NLS.makeSerializeNLHandle handleMsg sock
  forever (NLS.nlProcessIncoming handle)
