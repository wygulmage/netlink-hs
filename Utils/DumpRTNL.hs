module Main
where

import System.Linux.Netlink
import System.Linux.Netlink.Route
import System.Linux.Netlink.Constants

printLoop :: NetlinkSocket -> IO ()
printLoop sock = do
  pack <- (recvOne sock :: IO [RoutePacket])
  putStrLn $show pack
  printLoop sock


main :: IO ()
main = do
  sock <- makeSocket
  putStrLn "Opened socket"
  joinMulticastGroup sock eRTNLGRP_LINK
  joinMulticastGroup sock eRTNLGRP_NEIGH
  putStrLn "Joined multicast group"
  printLoop sock
