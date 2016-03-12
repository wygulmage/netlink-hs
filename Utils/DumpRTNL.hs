module Main
where

import System.Linux.Netlink
import System.Linux.Netlink.Route

printLoop :: NetlinkSocket -> IO ()
printLoop sock = do
  pack <- (recvOne sock :: IO [RoutePacket])
  putStrLn $show pack
  printLoop sock


main :: IO ()
main = do
  sock <- makeSocket
  putStrLn "Opened socket"
  joinMulticastGroup sock 1
  putStrLn "Joined multicast group"
  printLoop sock
