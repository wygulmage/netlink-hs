module Main
where

import System.Linux.Netlink.GeNetlink.NL80211


loopPrint :: NL80211Socket -> IO ()
loopPrint sock = do
  pack <- getPaket sock
  putStrLn $show pack
  loopPrint sock

main :: IO ()
main = do
  sock <- makeNL80211Socket
  putStrLn "Opened socket"
  joinMulticastByName sock "config"
  putStrLn "Joined multicast group"
  loopPrint sock
