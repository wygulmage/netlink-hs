module Main
where

import System.Linux.Netlink.GeNetlink.NL80211

main :: IO ()
main = do
  sock <- makeNL80211Socket
  interfaces <- getInterfaceList sock

  print $show interfaces
