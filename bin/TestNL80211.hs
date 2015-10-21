module Main
where

import System.Linux.Netlink.GeNetlink.NL80211

main :: IO ()
main = do
  sock <- makeNL80211Socket
  interfaces <- getInterfaceList sock
  putStrLn $show interfaces
  
  scans <- getScanResults sock (snd $head interfaces)
  sequence_ $map (putStrLn . show) scans

  current <- getConnectedWifi sock (snd $head interfaces)
  putStrLn $show current
