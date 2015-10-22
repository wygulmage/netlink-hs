module Main
where

import Data.Maybe (mapMaybe)
import Text.Hexdump

import System.Linux.Netlink.GeNetlink.NL80211



import qualified Data.Map as M

fori :: IO () -> Int -> IO ()
fori a 0 = a
fori a i = a >> fori a (i-1)

for :: Int -> IO () -> IO ()
for = flip fori

main :: IO ()
main = do
  sock <- makeNL80211Socket
  interfaces <- getInterfaceList sock
  putStrLn $show interfaces
  
  --for 5 (getScanResults sock (snd $head interfaces) >> return ())
  scans <- getScanResults sock (snd $head interfaces)
  sequence_ $map (putStrLn . show) scans

  current <- getConnectedWifi sock (snd $head interfaces)
  cur <- getConnectedWifi sock (snd $head interfaces)
  putStrLn $show current

  let eids = mapMaybe getWifiAttributes current
  putStrLn ("current: " ++ (show eids))

  joinMulticastByName sock "mlme"

  packet <- getPaket sock

  putStrLn $show packet

  --putStrLn $ prettyHex packet
  --cur <- getConnectedWifi sock (snd $head interfaces)
  --let eid = mapMaybe getWifiAttributes current
  --let ssid = map (M.lookup 0) eid
  --putStrLn $show ssid
