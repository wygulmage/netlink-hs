module Main
where

import System.Environment (getArgs)
import System.Linux.Netlink.GeNetlink.NL80211


loopPrint :: NL80211Socket -> IO ()
loopPrint sock = do
  pack <- getPacket sock
  putStrLn $show pack
  loopPrint sock

doMain :: String -> IO ()
doMain group = do
  sock <- makeNL80211Socket
  putStrLn "Opened socket"
  joinMulticastByName sock group
  putStrLn $ "Joined multicast group: " ++ group
  loopPrint sock

printGroups :: IO ()
printGroups = do
  sock <- makeNL80211Socket
  groups <- getMulticastGroups sock
  putStrLn "This executable needs the multicast group to dump as argument"
  putStrLn "Possible multicast groups:"
  mapM_ putStrLn groups

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> doMain x
    [] -> printGroups
    _ -> putStrLn "This executable only takes the group to dump as argument"
