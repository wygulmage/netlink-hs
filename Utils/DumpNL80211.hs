module Main
where

import Control.Monad (forever)

import System.Environment (getArgs)
import System.Linux.Netlink.GeNetlink.NL80211

import qualified System.Linux.Netlink.Simple as NLS

handleMsg :: Either String NL80211Packet -> IO ()
handleMsg (Left str) = putStrLn $ "Error decoding packet: " ++ str
handleMsg (Right pkt) = print pkt

doMain :: String -> IO ()
doMain group = do
  sock <- makeNL80211Socket
  putStrLn "Opened socket"
  joinMulticastByName sock group
  putStrLn $ "Joined multicast group: " ++ group
  handle <- NLS.makeSerializeNLHandle handleMsg (getNLSocket sock)
  forever $ NLS.nlProcessIncoming handle

printGroups :: IO ()
printGroups = do
  putStrLn "This executable needs the multicast group to dump as argument"
  putStrLn "Possible multicast groups:"
  mapM_ putStrLn =<< getMulticastGroups =<< makeNL80211Socket

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> doMain x
    [] -> printGroups
    _ -> putStrLn "This executable only takes the group to dump as argument"
