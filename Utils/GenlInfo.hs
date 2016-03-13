module Main
where

import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control

printMutlicastGroup :: CtrlAttrMcastGroup -> IO ()
printMutlicastGroup grp =
  putStr "  " >> putStrLn (grpName grp)

printFamily :: CtrlPacket -> IO ()
printFamily pack = do
  let [CTRL_ATTR_FAMILY_NAME name] = filter isName (ctrlAttributes pack)
      xs = filter isMGroup (ctrlAttributes pack)
  putStr name
  putStr ":"
  case xs of
    [CTRL_ATTR_MCAST_GROUPS grp] -> mapM_ printMutlicastGroup grp
    _ -> putStrLn " No multicast groups"
  where isName (CTRL_ATTR_FAMILY_NAME _) = True
        isName _ = False
        isMGroup (CTRL_ATTR_MCAST_GROUPS _) = True
        isMGroup _ = False

main :: IO ()
main = do
  sock <- makeSocket
  stuffz <- getFamilies sock
  mapM_ printFamily stuffz
