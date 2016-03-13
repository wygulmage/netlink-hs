module Main
where

import System.Environment (getArgs)
import System.Linux.Netlink (joinMulticastGroup, recvOne, NoData, NetlinkSocket)
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


printFamilies :: IO ()
printFamilies = do
  sock <- makeSocket
  fams <- getFamilies sock
  mapM_ printFamily fams


printFamilieDetails :: String -> IO ()
printFamilieDetails name = do
  sock <- makeSocket
  fam <- getFamilie sock name
  putStrLn $show fam


doDumpLoop :: NetlinkSocket -> IO ()
doDumpLoop sock = do
  pack <- (recvOne sock :: IO [GenlPacket NoData])
  putStrLn $show pack
  doDumpLoop sock


dumpGeneric :: String -> String -> IO ()
dumpGeneric fam grp = do
  sock <- makeSocket
  may <- getFamilyWithMulticastsS sock fam
  case may of
    Just (_, grps) -> do
      let gid = getMulticast grp grps
      case gid of
        Just x -> joinMulticastGroup sock x >> doDumpLoop sock
        Nothing -> error "Could not find the specified multicast group"
    Nothing -> error "Could not find the specified family"


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printFamilies
    [x] -> printFamilieDetails x
    [x,y] -> dumpGeneric x y
    _ -> putStrLn "This executable only takes a maximum of 2 arguments. Family and multicast group"
