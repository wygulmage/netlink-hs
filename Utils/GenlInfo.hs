{-# LANGUAGE LambdaCase #-}
module Main
where

import Control.Monad (forever)

import System.Environment (getArgs)
import System.Linux.Netlink (joinMulticastGroup, NoData)
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control

import qualified System.Linux.Netlink.Simple as NLS

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

printFamilyDetails :: Either String CtrlPacket -> IO ()
printFamilyDetails (Right fam) = print fam
printFamilyDetails (Left err) = putStrLn $ "Failed to decode family packet: " ++ err

printSimple :: CTRLPacket -> (Either String CtrlPacket -> IO ()) -> IO ()
printSimple request cb = do
  sock <- NLS.makeSimpleNLHandle =<< makeSocket
  NLS.nlSyncMessage sock request $ NLS.simpleSerializeCallback cb

printFamilies :: IO ()
printFamilies = printSimple  familiesRequest (\case
    Left err -> putStrLn $  "Failed to decode family packet: " ++ err
    Right family -> printFamily family)

printFamilieDetails :: String -> IO ()
printFamilieDetails name = printSimple (familyIdRequest name)  printFamilyDetails

printGeneric :: Either String (GenlPacket NoData) -> IO ()
printGeneric (Right packet) = print packet
printGeneric (Left err) = putStrLn $ "Failed to decode generic packet: " ++ err

handleFamily :: String -> NLS.NLHandle IO -> Either String CtrlPacket -> IO ()
handleFamily _ _ (Left err) = putStrLn $ "Failed to decode family message: " ++ err
handleFamily name sock (Right (CtrlPacket _ _ attrs)) = case getMulticast name $ getMCFromList attrs of
    Nothing-> putStrLn $ "Failed to find group: " ++ name
    Just grp -> joinMulticastGroup (NLS.nlHandleSock sock) grp

dumpGeneric :: String -> String -> IO ()
dumpGeneric fam grp = do
  sock <- NLS.makeSerializeNLHandle printGeneric =<< makeSocket
  NLS.nlSyncMessage sock (familyIdRequest fam) (NLS.simpleSerializeCallback $ handleFamily grp sock)
  forever $ NLS.nlProcessIncoming sock

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printFamilies
    [x] -> printFamilieDetails x
    [x,y] -> dumpGeneric x y
    _ -> putStrLn "This executable only takes a maximum of 2 arguments. Family and multicast group"
