module Main where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.List (intersperse)
import Data.Map (Map, union)
import System.Environment (getArgs)

import Scripts.Helpers

main = do
    [out] <- getArgs
    let inc = mkIncludeBlock includeFiles
    defines <- getDefinitions inc
    enums <- getEnums inc
    let (exports, definitions) = outputs defines enums
        prelude = [
            "{-# LANGUAGE GeneralizedNewtypeDeriving #-}",
            "module System.Linux.Netlink.Constants (" ++
            join (intersperse ", " $ join exports) ++
            ") where",
            "",
            "import Data.Bits",
            ""]
    writeFile out $ unlines (prelude ++ join definitions)

outputs :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e = let define r = selectDefines r d
                  enum r = selectEnum r e
              in map fst &&& map snd $
    [mkEnum "AddressFamily" $ define "^AF_",
     mkEnum "MessageType" $
       union (define "^NLMSG_(?!ALIGNTO)") (enum "^RTM_"),
     mkFlag "MessageFlags"  $ define "^NLM_F_",
     mkEnum "LinkType"      $ define "^ARPHRD_",
     mkFlag "LinkFlags"     $
       union (define "^IFF_") (enum "^IFF_"),
     mkEnum "LinkAttrType"  $ enum   "^IFLA_",
     mkFlag "AddrFlags"     $ define "^IFA_F_",
     mkEnum "Scope"         $ enum   "^RT_SCOPE_",
     mkEnum "AddrAttrType"  $ enum   "^IFA_",
     mkEnum "RouteTableId"  $ enum   "^RT_TABLE_",
     mkEnum "RouteProto"    $ define "^RTPROT_",
     mkEnum "RouteType"     $ enum   "^RTN_",
     mkFlag "RouteFlags"    $ define "^RTM_F_",
     mkEnum "RouteAttrType" $ enum   "^RTA_"]

includeFiles :: [String]
includeFiles = [ "sys/types.h"
               , "sys/socket.h"
               , "linux/if.h"
               , "linux/if_tun.h"
               , "linux/if_arp.h"
               , "linux/if_link.h"
               , "linux/netlink.h"
               , "linux/rtnetlink.h"
               ]

