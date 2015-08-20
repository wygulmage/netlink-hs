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
            "module System.Linux.Netlink.GeNetlink.Constants (" ++
            join (intersperse ", " $ join exports) ++
            ") where",
            ""]
    writeFile out $ unlines (prelude ++ join definitions)

outputs :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e = let define r = selectDefines r d
                  enum r = selectEnum r e
              in map fst &&& map snd $
    [mkEnum "ControlCommand" $ enum "^CTRL_CMD_[^_]+",
     mkEnum "CtrlAttr"      $ enum "^CTRL_ATTR_",
     mkEnum "CtrlAttrOp"    $ enum   "^CTRL_ATTR_OP_",
     mkEnum "CtrlAttrMcast"  $ enum   "^CTRL_ATTR_MCAST_"]

includeFiles :: [String]
includeFiles = [ "sys/types.h"
               , "sys/socket.h"
               , "linux/genetlink.h"
               ]
