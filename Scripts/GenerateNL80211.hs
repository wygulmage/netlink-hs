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
            "module System.Linux.Netlink.GeNetlink.NL80211.Constants (" ++
            join (intersperse ", " $ join exports) ++
            ") where",
            ""]
    writeFile out $ unlines (prelude ++ join definitions)

outputs :: Map String Integer -> [Map String Integer] -> ([[String]], [[String]])
outputs d e = let define r = selectDefines r d
                  enum r = selectEnum r e
              in map fst &&& map snd $
       [ mkEnum "NL80211Commands" $ enum "^NL80211_CMD_"
       , mkEnum "NL80211Attrs" $ enum "^NL80211_ATTR_([^C]|(C[^Q])|(CQ[^M])|(CQM$))|NUM_NL80211_ATTR$"
       ]

includeFiles :: [String]
includeFiles = [ "linux/nl80211.h" ]

