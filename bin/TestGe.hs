module Main where

import Prelude hiding (length, concat)

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.ByteString (ByteString, length, unpack, pack, concat)
import Data.Bits ((.|.))
import Data.Map (fromList, empty)
import Data.Char (ord)

import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control

main = do
    sock <- makeSocket

    ifd <- getFamilyId sock "nl80211"
    print $show ifd

