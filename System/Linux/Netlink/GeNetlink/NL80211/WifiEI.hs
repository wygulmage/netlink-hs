{-# LANGUAGE CPP #-}
module System.Linux.Netlink.GeNetlink.NL80211.WifiEI
    ( showWifiEid
    , getWifiEIDs
    )
where

import qualified Data.Map as M
import Data.ByteString (ByteString)
import System.Linux.Netlink
import System.Linux.Netlink.Helpers (indent)
import Data.Serialize.Get (runGet, getByteString, getWord8, isEmpty, Get)
import Control.Monad.Loops (whileM)

import System.Linux.Netlink.GeNetlink.NL80211.Constants

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

getRight :: Show a => Either a b -> b
getRight (Right x) = x
getRight (Left err) = error $show err


showWifiEid :: ByteString -> String
showWifiEid bs = let attrs = getRight $ runGet getWifiEIDs bs in
  "WifiEIDs:\n" ++
  (indent $showAttrs showIEEE80211EID attrs)

-- |'Get' the EID Attributes from a buffer
getWifiEIDs :: Get Attributes
getWifiEIDs = M.fromList <$> whileM (not <$> isEmpty) getWifiEID

-- |'Get' an EID attribute from a buffer
getWifiEID :: Get (Int, ByteString)
getWifiEID = do
  ty  <- fromIntegral <$> getWord8
  len <- fromIntegral <$> getWord8
  val <- getByteString len
  return (ty, val)
