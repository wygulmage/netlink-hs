{-# LANGUAGE CPP #-}
module System.Linux.Netlink.GeNetlink.NL80211.StaInfo
    ( StaInfo (..)
    , SignalWidth (..)
    , Signal (..)
    , StaRate (..)

    , signalFromAttributes
    , staRateFromAttributes
    , staInfoFromAttributes
    , getStaInfo
    , staInfoFromPacket
    )
where

import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Applicative ((<|>))

import System.Linux.Netlink
import System.Linux.Netlink.GeNetlink.NL80211.WifiEI
import System.Linux.Netlink.GeNetlink.NL80211.Constants
import Data.Word

import Data.Serialize.Get

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

newtype Signal = Signal [Word8] deriving (Show, Eq, Read)

-- |Get a Signal from the nested attributes.
signalFromAttributes :: Attributes -> Signal
signalFromAttributes attrs =
    let bss = map snd . M.toList $ attrs
        eth = map (runGet getWord8) bss
     in Signal . map getRight $ eth
    where getRight (Right x) = x
          getRight (Left x)  = error $ "Failed to decode signal: " ++ x


{- | Type for the signal width reported by the kernel.

 The nl80211 header defines more than this, but nl80211.c only uses the widths defined here.
-}
data SignalWidth
    = Width5MHz
    | Width10MHz
    | Width20MHz
    | Width40MHz
    | Width80MHz
    | Width160MHz
    deriving (Show, Eq, Read)

-- |Get the signal width from attributes that contain the flag.
widthFromAttributes :: Attributes -> SignalWidth
widthFromAttributes attrs =
    let five  = opt Width5MHz eNL80211_RATE_INFO_5_MHZ_WIDTH
        ten   = opt Width10MHz eNL80211_RATE_INFO_10_MHZ_WIDTH
        forty = opt Width40MHz eNL80211_RATE_INFO_40_MHZ_WIDTH
        eighty = opt Width80MHz eNL80211_RATE_INFO_80_MHZ_WIDTH
        osixty = opt Width160MHz eNL80211_RATE_INFO_160_MHZ_WIDTH
        alls = [five, ten, forty, eighty, osixty]
     in fromMaybe Width20MHz $ foldr1 (<|>) alls
    where opt :: SignalWidth -> Int -> Maybe SignalWidth
          opt c e = fmap (const c) . M.lookup e $ attrs

data StaRate = StaRate
    { -- |This will be reported as Word16/Word32 from the kernel. We read it into one value.
      -- |If this is Nothing, mcs is >= 32 looking at the code, so it *should*
      -- |never be Nothing.
      rateBitrate   :: Maybe Word32
    , rateWidthFlag :: SignalWidth
    , rateMCS       :: Maybe Word8
    -- Hm
    , rateShortGI   :: Bool

    , rateVHTMCS    :: Maybe Word8
    , rateVHTNSS    :: Maybe Word8

    , rateSelf      :: Attributes
    } deriving (Show, Eq, Read)

staRateFromAttributes :: Attributes -> StaRate
staRateFromAttributes attrs =
    let rate16 = getField getWord16host eNL80211_RATE_INFO_BITRATE
        rate32 = getField getWord32host eNL80211_RATE_INFO_BITRATE32
        -- this locks us into Word32 for now, but that's ok.
        rate = rate32 <|> fmap fromIntegral rate16
        -- The rate width flag is "inline" in the rate.
        width = widthFromAttributes attrs
        mcs = getField getWord8 eNL80211_RATE_INFO_MCS
        shortGI = M.member eNL80211_RATE_INFO_SHORT_GI attrs
        vhtmcs = getField getWord8 eNL80211_RATE_INFO_VHT_MCS
        vhtnss = getField getWord8 eNL80211_RATE_INFO_VHT_NSS
    in StaRate rate width mcs shortGI vhtmcs vhtnss attrs
    where getField :: Get a -> Int -> Maybe a
          getField g e = fmap (getRight . runGet g) . M.lookup e $ attrs
          getRight :: Either String a -> a
          getRight (Right x) = x
          getRight (Left x)  = error $ "Failed to parse something in StaRate: " ++ x

-- |Structure for wifi station information.
data StaInfo = StaInfo
    { -- |For how long we are connected.
      staConTime    :: Maybe Word32
    -- |Time since the last time we saw the station send something.
    , staInaTime    :: Maybe Word32
    -- |Bytes received. This will be transmitted twice if 64bit in kernel. Will be parsed into this either way.
    , staRXBytes    :: Maybe Word64
    -- |Bytes received. This will be transmitted twice if 64bit in kernel. Will be parsed into this either way.
    , staTXBytes    :: Maybe Word64
    , staLLID       :: Maybe Word16
    , staPLID       :: Maybe Word16
    -- |TODO: Is this a enum? Could be a bit better.
    , staPLState    :: Maybe Word8
    , staRXDur      :: Maybe Word64
    , staSignalMBM  :: Maybe Word8
    , staSignalMBMA :: Maybe Word8
    , staSignal     :: Maybe Signal
    , staSignalAvg  :: Maybe Signal
    , staTXRate     :: Maybe StaRate
    , staRXRate     :: Maybe StaRate

    , staRXPackets  :: Maybe Word32
    , staTXPackets  :: Maybe Word32
    , staTXRetries  :: Maybe Word32
    , staTXFailed   :: Maybe Word32
    -- |Expected throughput. TODO: Can we use this?
    , staExpectTP   :: Maybe Word32
    , staBeaconLoss :: Maybe Word32

    -- |PM: STA link specific Power Mode
    , staLocalPM    :: Maybe Word32
    , staPeerPM     :: Maybe Word32
    , staNonPeerPM  :: Maybe Word32

    -- |This field is a bit weird in the code :(
    , staBssAttrs   :: Maybe Attributes
    , staInfoFlags  :: Maybe ByteString
    , staTOffset    :: Maybe Word64
    , staRXDropMisc :: Maybe Word64
    , staBeaconRX   :: Maybe Word64
    , staBSignalAvg :: Maybe Word8

    , staTidStats   :: Maybe Attributes
    , staAssocIES   :: Maybe Attributes

    -- |Pointer to the Attributes map used to build this struct. This is purely
    -- |for forward compat, please file a feature report if you have to use this.
    , staSelf       :: Attributes
    } deriving (Show, Eq, Read)

staInfoFromAttributes :: Attributes -> StaInfo
staInfoFromAttributes attrs =
    let conTime = getField getWord32host eNL80211_STA_INFO_CONNECTED_TIME
        inaTime = getField getWord32host eNL80211_STA_INFO_INACTIVE_TIME
        rxB32   = getField getWord32host eNL80211_STA_INFO_RX_BYTES
        txB32   = getField getWord32host eNL80211_STA_INFO_TX_BYTES
        rxB64   = getField getWord64host eNL80211_STA_INFO_RX_BYTES64
        txB64   = getField getWord64host eNL80211_STA_INFO_TX_BYTES64
        rxBytes = rxB64 <|> fmap fromIntegral rxB32
        txBytes = txB64 <|> fmap fromIntegral txB32
        llid    = getField getWord16host eNL80211_STA_INFO_LLID
        plid    = getField getWord16host eNL80211_STA_INFO_PLID
        lstate  = getField getWord8 eNL80211_STA_INFO_PLINK_STATE
        rxDur   = getField getWord64host eNL80211_STA_INFO_RX_DURATION
        sigMBM  = getField getWord8 eNL80211_STA_INFO_SIGNAL
        sigMBMA = getField getWord8 eNL80211_STA_INFO_SIGNAL_AVG
        sigBS   = getField getAttributes eNL80211_STA_INFO_CHAIN_SIGNAL
        sigBSA  = getField getAttributes eNL80211_STA_INFO_CHAIN_SIGNAL_AVG
        txr     = getField getAttributes eNL80211_STA_INFO_TX_BITRATE
        rxr     = getField getAttributes eNL80211_STA_INFO_RX_BITRATE

        rxpack  = getField getWord32host eNL80211_STA_INFO_RX_PACKETS
        txpack  = getField getWord32host eNL80211_STA_INFO_TX_PACKETS
        txretr  = getField getWord32host eNL80211_STA_INFO_TX_RETRIES
        txfail  = getField getWord32host eNL80211_STA_INFO_TX_FAILED

        exptp   = getField getWord32host eNL80211_STA_INFO_EXPECTED_THROUGHPUT
        beloss  = getField getWord32host eNL80211_STA_INFO_BEACON_LOSS
        localpm = getField getWord32host eNL80211_STA_INFO_LOCAL_PM
        peerpm  = getField getWord32host eNL80211_STA_INFO_PEER_PM
        npeerpm = getField getWord32host eNL80211_STA_INFO_NONPEER_PM

        bsspar  = getField getAttributes eNL80211_STA_INFO_BSS_PARAM
        flags   = M.lookup eNL80211_STA_INFO_STA_FLAGS attrs
        toff    = getField getWord64host eNL80211_STA_INFO_T_OFFSET
        rxdrop  = getField getWord64host eNL80211_STA_INFO_RX_DROP_MISC
        beacr   = getField getWord64host eNL80211_STA_INFO_BEACON_RX
        beacsa  = getField getWord8 eNL80211_STA_INFO_BEACON_SIGNAL_AVG

        tidStat = getField getAttributes eNL80211_STA_INFO_TID_STATS
        associe = getField getWifiEIDs eNL80211_ATTR_IE
     in StaInfo
            conTime inaTime rxBytes txBytes llid plid
            lstate rxDur sigMBM sigMBMA
            (signalFromAttributes <$> sigBS)
            (signalFromAttributes <$> sigBSA)
            (staRateFromAttributes <$> txr)
            (staRateFromAttributes <$> rxr)
            rxpack txpack txretr txfail exptp beloss localpm peerpm
            npeerpm bsspar flags toff rxdrop beacr beacsa tidStat associe
            attrs
    where getField :: Get a -> Int -> Maybe a
          getField g e = fmap (getRight . runGet g) . M.lookup e $ attrs
          getRight :: Either String a -> a
          getRight (Right x) = x
          getRight (Left x)  = error $ "Failed to parse something in StaInfo: " ++ x

getStaInfo :: Get StaInfo
getStaInfo = fmap staInfoFromAttributes getAttributes

staInfoFromPacket :: Packet a -> Maybe StaInfo
staInfoFromPacket (Packet _ _ attrs) =
    let y = runGet getStaInfo <$> M.lookup eNL80211_ATTR_STA_INFO attrs
    in fmap getRight y
    where getRight (Right x) = x
          getRight (Left x)  = error $ "Failed to decode staInfo: " ++ x
staInfoFromPacket _ = Nothing
-- This eats an error packet, fix?
