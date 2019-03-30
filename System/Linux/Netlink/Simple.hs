{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : System.Linux.Netlink.Simple
Description : The base module for the netlink package
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module System.Linux.Netlink.Simple
    ( NLHandle
    , NLCallback (..)
    , makeNLHandle
    , nlPostMessage
    , nlProcessIncoming
    , nlWaitCurrent
    , isDone
    )
where

import Data.Bits (Bits(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.IntMap (IntMap)
import Data.Word (Word32)

import Data.Serialize.Get (runGetPartial, Result(..))

import qualified Data.IntMap as IM
import qualified Data.ByteString as BS
import qualified System.Linux.Netlink as NL
import qualified System.Linux.Netlink.Constants as NLC

import System.IO (hPutStrLn, stderr)

data NLCallback m = NLCallback
    { nlcEnd     :: m ()
    , nlcProcess :: ByteString -> m ()
    }

data NLHandle m = NLHandle
    { nlHandleSeq       :: IORef Word32
    , nlHandleSock      :: NL.NetlinkSocket
    , nlHandleCallbacks :: IORef (IntMap (NLCallback m))
    , nlHandleBCast     :: ByteString -> m ()
    }

isDone :: (MonadIO m) =>  NLHandle n -> m Bool
isDone = fmap IM.null . liftIO . readIORef . nlHandleCallbacks

handleBuffer :: (MonadIO m) => IntMap (NLCallback m) -> IORef (IntMap (NLCallback m)) -> (ByteString -> m ()) -> ByteString -> m ()
handleBuffer _ _ _ "" = pure ()
handleBuffer cbs cbRef bc bs = case runGetPartial NL.getHeader bs of
    Partial _ -> liftIO (hPutStrLn stderr "Failed to parse header from buffer :/")
    Fail msg _ -> liftIO (hPutStrLn stderr $ "Failed to parse header form buffer: " ++ msg)
    Done (clen, hdr) bs' -> do
        case IM.lookup (fromIntegral $ NL.messageSeqNum hdr) cbs of
            Nothing -> bc $ BS.take (clen + 16) bs
            Just cb -> do
                when (NL.messageType hdr /= NLC.eNLMSG_DONE) $ nlcProcess cb $ BS.take (clen + 16) bs
                when ((NLC.fNLM_F_MULTI .&. NL.messageFlags hdr) == 0 || NL.messageType hdr == NLC.eNLMSG_DONE) $ do
                    liftIO $ atomicModifyIORef' cbRef ((,()) . IM.delete (fromIntegral $ NL.messageSeqNum hdr))
                    nlcEnd cb
        handleBuffer cbs cbRef bc (BS.drop clen bs')

nlProcessIncoming :: (MonadIO m) => NLHandle m -> m ()
nlProcessIncoming NLHandle {nlHandleSock = sock, nlHandleCallbacks = cbRef, nlHandleBCast = bc} = do
    buffer <- liftIO $ NL.recvBuffer sock
    cbs <- liftIO $ readIORef cbRef
    handleBuffer cbs cbRef bc buffer

nlPostMessage :: (NL.Convertable a, Eq a, Show a, MonadIO m) => NLHandle m -> NL.Packet a -> NLCallback m-> m ()
nlPostMessage NLHandle {nlHandleSeq = seqRef, nlHandleSock = sock, nlHandleCallbacks = cbRef} pkt cb = liftIO $ do
    seqNum <- atomicModifyIORef' seqRef (\x -> (x + 1, x + 1))
    NL.sendPacket sock pkt {NL.packetHeader = (NL.packetHeader pkt) {NL.messageSeqNum = seqNum}}
    atomicModifyIORef' cbRef ((,()) . IM.insert (fromIntegral seqNum) cb)

makeNLHandle :: (MonadIO m) => (ByteString -> m()) -> NL.NetlinkSocket -> m (NLHandle m)
makeNLHandle bcastHandler socket = NLHandle
    <$> liftIO (newIORef 0)
    <*> pure socket
    <*> liftIO (newIORef mempty)
    <*> pure bcastHandler

nlWaitCurrent :: (MonadIO m) => NLHandle m -> m ()
nlWaitCurrent sock = do
    done <- isDone sock
    when (not done) $ do
        nlProcessIncoming sock
        nlWaitCurrent sock
