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
    ( NLHandle (nlHandleSock)
    , NLCallback (..)
    , makeNLHandle
    , makeSerializeNLHandle
    , makeSimpleNLHandle
    , nlPostMessage
    , nlSyncMessage
    , nlProcessIncoming
    , nlWaitCurrent
    , isDone
    , simpleCallback
    , simpleSerializeCallback
    )
where

import Control.Monad (when)
import Control.Monad.Loops (whileM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bits (Bits(..))
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.IntMap (IntMap)
import Data.Word (Word32)

import Data.Serialize.Get (runGetPartial, Result(..))
import Data.Serialize (Serialize, decode)

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

-- | Helper function for callback without end action
simpleCallback :: Applicative m => (ByteString -> m ()) -> NLCallback m
simpleCallback = NLCallback (pure ())

-- | Helper function for callback without end action over a 'Serialize'able type
simpleSerializeCallback :: (Serialize a, Applicative m)
                        => (Either String a -> m ())
                        -> NLCallback m
simpleSerializeCallback = NLCallback (pure ()) . flip (.) decode

{- |Check if we've receive answers to all registered messages
 -
 - This is done by checking if any callbacks are still registered.
 - They will be unregistered automagically by the callback handler
 - when the last message (checked by the multi-part flag) is dispatched.
 -}
isDone :: (MonadIO m) =>  NLHandle n -> m Bool
isDone = fmap IM.null . liftIO . readIORef . nlHandleCallbacks

{- |Internal function to handle a single current read buffer
 -
 - This only de-marshals the netlink header to check the sequence number of
 - the message to dispatch it onto the right callback.
 - This might call more than one callback, since we don't have the exact
 - message size before reading and there may be more than one message in the
 - buffer
 -}
handleBuffer :: (MonadIO m)
             => IORef (IntMap (NLCallback m)) -- ^Ref to callback map to remove callback
             -> (ByteString -> m ()) -- ^Handler for messages without attached callback
             -> ByteString  -- ^The byte buffer to handle
             -> m ()
handleBuffer _ _ "" = pure ()
handleBuffer cbRef bc bs = case runGetPartial NL.getHeader bs of
    Partial _ -> liftIO (hPutStrLn stderr "Failed to parse header from buffer :/")
    Fail msg _ -> liftIO (hPutStrLn stderr $ "Failed to parse header form buffer: " ++ msg)
    Done (clen, hdr) bs' -> do
        cbs <- liftIO $ readIORef cbRef
        case IM.lookup (fromIntegral $ NL.messageSeqNum hdr) cbs of
            Nothing -> bc $ BS.take (clen + 16) bs
            Just cb -> do
                when (NL.messageType hdr /= NLC.eNLMSG_DONE) $ nlcProcess cb $ BS.take (clen + 16) bs
                when ((NLC.fNLM_F_MULTI .&. NL.messageFlags hdr) == 0 || NL.messageType hdr == NLC.eNLMSG_DONE) $ do
                    liftIO $ atomicModifyIORef' cbRef ((,()) . IM.delete (fromIntegral $ NL.messageSeqNum hdr))
                    nlcEnd cb
        handleBuffer cbRef bc (BS.drop clen bs')

-- |Receive messages from the netlink socket and dispatch callbacks
nlProcessIncoming :: (MonadIO m)
                  => NLHandle m
                  -> m ()
nlProcessIncoming NLHandle {nlHandleSock = sock, nlHandleCallbacks = cbRef, nlHandleBCast = bc} = do
    buffer <- liftIO $ NL.recvBuffer sock
    handleBuffer cbRef bc buffer

-- |Send a single message over netlink socket to kernel and register callback
nlPostMessage :: (NL.Convertable a, MonadIO m)
              => NLHandle m -- ^The handle to send stuff over
              -> NL.Packet a -- ^The message to send to the kernel
              -> NLCallback m -- ^The callback to register for answers
              -> m ()
nlPostMessage NLHandle {nlHandleSeq = seqRef, nlHandleSock = sock, nlHandleCallbacks = cbRef} pkt cb = liftIO $ do
    seqNum <- atomicModifyIORef' seqRef (\x -> (x + 1, x + 1))
    NL.sendPacket sock pkt {NL.packetHeader = (NL.packetHeader pkt) {NL.messageSeqNum = seqNum}}
    atomicModifyIORef' cbRef ((,()) . IM.insert (fromIntegral seqNum) cb)

-- | 'nlPostMessage' >> 'nlWaitCurrent'
nlSyncMessage :: (NL.Convertable a, MonadIO m)
              => NLHandle m -- ^The handle to send stuff over
              -> NL.Packet a -- ^The message to send to the kernel
              -> NLCallback m -- ^The callback to register for answers
              -> m ()
nlSyncMessage h p c = nlPostMessage h p c >> nlWaitCurrent h

-- |Create a Netlink handle for callback style handling of messages
makeNLHandle :: (MonadIO m)
             => (ByteString -> m()) -- ^The callback to call for messages that can't be associated with a previously sent message
             -> NL.NetlinkSocket -- ^The NetlinkSocket to wrap
             -> m (NLHandle m)
makeNLHandle bcastHandler socket = NLHandle
    <$> liftIO (newIORef 0)
    <*> pure socket
    <*> liftIO (newIORef mempty)
    <*> pure bcastHandler

-- |Create a Netlink handle for callback style handling of messages without broadcast handler
makeSimpleNLHandle :: (MonadIO m)
                   => NL.NetlinkSocket -- ^The NetlinkSocket to wrap
                   -> m (NLHandle m)
makeSimpleNLHandle = makeNLHandle (const $ pure ())

-- |Create a Netlink handle for callback style handling of messages on 'Serialize'able data
makeSerializeNLHandle :: (MonadIO m, Serialize a)
                      => (Either String a -> m()) -- ^The callback to call for messages that can't be associated with a previously sent message
                      -> NL.NetlinkSocket -- ^The NetlinkSocket to wrap
                      -> m (NLHandle m)
makeSerializeNLHandle bcastHandler socket =
    makeNLHandle (bcastHandler . decode ) socket

{- |Handle all messages on the netlink socket while there's a callback registered
 -
 - This will block and dispatch callbacks until we received a message for every
 - message sent via 'nlPostMessage' on the handle before.
 - The callback registered with the handle in the beginning might be called
 - an arbitrary amount of times as well, to handle messages caused by e.g.
 - multicast groups.
 -}
nlWaitCurrent :: (MonadIO m) => NLHandle m -> m ()
nlWaitCurrent sock = whileM_ (not <$> isDone sock) $ nlProcessIncoming sock
