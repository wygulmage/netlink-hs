{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Linux.Netlink.GeNetlink.Constants (ControlCommand, showControlCommand, eCTRL_CMD_UNSPEC, eCTRL_CMD_NEWFAMILY, eCTRL_CMD_DELFAMILY, eCTRL_CMD_GETFAMILY, eCTRL_CMD_NEWOPS, eCTRL_CMD_DELOPS, eCTRL_CMD_GETOPS, eCTRL_CMD_NEWMCAST_GRP, eCTRL_CMD_DELMCAST_GRP, eCTRL_CMD_GETMCAST_GRP, CtrlAttr, showCtrlAttr, eCTRL_ATTR_UNSPEC, eCTRL_ATTR_FAMILY_ID, eCTRL_ATTR_FAMILY_NAME, eCTRL_ATTR_VERSION, eCTRL_ATTR_HDRSIZE, eCTRL_ATTR_MAXATTR, eCTRL_ATTR_OPS, eCTRL_ATTR_MCAST_GROUPS, CtrlAttrOp, showCtrlAttrOp, eCTRL_ATTR_OP_UNSPEC, eCTRL_ATTR_OP_ID, eCTRL_ATTR_OP_FLAGS, CtrlAttrMcast, showCtrlAttrMcast, eCTRL_ATTR_MCAST_GRP_UNSPEC, eCTRL_ATTR_MCAST_GRP_NAME, eCTRL_ATTR_MCAST_GRP_ID) where

newtype ControlCommand = ControlCommand Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

showControlCommand :: (Num a) => (Show a) => (Eq a) => a -> String
showControlCommand 0 = "CTRL_CMD_UNSPEC"
showControlCommand 1 = "CTRL_CMD_NEWFAMILY"
showControlCommand 2 = "CTRL_CMD_DELFAMILY"
showControlCommand 3 = "CTRL_CMD_GETFAMILY"
showControlCommand 4 = "CTRL_CMD_NEWOPS"
showControlCommand 5 = "CTRL_CMD_DELOPS"
showControlCommand 6 = "CTRL_CMD_GETOPS"
showControlCommand 7 = "CTRL_CMD_NEWMCAST_GRP"
showControlCommand 8 = "CTRL_CMD_DELMCAST_GRP"
showControlCommand 9 = "CTRL_CMD_GETMCAST_GRP"
showControlCommand i = "ControlCommand #" ++ (show i)


eCTRL_CMD_UNSPEC :: (Num a) => a
eCTRL_CMD_UNSPEC = 0
eCTRL_CMD_NEWFAMILY :: (Num a) => a
eCTRL_CMD_NEWFAMILY = 1
eCTRL_CMD_DELFAMILY :: (Num a) => a
eCTRL_CMD_DELFAMILY = 2
eCTRL_CMD_GETFAMILY :: (Num a) => a
eCTRL_CMD_GETFAMILY = 3
eCTRL_CMD_NEWOPS :: (Num a) => a
eCTRL_CMD_NEWOPS = 4
eCTRL_CMD_DELOPS :: (Num a) => a
eCTRL_CMD_DELOPS = 5
eCTRL_CMD_GETOPS :: (Num a) => a
eCTRL_CMD_GETOPS = 6
eCTRL_CMD_NEWMCAST_GRP :: (Num a) => a
eCTRL_CMD_NEWMCAST_GRP = 7
eCTRL_CMD_DELMCAST_GRP :: (Num a) => a
eCTRL_CMD_DELMCAST_GRP = 8
eCTRL_CMD_GETMCAST_GRP :: (Num a) => a
eCTRL_CMD_GETMCAST_GRP = 9
newtype CtrlAttr = CtrlAttr Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

showCtrlAttr :: (Num a) => (Show a) => (Eq a) => a -> String
showCtrlAttr 0 = "CTRL_ATTR_UNSPEC"
showCtrlAttr 1 = "CTRL_ATTR_FAMILY_ID"
showCtrlAttr 2 = "CTRL_ATTR_FAMILY_NAME"
showCtrlAttr 3 = "CTRL_ATTR_VERSION"
showCtrlAttr 4 = "CTRL_ATTR_HDRSIZE"
showCtrlAttr 5 = "CTRL_ATTR_MAXATTR"
showCtrlAttr 6 = "CTRL_ATTR_OPS"
showCtrlAttr 7 = "CTRL_ATTR_MCAST_GROUPS"
showCtrlAttr i = "CtrlAttr #" ++ (show i)


eCTRL_ATTR_UNSPEC :: (Num a) => a
eCTRL_ATTR_UNSPEC = 0
eCTRL_ATTR_FAMILY_ID :: (Num a) => a
eCTRL_ATTR_FAMILY_ID = 1
eCTRL_ATTR_FAMILY_NAME :: (Num a) => a
eCTRL_ATTR_FAMILY_NAME = 2
eCTRL_ATTR_VERSION :: (Num a) => a
eCTRL_ATTR_VERSION = 3
eCTRL_ATTR_HDRSIZE :: (Num a) => a
eCTRL_ATTR_HDRSIZE = 4
eCTRL_ATTR_MAXATTR :: (Num a) => a
eCTRL_ATTR_MAXATTR = 5
eCTRL_ATTR_OPS :: (Num a) => a
eCTRL_ATTR_OPS = 6
eCTRL_ATTR_MCAST_GROUPS :: (Num a) => a
eCTRL_ATTR_MCAST_GROUPS = 7
newtype CtrlAttrOp = CtrlAttrOp Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

showCtrlAttrOp :: (Num a) => (Show a) => (Eq a) => a -> String
showCtrlAttrOp 0 = "CTRL_ATTR_OP_UNSPEC"
showCtrlAttrOp 1 = "CTRL_ATTR_OP_ID"
showCtrlAttrOp 2 = "CTRL_ATTR_OP_FLAGS"
showCtrlAttrOp i = "CtrlAttrOp #" ++ (show i)


eCTRL_ATTR_OP_UNSPEC :: (Num a) => a
eCTRL_ATTR_OP_UNSPEC = 0
eCTRL_ATTR_OP_ID :: (Num a) => a
eCTRL_ATTR_OP_ID = 1
eCTRL_ATTR_OP_FLAGS :: (Num a) => a
eCTRL_ATTR_OP_FLAGS = 2
newtype CtrlAttrMcast = CtrlAttrMcast Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)

showCtrlAttrMcast :: (Num a) => (Show a) => (Eq a) => a -> String
showCtrlAttrMcast 0 = "CTRL_ATTR_MCAST_GRP_UNSPEC"
showCtrlAttrMcast 1 = "CTRL_ATTR_MCAST_GRP_NAME"
showCtrlAttrMcast 2 = "CTRL_ATTR_MCAST_GRP_ID"
showCtrlAttrMcast i = "CtrlAttrMcast #" ++ (show i)


eCTRL_ATTR_MCAST_GRP_UNSPEC :: (Num a) => a
eCTRL_ATTR_MCAST_GRP_UNSPEC = 0
eCTRL_ATTR_MCAST_GRP_NAME :: (Num a) => a
eCTRL_ATTR_MCAST_GRP_NAME = 1
eCTRL_ATTR_MCAST_GRP_ID :: (Num a) => a
eCTRL_ATTR_MCAST_GRP_ID = 2
