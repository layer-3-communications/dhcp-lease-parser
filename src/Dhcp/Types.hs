{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module Dhcp.Types
  ( Lease(..)
  , Value(..)
  , Hardware(..)
  , BindingState(..)
  , Name(..) 
  , NextValue(..)
  , NextName(..)
  , NextChar(..)
  , LazyByteString
  , StrictByteString
  , CharByteString
  , StrictText
  , BCParser 
  , DhcpStream 
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Net.Types (IPv4, Mac)
import Chronos.Types (Time)
import Streaming (Of, Stream)

type LazyByteString   = LB.ByteString
type StrictByteString =  B.ByteString
type CharByteString   = BC.ByteString
type StrictText       =  T.Text
type BCParser         = AB.Parser
type DhcpStream       = Stream (Of Lease) (Either String) ()

data Lease = Lease 
  { leaseIp     :: !IPv4
  , leaseValues :: ![Value]
  } deriving (Show,Read)

data Value 
  = ValueStarts           !Time
  | ValueEnds             !Time
  | ValueTstp             !Time
  | ValueAtsfp            !Time
  | ValueCltt             !Time
  | ValueBindingState     !BindingState
  | ValueNextBindingState !BindingState
  | ValueHardware         !Hardware
  | ValueUid              !StrictByteString
  | ValueClientHostname   !StrictText
  deriving (Eq,Ord,Show,Read)

data NextValue = NextValuePresent !Value | NextValueAbsent
data NextName  = NextNamePresent  !Name  | NextNameAbsent

data Name
  = NameStarts
  | NameEnds
  | NameTstp
  | NameAtsfp
  | NameCltt
  | NameBindingState
  | NameNextBindingState
  | NameHardware
  | NameUid
  | NameClientHostname

data BindingState
  = BindingStateFree
  | BindingStateActive
  deriving (Eq,Ord,Show,Read)

data Hardware = Hardware
  { hardwareType :: !StrictText
  , hardwareMac  :: !Mac
  } deriving (Eq,Ord,Show,Read)

-- useful for parsing, not necessary for dhcp leases
data NextChar = NextCharAgain {-# UNPACK #-} !Char | NextCharDone
