{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Dhcp.Types
  ( Lease(..)
  , Value(..)
  , Hardware(..)
  , BindingState(..)
  , Name(..) 
  , NextValue(..)
  , NextName(..)
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Net.Types (IPv4, Mac)
import Chronos.Types (Time)

data Lease = Lease 
  { leaseIp     :: !IPv4
  , leaseValues :: [Value]
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
  | ValueUid              !B.ByteString
  | ValueClientHostname   !T.Text
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
  | BindingStateAbandoned
  deriving (Eq,Ord,Show,Read)

data Hardware = Hardware
  { hardwareType :: !T.Text
  , hardwareMac  :: !Mac
  } deriving (Eq,Ord,Show,Read)
