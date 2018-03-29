{-# LANGUAGE BangPatterns #-}

module Dhcp.Load
  ( loadDhcpIndex
  ) where

import Chronos.Types (Time)
import Data.HashMap.Strict (HashMap)
import Dhcp.Parse (decodeLeases)
import Net.Types (IPv4,Mac)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LB
import qualified Dhcp.Types as DT

loadLeases :: FilePath -> IO LB.ByteString
loadLeases filepath = LB.readFile filepath

loadDhcp :: LB.ByteString -> [DT.Lease]
loadDhcp lbs = either (\str -> fail str) id (decodeLeases lbs)

macLookup :: [DT.Lease] -> (Mac -> Maybe IPv4)
macLookup leases = flip HM.lookup (leasesToHashMap leases)

loadDhcpIndex :: FilePath -> IO (Mac -> Maybe IPv4)
loadDhcpIndex settings = macLookup <$> loadDhcp <$> loadLeases settings

data Pair = Pair {-# UNPACK #-} !IPv4 {-# UNPACK #-} !Time

leasesToHashMap :: [DT.Lease] -> HashMap Mac IPv4
leasesToHashMap = fmap (\(Pair ip _) -> ip) . leasesToTimedHashMap

leasesToTimedHashMap :: [DT.Lease] -> HashMap Mac Pair
leasesToTimedHashMap = foldr h HM.empty
  where
  h :: DT.Lease -> HashMap Mac Pair -> HashMap Mac Pair
  h (DT.Lease ip vals) hm = case findMacAndTime vals of
    Nothing -> hm
    Just (mac,time) -> HM.alter g mac hm
      where
      g :: Maybe Pair -> Maybe Pair
      g Nothing = Just (Pair ip time)
      g (Just prevPair@(Pair _ prevTime)) = Just $ if time > prevTime
        then Pair ip time
        else prevPair

findMacAndTime :: [DT.Value] -> Maybe (Mac,Time)
findMacAndTime = hasNeither
  where
  hasNeither [] = Nothing
  hasNeither (v : vs) = case v of
    DT.ValueStarts t -> hasTime t vs
    DT.ValueHardware h -> hasMac (DT.hardwareMac h) vs
    _ -> hasNeither vs
  hasTime _ [] = Nothing
  hasTime t (v : vs) = case v of
    DT.ValueHardware h -> Just (DT.hardwareMac h,t)
    _ -> hasTime t vs
  hasMac _ [] = Nothing
  hasMac m (v : vs) = case v of
    DT.ValueStarts t -> Just (m,t)
    _ -> hasMac m vs