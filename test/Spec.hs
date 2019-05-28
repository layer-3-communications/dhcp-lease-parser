{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Chronos (Time(..))
import Data.Semigroup (Max(..))
import Dhcp.Parse
import Dhcp.Types
import Leases
import Net.Types (Mac, IPv4)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Net.Mac as Mac

main :: IO ()
main = defaultMain $
  testGroup "Tests" [unitTests]

deriving instance Bounded Time

mostRecent :: ()
  => Mac
  -> [Lease]
  -> Assertion
mostRecent mac leases = case mostRecentDebug mac leases of
  Left err -> assertBool ("Most Recent IPv4 not retrieved by leasesToTimedHashMap.\n" <> err) False
  Right () -> assertBool "shouldn't show up" True

-- test that when a collision occurs, we get the most recent IPv4
mostRecentDebug :: ()
  => Mac
  -> [Lease]
  -> Either String ()
mostRecentDebug mac leases =
  let biggestIp = findMostRecentIp leases
      whatISay = HM.lookup mac (leasesToTimedHashMap leases)
   in case (biggestIp,whatISay) of
        (Nothing,Nothing) -> Left
          $ "Biggest IPv4 resulted in a Nothing.\n"
          <> "WhatISay resulted in a Nothing.\n"
        (l,Nothing) -> Left
          $ "Biggest IPv4 is: " <> show l <> ".\n"
          <> "WhatISay resulted in a Nothing.\n"
        (Nothing,r) -> Left
          $ "Biggest IPv4 resulted in a Nothing.\n"
          <> "WhatISay is: " <> show r <> ".\n"
        (Just l, Just r) -> if l == r
          then Right ()
          else Left $
            "Biggest IPv4 \\neq WhatISay.\n"
            <> "Biggest IPv4: " <> show l <> ".\n"
            <> "WhatISay: " <> show r <> ".\n"
  where
    findMostRecentIp [] = Nothing
    findMostRecentIp xs = Just $ L.last . L.sortBy s . foldr h [] $ xs
    h :: Lease -> [TimedIPv4] -> [TimedIPv4]
    h (Lease ip vals) tm = case findMacAndTime vals of
      Nothing -> tm
      Just (mac,time) -> (TimedIPv4 ip time : tm)
    s :: TimedIPv4 -> TimedIPv4 -> Ordering
    s (TimedIPv4 _ t1) (TimedIPv4 _ t2) = compare t1 t2

numEach :: ()
  => Int -- ^ number of expected errors
  -> Int -- ^ number of expected successes
  -> ([LeaseError], [Lease])
  -> Assertion
numEach expectedErrs expectedLeases (errs,leases) =
  let actualErrs = length errs
      actualLeases = length leases
      msg = mconcat
        [ "number of expected errors: " <> show expectedErrs <> "\n"
        , "number of actual errors:   " <> show actualErrs <> "\n\n"
        , "number of expected successes: " <> show expectedLeases <> "\n"
        , "number of actual successes: " <> show actualLeases <> "\n"
        ]
   in assertBool msg (expectedErrs == actualErrs && expectedLeases == actualLeases)

oneLease = numEach 0 1

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "lease0" $ oneLease $ decodeLeases lease0
  , testCase "lease1" $ oneLease $ decodeLeases lease1
  , testCase "lease2" $ oneLease $ decodeLeases lease2
  , testCase "lease3" $ oneLease $ decodeLeases lease3
  , testCase "lease4" $ oneLease $ decodeLeases lease4
  , testCase "lease5" $ oneLease $ decodeLeases lease5
  , testCase "lease6" $ oneLease $ decodeLeases lease6
  , testCase "lease7" $ oneLease $ decodeLeases lease7
  , testCase "lease8" $ numEach 0 2 $ decodeLeases lease8
  , testCase "lease9" $ numEach 1 1 $ decodeLeases lease9
  , testCase "lease10" $ numEach 1 3 $ decodeLeases lease10
  , testCase "lease11" $ mostRecent (Mac.fromOctets 0x0 0x02 0xa1 0x25 0x90 0xa0) $ (snd $ decodeLeases lease11)
  , testCase "lease12" $ mostRecent (Mac.fromOctets 0x0 0x02 0xa1 0x25 0x90 0xa0) $ (snd $ decodeLeases lease12)
  , testCase "lease13" $ mostRecent (Mac.fromOctets 0x0 0x02 0xa1 0x25 0x90 0xa0) $ (snd $ decodeLeases lease13)
  ]
