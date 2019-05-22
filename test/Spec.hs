{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

{-
import Test.Tasty
import Test.Tasty.HUnit
import Dhcp.Load
import Dhcp.Parse
import Dhcp.Types
import Data.Monoid

import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac
import qualified Chronos as Chronos
import qualified Chronos as PX
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Lazy as ABL
-}

import Dhcp.Types
import Dhcp.Parse
import Leases
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain $
  testGroup "Tests" [unitTests]

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
  ]
