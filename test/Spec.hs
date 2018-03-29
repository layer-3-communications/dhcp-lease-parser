{-# LANGUAGE OverloadedStrings #-}

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "000" $ oneLease "sample/000.txt" $ Lease
      (IPv4.fromOctets 10 160 23 253)
      [ ValueStarts $ Chronos.timeFromYmdhms 2016 1 11 18 31 21
      , ValueEnds   $ Chronos.timeFromYmdhms 2016 02 08 18 38 1
      , ValueTstp   $ Chronos.timeFromYmdhms 2016 02 08 18 38 1
      , ValueBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0x80 0x05 0x3E 0x9A
      ]
  , testCase "001" $ oneLease "sample/001.txt" $ Lease
      (IPv4.fromOctets 10 152 33 140)
      [ ValueStarts $ Chronos.timeFromYmdhms 2017 5 18 14 05 18
      , ValueEnds   $ Chronos.timeFromYmdhms 2017 06 15 14 11 58
      , ValueBindingState BindingStateActive
      , ValueNextBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0x80 0x04 0x30 0x63
      , ValueUid $ ""
      , ValueClientHostname "000280043063"
      ]
  , testCase "002" $ oneLease "sample/002.txt" $ Lease
      (IPv4.fromOctets 10 102 18 226)
      [ ValueStarts $ Chronos.timeFromYmdhms 2017 5 18 5 40 43
      , ValueEnds   $ Chronos.timeFromYmdhms 2017 05 19 0 40 43
      , ValueBindingState BindingStateActive
      , ValueNextBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0xA1 0x23 0x03 0x80
      , ValueUid ""
      ]
--  , testCase "003" $ oneLease "sample/003.txt" $ Lease
--      (IPv4.fromOctets 10 160 22 231)
--      [ ValueStarts $ Chronos.timeFromYmdhms 2018 02 21 23 52 59
--      , ValueEnds   $ Chronos.timeFromYmdhms 2018 03 21 23 59 39
--      , ValueBindingState BindingStateActive
--      , ValueNextBindingState BindingStateFree
--      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0x80 0x04 0x34 0xe8
--      , ValueUid ""
--      , ValueClientHostname "0002800434E8"
--      ]
  , testCase "004" $ oneLease "sample/004.txt" $ Lease
      (IPv4.fromOctets 10 160 22 225)
      [ ValueStarts $ Chronos.timeFromYmdhms 2018 02 21 23 52 59
      , ValueEnds   $ Chronos.timeFromYmdhms 2018 03 21 23 59 39
      , ValueBindingState BindingStateActive
      , ValueNextBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0x80 0x04 0x3b 0x39
      , ValueUid ""
      , ValueClientHostname "000280043B39"
      ]
  , testCase "005" $ oneLease "sample/005.txt" $ Lease
      (IPv4.fromOctets 10 160 22 227)
      [ ValueStarts $ Chronos.timeFromYmdhms 2018 02 26 17 18 08
      , ValueEnds   $ Chronos.timeFromYmdhms 2018 03 26 17 24 48
      , ValueBindingState BindingStateActive
      , ValueNextBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0x80 0x04 0x1d 0x7f
      , ValueUid ""
      , ValueClientHostname "000280041D7F"
      ]
  , testCase "007" $ oneLease' "sample/007.txt" $ pure $ Lease
      (IPv4.fromOctets 10 153 2 77)
      [ ValueStarts $ Chronos.timeFromYmdhms 2015 12 30 8 48 24
      , ValueEnds   $ Chronos.timeFromYmdhms 2015 12 30 8 50 24
      , ValueTstp   $ Chronos.timeFromYmdhms 2015 12 30 8 50 24
      , ValueBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0 0 0 0 0 0
      , ValueUid ""
      , ValueClientHostname "000280045079"
      ]
  ]

oneLease :: String -> Lease -> Assertion
oneLease filename expected = do
  bs <- B.readFile filename
  case AB.parseOnly (parser <* AB.endOfInput) bs of
    Left err -> fail ("parse failed with: " ++ err)
    Right actual -> assertEqual "Lease Equality" (LeaseWrap expected) (LeaseWrap actual)

oneLease' :: String -> [Lease] -> Assertion
oneLease' filename expected = do
  bs <- BSL.readFile filename
  case ((decodeLeases bs)) of
    Left err -> fail ("parse failed with: " ++ err)
    Right actual -> assertEqual "Lease Equality" (LeaseWraps expected) (LeaseWraps actual)

newtype LeaseWraps = LeaseWraps [Lease]
  deriving (Show)

instance Eq LeaseWraps where
  LeaseWraps [] == LeaseWraps [] = True
  LeaseWraps [] == LeaseWraps _  = False
  LeaseWraps _  == LeaseWraps [] = False
  LeaseWraps (x:xs) == LeaseWraps (y:ys) = leaseEq x y && (LeaseWraps xs == LeaseWraps ys)

newtype LeaseWrap = LeaseWrap Lease
  deriving (Show)

instance Eq LeaseWrap where
  LeaseWrap a == LeaseWrap b = leaseEq a b

leaseEq :: Lease -> Lease -> Bool
leaseEq (Lease ipA valsA) (Lease ipB valsB) =
  ipA == ipB && L.sort valsA == L.sort valsB
