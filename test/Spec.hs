{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Dhcp.Lease
import Data.Monoid

import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac
import qualified Chronos as Chronos
import qualified Chronos as PX
import qualified Data.Attoparsec.ByteString as AB

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
      , ValueUid $ "\o001\o000\o002\o200\o004" <> "0c"
      , ValueClientHostname "000280043063"
      ]
  , testCase "002" $ oneLease "sample/002.txt" $ Lease
      (IPv4.fromOctets 10 102 18 226)
      [ ValueStarts $ Chronos.timeFromYmdhms 2017 5 18 5 40 43
      , ValueEnds   $ Chronos.timeFromYmdhms 2017 05 19 0 40 43
      , ValueBindingState BindingStateActive
      , ValueNextBindingState BindingStateFree
      , ValueHardware $ Hardware "ethernet" $ Mac.fromOctets 0x00 0x02 0xA1 0x23 0x03 0x80
      , ValueUid "\o001\o000\o002\o241#\o003\o200"
      ]
  ]

oneLease :: String -> Lease -> Assertion
oneLease filename expected = do
  bs <- B.readFile filename
  case AB.parseOnly (parser <* AB.endOfInput) bs of
    Left err -> fail ("parse failed with: " ++ err)
    Right actual -> assertEqual "Lease Equality" (LeaseWrap expected) (LeaseWrap actual)

newtype LeaseWrap = LeaseWrap Lease
  deriving (Show)

instance Eq LeaseWrap where
  LeaseWrap a == LeaseWrap b = leaseEq a b

leaseEq :: Lease -> Lease -> Bool
leaseEq (Lease ipA valsA) (Lease ipB valsB) =
  ipA == ipB && L.sort valsA == L.sort valsB

