{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Dhcp.Lease
  ( Lease(..)
  , Value(..)
  , Hardware(..)
  , BindingState(..)
  , parser
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString as ABB
import qualified Net.IPv4.ByteString.Char8 as IP
import qualified Net.Mac.ByteString.Char8 as Mac
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord,chr)
import Data.Text.Encoding (decodeUtf8')
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.ByteString (ByteString)
import Data.Text (Text)
import Net.Types
import Chronos.Datetime.ByteString.Char7 (parser_YmdHMS)
import Chronos.Posix (fromDatetime)
import Chronos.Types

data Lease = Lease 
  { leaseIp :: !IPv4
  , leaseValues :: ![Value]
  } deriving (Show,Read)

data Value 
  = ValueStarts !PosixTime
  | ValueEnds !PosixTime
  | ValueTstp !PosixTime
  | ValueAtsfp !PosixTime
  | ValueCltt !PosixTime
  | ValueBindingState !BindingState
  | ValueNextBindingState !BindingState
  | ValueHardware !Hardware
  | ValueUid !ByteString
  | ValueClientHostname !Text
  deriving (Eq,Ord,Show,Read)

data NextValue = NextValuePresent !Value | NextValueAbsent
data NextName = NextNamePresent !Name | NextNameAbsent

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

data BindingState = BindingStateFree | BindingStateActive
  deriving (Eq,Ord,Show,Read)
data Hardware = Hardware
  { hardwareType :: !Text
  , hardwareMac :: !Mac
  } deriving (Eq,Ord,Show,Read)

parser :: AB.Parser Lease
parser = do
  _ <- AB.string "lease"
  AB.skipSpace
  ip <- IP.parser
  AB.skipSpace
  _ <- AB.char '{'
  AB.skipSpace
  let go vs = do
        nv <- parserValue
        case nv of
          NextValueAbsent -> return vs
          NextValuePresent v -> go (v : vs)
  vals <- go []
  return (Lease ip vals)

parserValue :: AB.Parser NextValue
parserValue = do
  nname <- (AB.string "starts" $> NextNamePresent NameStarts)
    <|> (AB.string "ends" $> NextNamePresent NameEnds)
    <|> (AB.string "tstp" $> NextNamePresent NameTstp)
    <|> (AB.string "atsfp" $> NextNamePresent NameAtsfp)
    <|> (AB.string "cltt" $> NextNamePresent NameCltt)
    <|> (AB.string "binding state" $> NextNamePresent NameBindingState)
    <|> (AB.string "next binding state" $> NextNamePresent NameNextBindingState)
    <|> (AB.string "hardware" $> NextNamePresent NameHardware)
    <|> (AB.string "uid" $> NextNamePresent NameUid)
    <|> (AB.string "client-hostname" $> NextNamePresent NameClientHostname)
    <|> (AB.char '}' $> NextNameAbsent)
  case nname of
    NextNameAbsent -> do
      AB.skipSpace
      return NextValueAbsent
    NextNamePresent name -> do
      AB.skipSpace
      value <- case name of
        NameStarts -> ValueStarts <$> parserTime
        NameEnds -> ValueEnds <$> parserTime
        NameTstp -> ValueTstp <$> parserTime
        NameAtsfp -> ValueAtsfp <$> parserTime
        NameCltt -> ValueCltt <$> parserTime
        NameBindingState -> ValueBindingState <$> parserBindingState
        NameNextBindingState -> ValueNextBindingState <$> parserBindingState
        NameHardware -> ValueHardware <$> parserHardware
        NameUid -> ValueUid <$> parserUid
        NameClientHostname -> ValueClientHostname <$> parserClientHostname
      AB.skipSpace
      _ <- AB.char ';'
      AB.skipSpace
      return (NextValuePresent value)

-- | This doesn't actually work yet. It doesn't espace octal codes.
parserUid :: AB.Parser ByteString
parserUid = do
  isMac <- (AB.char '"' $> False ) <|> pure True
  if isMac
    then AB.takeTill (\x -> not (x == ':' || (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')))
    else do
      let go !cs = do
            n <- possiblyOctal
            case n of
              NextCharDone -> return cs
              NextCharAgain c -> go (c : cs)
      chars <- go []
      return (B.reverse (BC.pack chars))

octalErrorMessage :: String
octalErrorMessage = "invalid octal escape sequence while parsing uid"

c2i :: Char -> Int
c2i = ord

possiblyOctal :: AB.Parser NextChar
possiblyOctal = do
  c <- AB.anyChar
  case c of
    '"' -> return NextCharDone
    '\\' -> do
      c1 <- AB.anyChar
      let i1 = c2i c1 - 48
      when (i1 < 0 || i1 > 3) $ fail octalErrorMessage
      c2 <- AB.anyChar
      let i2 = c2i c2 - 48
      when (i2 < 0 || i2 > 7) $ fail octalErrorMessage
      c3 <- AB.anyChar
      let i3 = c2i c3 - 48
      when (i3 < 0 || i3 > 7) $ fail octalErrorMessage
      return (NextCharAgain (chr (i1 * 64 + i2 * 8 + i3)))
    _ -> return (NextCharAgain c)

data NextChar = NextCharAgain {-# UNPACK #-} !Char | NextCharDone

parserClientHostname :: AB.Parser Text
parserClientHostname = do
  _ <- AB.char '"'
  bs <- AB.takeTill (== '"')
  _ <- AB.anyChar
  case decodeUtf8' bs of
    Left _ -> fail "client hostname name not UTF-8"
    Right name -> return name

parserHardware :: AB.Parser Hardware
parserHardware = Hardware
  <$> (do
        bs <- AB.takeWhile1 (/= ' ')
        case decodeUtf8' bs of
          Left _ -> fail "hardware name not UTF-8"
          Right name -> return name
      ) 
  <*  AB.anyChar
  <*> Mac.parserWith (MacCodec (MacGroupingPairs ':') False)

parserBindingState :: AB.Parser BindingState
parserBindingState =
      (AB.string "active" $> BindingStateActive)
  <|> (AB.string "free" $> BindingStateFree)

parserTime :: AB.Parser PosixTime
parserTime = do
  _ <- AB.decimal :: AB.Parser Int
  AB.skipSpace
  dt <- parser_YmdHMS (DatetimeFormat (Just '/') (Just ' ') (Just ':'))
  return (fromDatetime dt)
  



