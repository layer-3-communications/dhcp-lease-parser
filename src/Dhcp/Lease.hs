{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Dhcp.Lease
  ( Lease(..)
  , Value(..)
  , Hardware(..)
  , BindingState(..)
  , parser
  , decodeLeases 
  ) where

import qualified Text.Parser.Token  as TPT
import qualified Text.Parser.Char as TPC
import qualified Text.Trifecta.Parser as TTP
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString as ABB
import qualified Data.Attoparsec.ByteString.Lazy as ALB
import qualified Net.IPv4 as I4
import qualified Net.Mac as Mac
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord,chr)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.ByteString (ByteString)
import Data.Text (Text)
import Net.Types
import Chronos (parserUtf8_YmdHMS, datetimeToTime)
import Chronos.Types

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
  | ValueUid              !ByteString
  | ValueClientHostname   !Text
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
  { hardwareType :: !Text
  , hardwareMac  :: !Mac
  } deriving (Eq,Ord,Show,Read)

parserIPv4 :: TTP.Parser I4.IPv4
parserIPv4 = I4.fromOctets
  <$> (TPT.natural >>= limitSize)
  <*  TPC.char '.'
  <*> (TPT.natural >>= limitSize)
  <*  TPC.char '.'
  <*> (TPT.natural >>= limitSize)
  <*  TPC.char '.'
  <*> (TPT.natural >>= limitSize)
  where
    limitSize i =
      if (i < 0 || i > 255)
        then fail ("All octets in an ipv4 address must be between 0 and 255. Found: " ++ (show i))
        else pure $ integerToWord8 i

integerToWord8 :: Integer -> Word8
integerToWord8 i = fromIntegral i
{-# INLINE integerToWord8 #-}

--parser' :: TTP.Parser Lease
--parser' = do
--  _ <- TPC.string "lease"
--  TPC.spaces
  
parser :: AB.Parser Lease
parser = do
  _ <- AB.string "lease"
  AB.skipSpace
  ip <- I4.parserUtf8
  AB.skipSpace
  _ <- AB.char '{'
  AB.skipSpace
  let go vs = do
        nv <- parserValue
        case nv of
          NextValueAbsent -> pure vs
          NextValuePresent v -> go (v : vs)
  vals <- go []
  pure (Lease ip vals)

-- | Parse as many leases as possible. Also,
--   strip comments at the start of the input and between
--   leases.
parserLeases :: AB.Parser [Lease]
parserLeases = go id
  where
  go :: ([Lease] -> [Lease]) -> AB.Parser [Lease]
  go diffList = do
    m <- AB.peekChar
    case m of
      Nothing -> pure (diffList [])
      Just c -> if c == '#'
        then comment >> go diffList
        else do
          lease <- parser
          go ((lease :) . diffList)

comment :: AB.Parser ()
comment = do
  _ <- AB.takeTill (== '\n')
  AB.skipSpace

parseValue :: TTP.Parser NextValue
parseValue = do
  nname
     <- (TPC.string "starts" $> NextNamePresent NameStarts)
    <|> (TPC.string "tstp" $> NextNamePresent NameTstp)
    <|> (TPC.string "atsfp" $> NextNamePresent NameAtsfp)
    <|> (TPC.string "cltt" $> NextNamePresent NameCltt)
    <|> (TPC.string "binding state" $> NextNamePresent NameBindingState)
    <|> (TPC.string "next binding state" $> NextNamePresent NameNextBindingState)
    <|> (TPC.string "hardware" $> NextNamePresent NameHardware)
    <|> (TPC.string "uid" $> NextNamePresent NameUid)
    <|> (TPC.string "client-hostname" $> NextNamePresent NameClientHostname)
    <|> (TPC.char '}' $> NextNameAbsent)
  case nname of
    NextNameAbsent -> do
      TPC.spaces 
      pure NextValueAbsent
    NextNamePresent name -> do
      TPC.spaces 
      value <- case name of
        NameStarts -> ValueStarts <$> parseTime
        NameEnds -> ValueEnds <$> parseTime
        NameTstp -> ValueTstp <$> parseTime
        NameAtsfp -> ValueAtsfp <$> parseTime
        NameCltt -> ValueCltt <$> parseTime
        NameBindingState -> ValueBindingState <$> parseBindingState
        NameNextBindingState -> ValueNextBindingState <$> parseBindingState
        NameHardware -> ValueHardware <$> parseHardware
        NameUid -> ValueUid <$> parseUid
        NameClientHostname -> ValueClientHostname <$> parseClientHostname
      TPC.spaces
      _ <- TPC.char ';'
      TPC.spaces
      pure (NextValuePresent value)

parseBindingState = undefined
parseHardware = undefined
parseUid = undefined
parseClientHostname = undefined


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
      pure NextValueAbsent
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
      pure (NextValuePresent value)

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
              NextCharDone -> pure cs
              NextCharAgain c -> go (c : cs)
      chars <- go []
      pure (B.reverse (BC.pack chars))

octalErrorMessage :: String
octalErrorMessage = "invalid octal escape sequence while parsing uid"

c2i :: Char -> Int
c2i = ord

possiblyOctal :: AB.Parser NextChar
possiblyOctal = do
  c <- AB.anyChar
  case c of
    '"' -> pure NextCharDone
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
      pure (NextCharAgain (chr (i1 * 64 + i2 * 8 + i3)))
    _ -> pure (NextCharAgain c)

data NextChar = NextCharAgain {-# UNPACK #-} !Char | NextCharDone

parserClientHostname :: AB.Parser Text
parserClientHostname = do
  _ <- AB.char '"'
  bs <- AB.takeTill (== '"')
  _ <- AB.anyChar
  case decodeUtf8' bs of
    Left _ -> fail "client hostname name not UTF-8"
    Right name -> pure name

parserHardware :: AB.Parser Hardware
parserHardware = Hardware
  <$> (do
        bs <- AB.takeWhile1 (/= ' ')
        case decodeUtf8' bs of
          Left _ -> fail "hardware name not UTF-8"
          Right name -> pure name
      ) 
  <*  AB.anyChar
  <*> Mac.parserWithUtf8 (MacCodec (MacGroupingPairs ':') False)

parserBindingState :: AB.Parser BindingState
parserBindingState =
      (AB.string "active" $> BindingStateActive)
  <|> (AB.string "free" $> BindingStateFree)

parseTime :: TTP.Parser Time
parseTime = do
  _ <- TPT.integer
  TPC.spaces
  dt <- parseUtf8YmdHms (DatetimeFormat (Just '/') (Just ' ') (Just ':'))
  pure (datetimeToTime dt)

parseUtf8YmdHms :: DatetimeFormat -> TTP.Parser Datetime
parseUtf8YmdHms = undefined

parserTime :: AB.Parser Time
parserTime = do
  _ <- AB.decimal :: AB.Parser Int
  AB.skipSpace
  dt <- parserUtf8_YmdHMS (DatetimeFormat (Just '/') (Just ' ') (Just ':'))
  pure (datetimeToTime dt)

decodeLeases :: LB.ByteString -> Maybe [Lease]
decodeLeases = ALB.maybeResult . ALB.parse (parserLeases <* AB.endOfInput)
