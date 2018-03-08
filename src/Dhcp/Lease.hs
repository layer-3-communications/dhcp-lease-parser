{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module Dhcp.Lease
  ( parser
  , decodeLeases 
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString.Lazy as ALB
--import qualified Data.ByteString.Utf8 as BUtf8
import qualified Net.IPv4 as I4
import qualified Net.Mac as Mac
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import Control.Monad (when)
import Data.Char (ord,chr)
import Data.Text.Encoding (decodeUtf8')
import Dhcp.Types
import Control.Applicative
import Data.Functor
import Data.ByteString (ByteString)
import Data.Text (Text)
import Net.Types
import Chronos (parserUtf8_YmdHMS, datetimeToTime)
import Chronos.Types
import Prelude hiding (rem)

parser :: BCParser Lease
parser = do
  m <- AB.peekChar
  case m of
    Nothing -> pure $ Lease I4.any []
    Just c  -> if c == '#'
      then comment >> parser
      else do 
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
parserLeases :: BCParser [Lease]
parserLeases = go id
  where
  go :: ([Lease] -> [Lease]) -> BCParser [Lease]
  go diffList = do
    m <- AB.peekChar
    case m of
      Nothing -> pure (diffList [])
      Just c -> if c == '#'
        then comment >> go diffList
        else do
          lease <- parser
          go ((lease :) . diffList)

comment :: BCParser ()
comment = do
  _ <- AB.takeTill (== '\n')
  AB.skipSpace

parserValue :: BCParser NextValue
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
        NameUid -> ValueUid <$> skipUid
        NameClientHostname -> ValueClientHostname <$> parserClientHostname
      AB.skipSpace
      _ <- AB.char ';'
      AB.skipSpace
      pure (NextValuePresent value)

skipUid :: BCParser ByteString
skipUid = pure B.empty
  
-- | This doesn't actually work yet. It doesn't escape octal codes.
parserUid :: BCParser ByteString
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

possiblyOctal :: BCParser NextChar
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

parserClientHostname :: BCParser Text
parserClientHostname = do
  _ <- AB.char '"'
  bs <- AB.takeTill (== '"')
  _ <- AB.anyChar
  case decodeUtf8' bs of
    Left _ -> fail "client hostname name not UTF-8"
    Right name -> pure name

parserHardware :: BCParser Hardware
parserHardware = Hardware
  <$> (do
        bs <- AB.takeWhile1 (/= ' ')
        case decodeUtf8' bs of
          Left _ -> fail "hardware name not UTF-8"
          Right name -> pure name
      ) 
  <*  AB.anyChar
  <*> Mac.parserWithUtf8 (MacCodec (MacGroupingPairs ':') False)

parserBindingState :: BCParser BindingState
parserBindingState =
      (AB.string "active" $> BindingStateActive)
  <|> (AB.string "free" $> BindingStateFree)

parserTime :: BCParser Time
parserTime = do
  _ <- AB.decimal :: BCParser Int
  AB.skipSpace
  dt <- parserUtf8_YmdHMS (DatetimeFormat (Just '/') (Just ' ') (Just ':'))
  pure (datetimeToTime dt)

debug :: BCParser a -> LazyByteString -> [a] -> Int -> Either String [a]
debug psr bs xs i = case ALB.parse psr bs of
  ALB.Fail _ _ s ->
    Left $ "failed at input number" ++ (show i)
      ++ ", original error message: " ++ s
  ALB.Done rem r -> debug psr rem (r : xs) (i + 1)

decodeLeases :: LazyByteString -> Either String [Lease]
decodeLeases bs = debug parser bs [] 0 

--ALB.maybeResult . ALB.parse (parserLeases <* AB.endOfInput)
