{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Dhcp.Lease
  ( parser
  , decodeLeases 
  ) where

import Data.Attoparsec.ByteString ((<?>))
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
    Nothing -> pure $ emptyLease
    Just c  -> if c == '#'
      then comment >> parser
      else do 
        _ <- AB.string "lease"
        AB.skipSpace
        ip <- I4.parserUtf8
        if (notOntRange ip)
          then do
            AB.skipSpace
            _ <- AB.char '{'
            AB.skipSpace
            skipFieldMany
            pure $ emptyLease
          else do 
            AB.skipSpace
            _ <- AB.char '{'
            AB.skipSpace
            vals <- go []
            pure (Lease ip vals)
  where
    go :: [Value] -> BCParser [Value]
    go vs = do
      nv <- parserValue
      case nv of
        NextValueAbsent -> pure vs
        NextValuePresent v -> go (v : vs)

skipField :: BCParser ()
skipField = do
  _ <- AB.takeTill (== '\n')
  AB.skipSpace

skipFieldMany :: BCParser ()
skipFieldMany = do
  m <- AB.peekChar
  case m of
    Nothing -> pure ()
    Just c  -> if c == '}'
      then do
        _ <- AB.char '}'
        AB.skipSpace 
      else skipField >> skipFieldMany

emptyLease :: Lease
emptyLease = Lease I4.any []

notOntRange :: IPv4 -> Bool
notOntRange = not . ontRange

ontRange :: IPv4 -> Bool
ontRange a = go (I4.toOctets a)
  where
    go (_,x,_,_) = (x < 150 || x > 160)

comment :: BCParser ()
comment = do
  _ <- AB.takeTill (== '\n')
  AB.skipSpace

parserValue :: BCParser NextValue
parserValue = do
  nname
     <- (AB.string "starts" $> NextNamePresent NameStarts)
    <|> (AB.string "ends"   $> NextNamePresent NameEnds)
    <|> (AB.string "tstp"   $> NextNamePresent NameTstp)
    <|> (AB.string "atsfp"  $> NextNamePresent NameAtsfp)
    <|> (AB.string "cltt"   $> NextNamePresent NameCltt)
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
        NameStarts -> ValueStarts <$> ((parserTime <* semicolon) <?> "starts error")
        NameEnds   -> ValueEnds   <$> ((parserTime <* semicolon) <?> "ends error")
        NameTstp   -> ValueTstp   <$> ((parserTime <* semicolon) <?> "tstp error")
        NameAtsfp  -> ValueAtsfp  <$> ((parserTime <* semicolon) <?> "atsfp error")
        NameCltt   -> ValueCltt   <$> ((parserTime <* semicolon) <?> "namecltt error")
        NameBindingState
                   -> ValueBindingState     <$> ((parserBindingState <* semicolon) <?> "binding state error")
        NameNextBindingState
                   -> ValueNextBindingState <$> ((parserBindingState <* semicolon) <?> "next binding state error")
        NameHardware
                   -> ValueHardware         <$> ((parserHardware     <* semicolon) <?> "hardware error")
        NameUid    -> ValueUid              <$> (skipUid <?> "uid error") 
        NameClientHostname
                   -> ValueClientHostname   <$> ((parserClientHostname <* semicolon) <?> "hostname error")
      AB.skipSpace
      AB.skipSpace
      pure (NextValuePresent value)

semicolon :: BCParser ()
semicolon = do
  _ <- AB.char ';'
  pure ()

skipUid :: BCParser ByteString
skipUid = do
  _ <- AB.takeTill (== '\n')
  pure B.empty

-- | This doesn't actually work yet. It doesn't escape octal codes.
parserUid :: BCParser ByteString
parserUid = do
  isMac <- (AB.char '"' $> False ) <|> pure True
  if isMac
    then AB.takeTill (\x -> not (x == ':' || (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')))
    else do
    _ <- AB.takeTill (== '\n')
    AB.skipSpace
    pure B.empty
    --do
    --  let go !cs = do
    --        n <- possiblyOctal
    --        case n of
    --          NextCharDone -> pure cs
    --          NextCharAgain c -> go (c : cs)
    --  chars <- go []
    --  pure (B.reverse (BC.pack chars))

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
  <*> 
    ((Mac.parserWithUtf8 (MacCodec (MacGroupingPairs ':') False))
    <|> (pure $ Mac.fromOctets 0 0 0 0 0 0))

parserBindingState :: BCParser BindingState
parserBindingState =
      (AB.string "active" $> BindingStateActive)
  <|> (AB.string "free"   $> BindingStateFree)

parserTime :: BCParser Time
parserTime = do
  _ <- AB.decimal :: BCParser Int
  AB.skipSpace
  dt <- parserUtf8_YmdHMS (DatetimeFormat (Just '/') (Just ' ') (Just ':'))
  pure (datetimeToTime dt)

debug :: BCParser a -> LazyByteString -> [a] -> Int -> Either String [a]
debug psr bs xs i = case ALB.parse psr bs of
  ALB.Fail _ ss s ->
    Left $ "failed at lease number: " ++ (show $ (\x -> if x > 5 then x - 5 else x) i)
      ++ "\n\t(n.b.: this number is 1-indexed)" 
      ++ "\nOriginal error message Attoparsec: " ++ s
      ++ "\nContextual error messages from Attoparsec: " ++ (showStrs ss 0)
  ALB.Done rem r -> debug psr rem (r : xs) (i + 1)
  where
    showStrs :: [String] -> Int -> String
    showStrs [] _ = ""
    showStrs (k:ks) n = "\nContext " ++ (show n) ++ " " ++ k ++ showStrs ks (n + 1)

decodeLeases :: LazyByteString -> Either String [Lease]
decodeLeases bs = debug parser bs [] 1 

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

--ALB.maybeResult . ALB.parse (parserLeases <* AB.endOfInput)
