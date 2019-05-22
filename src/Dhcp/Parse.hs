{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhcp.Parse
  ( decodeLeases
  , leasesToTimedHashMap
  , leasesToHashMap
  , TimedIPv4(..)
  ) where

import Chronos (parserUtf8_YmdHMS, datetimeToTime)
import Chronos.Types
import Control.Applicative
import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Dhcp.Types
import Net.Types
import Prelude hiding (rem)
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString.Lazy as ALB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BI
import qualified Data.HashMap.Strict as HM
import qualified Net.IPv4 as I4
import qualified Net.Mac as Mac

data TimedIPv4 = TimedIPv4 {-# UNPACK #-} !IPv4 {-# UNPACK #-} !Time

leasesToHashMap :: [Lease] -> HashMap Mac IPv4
leasesToHashMap = fmap (\(TimedIPv4 ip _) -> ip) . leasesToTimedHashMap

leasesToTimedHashMap :: [Lease] -> HashMap Mac TimedIPv4
leasesToTimedHashMap = foldr h HM.empty
  where
  h :: Lease -> HashMap Mac TimedIPv4 -> HashMap Mac TimedIPv4
  h (Lease ip vals) hm = case findMacAndTime vals of
    Nothing -> hm
    Just (mac,time) -> HM.alter g mac hm
      where
      g :: Maybe TimedIPv4 -> Maybe TimedIPv4
      g Nothing = Just (TimedIPv4 ip time)
      g (Just prevPair@(TimedIPv4 _ prevTime)) = Just $ if time > prevTime
        then TimedIPv4 ip time
        else prevPair

findMacAndTime :: [Value] -> Maybe (Mac,Time)
findMacAndTime = hasNeither
  where
    hasNeither [] = Nothing
    hasNeither (v : vs) = case v of
      ValueStarts t -> hasTime t vs
      ValueHardware h -> hasMac (hardwareMac h) vs
      _ -> hasNeither vs
    hasTime _ [] = Nothing
    hasTime t (v : vs) = case v of
      ValueHardware h -> Just (hardwareMac h,t)
      _ -> hasTime t vs
    hasMac _ [] = Nothing
    hasMac m (v : vs) = case v of
      ValueStarts t -> Just (m,t)
      _ -> hasMac m vs

ipv4 :: Parser IPv4
ipv4 = skipSpace *> I4.parserUtf8 <* skipSpace

lease :: Parser Lease
lease = AB.peekChar >>= \case
  Nothing -> fail $
    "Attoparsec.peekChar failure: unable to take "
    ++ "next character when parsing lease file."
  Just c -> if c == '#'
    then comment *> lease
    else pure Lease
      <* string "lease"
      <*> ipv4
      <* leftCurly
      <* skipSpace
      <*> go []
  where
    go vs = nextValue >>= \case
      NextValueAbsent -> pure vs
      NextValuePresent v -> go (v : vs)

string :: B.ByteString -> Parser ()
string b = AB.skipSpace *> AB.string b *> AB.skipSpace

datum :: B.ByteString -> Parser a -> Parser a
datum b p = string b *> p <* AB.skipSpace

comment :: Parser ()
comment = AB.takeTill (== '\n') *> AB.skipSpace

semicolon :: Parser ()
semicolon = () <$ AB.char ';'

leftCurly :: Parser ()
leftCurly = () <$ AB.char '{'

rightCurly :: Parser ()
rightCurly = () <$ AB.char '}'

skipSpace :: Parser ()
skipSpace = AB.skipSpace

nextName :: Parser NextName
nextName = (string "starts" $> NextNamePresent NameStarts)
  <|> (string "ends" $> NextNamePresent NameEnds)
  <|> (string "tstp" $> NextNamePresent NameTstp)
  <|> (string "atsfp" $> NextNamePresent NameAtsfp)
  <|> (string "cltt" $> NextNamePresent NameCltt)
  <|> (string "binding state" $> NextNamePresent NameBindingState)
  <|> (string "next binding state" $> NextNamePresent NameNextBindingState)
  <|> (string "hardware" $> NextNamePresent NameHardware)
  <|> (string "uid" $> NextNamePresent NameUid)
  <|> (string "client-hostname" $> NextNamePresent NameClientHostname)
  <|> (rightCurly $> NextNameAbsent)

nextValue :: Parser NextValue
nextValue = nextName >>= \case
  NextNameAbsent -> AB.skipSpace *> pure NextValueAbsent 
  NextNamePresent name -> do
    AB.skipSpace
    value <- case name of
      NameStarts -> fmap ValueStarts $
        ((time <* skipSpace <* semicolon)
        <?> "failed to parse 'starts' statement")
      NameEnds -> fmap ValueEnds $
        ((time <* skipSpace <* semicolon)
        <?> "failed to parse 'ends' statement")
      NameTstp -> fmap ValueTstp $
        ((time <* skipSpace <* semicolon)
        <?> "failed to parse 'tstp' statement")
      NameAtsfp -> fmap ValueAtsfp $
        ((time <* skipSpace <* semicolon)
        <?> "failed to parse 'atsfp' statement")
      NameCltt -> fmap ValueCltt $
        ((time <* skipSpace <* semicolon)
        <?> "failed to parse 'namecltt' statement")
      NameBindingState -> fmap ValueBindingState $
        ((bindingState <* skipSpace <* semicolon)
        <?> "failed to parse 'binding state' statement")
      NameNextBindingState -> fmap ValueNextBindingState $
        ((bindingState <* skipSpace <* semicolon)
        <?> "failed to parse 'next binding state' statement")
      NameHardware -> fmap ValueHardware $
        ((hardware <* skipSpace <* semicolon)
        <?> "failed to parse 'hardware' statement")
      NameUid -> fmap ValueUid $
        (uid <?> "failed to parse 'uid' statement") 
      NameClientHostname -> fmap ValueClientHostname $
        ((clientHostname <* skipSpace <* semicolon)
        <?> "failed to parse 'client-hostname' statement")
    skipSpace
    skipSpace
    pure (NextValuePresent value)

uid :: Parser ByteString
uid = B.empty <$ AB.takeTill (== '\n')

clientHostname :: Parser Text
clientHostname = do
  _ <- AB.char '"'
  bs <- AB.takeTill (== '"')
  _ <- AB.anyChar
  case decodeUtf8' bs of
    Left _ -> fail "client hostname not UTF-8"
    Right name -> pure name

hardware :: Parser Hardware
hardware = Hardware
  <$> (AB.takeWhile1 (/= ' ') >>= \bs -> case decodeUtf8' bs of
         Left _ -> fail "hardware name not UTF-8"
         Right name -> pure name
      )
  <* AB.anyChar
  <*> (Mac.parserWithUtf8 (MacCodec (MacGroupingPairs ':') False))

bindingState :: Parser BindingState
bindingState = (string "active" $> BindingStateActive)
  <|> (string "free" $> BindingStateFree)
  <|> (string "abandoned" $> BindingStateAbandoned)

time :: Parser Time
time = pure datetimeToTime
  <* AB.decimal
  <* AB.skipSpace
  <*> parserUtf8_YmdHMS (DatetimeFormat (Just '/') (Just ' ') (Just ':'))

-- | Either returns what it was able to parse thus far, plus an error message,
--   or just everything parsed
debug :: Parser a -> BL.ByteString -> [a] -> Int -> Either ([a],String) [a]
debug psr bs xs !i = case ALB.parse psr bs of
  ALB.Done (BI.Empty) r -> Right (r:xs)
  ALB.Done rem r -> debug psr rem (r:xs) (i+1)
  ALB.Fail _ ss s ->
    let showStrs [] _ = ""
        showStrs (c:cs) n = "\n    Context " ++ show n ++ ": " ++ c
          ++ showStrs cs (n+1)
        errMsg = "failed at least number: "
          ++ show (if i > 6 then i - 6 else i)
          ++ "\n  (n.b.: This number is 1-indexed)"
          ++ "\n  Original error message from attoparsec: " ++ s
          ++ "\n  Contextual error message from attoparsec: " ++ showStrs ss 0
    in Left (xs,errMsg)

decodeLeases :: BL.ByteString -> Either ([Lease],String) [Lease]
decodeLeases bs = debug lease bs [] 1
