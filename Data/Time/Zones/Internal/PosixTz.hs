{-# LANGUAGE OverloadedStrings #-}

-- | Parsing and rendering of POSIX-TZ style rules
module Data.Time.Zones.Internal.PosixTz where

import Control.Monad (unless)
import Data.Word
import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.Time.Zones.Types

-- | Default time for DST rules
--
-- "The time fields specify when, in the local time currently in effect, the change to the other time occurs.
--  If omitted, the default is 02:00:00."
defaultDstRuleTime :: Int
defaultDstRuleTime = 7200

-- | Parse POSIX-TZ
--
-- User should ensure input size is properly limited.
--
-- >>> fmap renderPosixTz $ parsePosixTz "UTC0" -- Etc/Utc
-- Right "UTC0"
--
-- >>> renderPosixTz <$> parsePosixTz "CET-1CEST,M3.5.0,M10.5.0/3" -- Europe/Berlin
-- Right "CET-1CEST,M3.5.0,M10.5.0/3"
-- 
-- >>> renderPosixTz <$> parsePosixTz "<+1030>-10:30<+11>-11,M10.1.0,M4.1.0" -- Australia/Lord_Howe
-- Right "<+1030>-10:30<+11>-11,M10.1.0,M4.1.0"
parsePosixTz :: B.ByteString -> Either String PosixTz
parsePosixTz = parseOnly posixTzParser
{-# INLINE parsePosixTz #-}

posixTzParser :: Parser PosixTz
posixTzParser = do
  std <- zone
  stdoff <- offset
  mdst <- (Nothing <$ endOfInput) <|> (Just <$> dstspec stdoff)
  pure $ PosixTz (PosixZone std stdoff) mdst

zone :: Parser B.ByteString
zone = angleZone <|> simpleZone
  where
    angleZone = do
      _ <- char '<'
      x <- takeWhile1 (\c -> c /= '>' && c /= '\NUL')
      _ <- char '>'
      pure $ "<" <> x <> ">"
    simpleZone = takeWhile1 isSimpleChar
    isSimpleChar c =
         c /= ':' && c /= ',' && c /= '-' && c /= '+' -- is not explicitly banned
      && (c < '0' || c > '9') -- is not a digit
      && c /= '\NUL'

-- zone offset hour part must be in range [-24,24]
-- note docs say [0, 24] but they don't treat the sign as part of the number
offset :: Parser Int
offset = hms (\h -> h >= -24 && h <= 24)

-- as an extension to POSIX time hour part can be in range [-167, 167]
time :: Parser Int
time = hms (\h -> h >= -167 && h <= 167)

hms :: (Int -> Bool) -> Parser Int
hms hpred = do
  h <- signed (decimaln 3)
  unless (hpred h) $ fail "hours out of range"
  m <- option 0 (char ':' *> decimaln 2)
  unless (m >= 0 && m <= 59) $ fail "minutes out of range [0,59]"
  s <- option 0 (char ':' *> decimaln 2)
  unless (s >= 0 && s <= 59) $ fail "seconds out of range [0,59]"
  pure $ 3600*h + (if h > 0 then 1 else (-1)) * (60*m + s)

-- | Parse DST spec
dstspec
  :: Int -- std offset, needed to calculate default dst offset, if not explicitly given
  -> Parser (PosixZone, TzRule, TzRule)
dstspec stdoff = do
  dst <- zone
  -- If no offset follows dst, summer time is assumed to be one hour ahead of standard time.
  dstoff <- option (stdoff - 3600) offset
  _ <- char ','
  start <- tzrule
  _ <- char ','
  end <- tzrule
  pure (PosixZone dst dstoff, start, end)

-- | TZ rule
tzrule :: Parser TzRule
tzrule = do
  r <- jnrule <|> nrule <|> mrule
  t <- option defaultDstRuleTime $ char '/' *> time
  pure $ r { _tzrTime = t }

jnrule :: Parser TzRule
jnrule = do
  _ <- char 'J'
  n <- decimaln 3
  unless (n >= 1 && n <= 365) $ fail "date Jn: Julian day out of range [1,365]"
  pure $ TzRule TzRuleJ 0 0 n 0

nrule :: Parser TzRule
nrule = do
  n <- decimaln 3
  unless (n >= 0 && n <= 365) $ fail "date n: Julian day out of range [0,365]"
  pure $ TzRule TzRuleJ 0 0 n 0

mrule :: Parser TzRule
mrule = do
  _ <- char 'M'
  m <- decimaln 2
  unless (m >= 1 && m <= 12) $ fail "month number out of range [1,12]"
  _ <- char '.'
  n <- digitint
  unless (n >= 1 && n <= 5) $ fail "week number out of range [1,5]"
  _ <- char '.'
  d <- digitint
  unless (d >= 0 && d <= 6) $ fail "day number out of range [0,6]"
  pure $ TzRule TzRuleM m n d 0


-- Custom decimal parsers provided below. 'decimal' does not
-- check for overflow and we don't want to go through Integer
--
-- >>> flip feed "" $ parse decimal "257" :: Result Word8
-- Done "" 1
--

digitint :: Parser Int
digitint = fromIntegral . (\w -> w - 48) <$> AB.satisfy isDigitW8

-- this is only safe if maxlen digits can always fit into a
decimaln :: Integral a => Int -> Parser a
decimaln maxlen = do
  b <- AB.takeWhile1 isDigitW8
  unless (B.length b <= maxlen) $ fail "decimaln: found more digits than expected"
  pure $ B.foldl' step 0 (B.take maxlen b)
  where
    step a w = a * 10 + fromIntegral (w - 48)

isDigitW8 :: Word8 -> Bool
isDigitW8 w = w >= 48 && w <= 57


-- | Render 'PosixTz'
--
-- >>> renderPosixTz utcPosixTz
-- "UTC0"
--
-- Europe/Berlin CET-1CEST,M3.5.0,M10.5.0/3
-- >>> renderPosixTz (PosixTz (PosixZone "CET" (-3600)) (Just (PosixZone "CEST" (-2*3600), TzRule TzRuleM 3 0 5 7200, TzRule TzRuleM 10 0 5 (3*3600))))
-- "CET-1CEST,M3.0.5,M10.0.5/3"
--
-- >>> renderPosixTz (PosixTz (PosixZone "EST" (5*3600)) (Just (PosixZone "EDT" (4*3600), TzRule TzRuleM 3 2 0 (2*3600), TzRule TzRuleM 11 1 0 (2*3600))))
-- "EST5EDT,M3.2.0,M11.1.0"
--
-- Australia/Lord_Howe <+1030>-10:30<+11>-11,M10.1.0,M4.1.0
-- >>> renderPosixTz (PosixTz (PosixZone "<+1030>" (-10*3600 - 1800)) (Just (PosixZone "<+11>" (-11*3600), TzRule TzRuleM 10 1 0 (2*3600), TzRule TzRuleM 4 1 0 (2*3600))))
-- "<+1030>-10:30<+11>-11,M10.1.0,M4.1.0"
renderPosixTz :: PosixTz -> B.ByteString
renderPosixTz (PosixTz (PosixZone std stdoff) mdst) =
  mconcat
    [ std
    , renderOffset stdoff
    , maybe "" renderDst mdst
    ]
  where
    renderOffset x | x >= 0 = renderHms x
    renderOffset x = "-" <> renderHms (-x)

    renderTime x | x >= 0 = renderHms x
    renderTime x = "-" <> renderHms (-x)

    renderHms x =
      let (h, m, s) = tohms x
      in mconcat
        [ bshow h
        , if m /= 0 || s /= 0
          then ":" <> bshow m
          else ""
        , if s /= 0
          then ":" <> bshow s
          else ""
        ]

    tohms x =
      let (h, r) = x `divMod` 3600
          (m, s) = r `divMod` 60
      in (h, m, s)

    renderDst (PosixZone dst dstoff, start, end) =
      mconcat
        [ dst
        , if dstoff /= (stdoff - 3600)
          then renderOffset dstoff
          else ""
        , ","
        , renderTzRule start
        , ","
        , renderTzRule end
        ]

    renderTzRule (TzRule ty m n d t) =
      mconcat
        [ case ty of
            TzRuleJ -> "J" <> bshow d
            TzRuleN -> bshow d
            TzRuleM -> "M" <> bshow m <> "." <> bshow n <> "." <> bshow d
        , if t /= defaultDstRuleTime then "/" <> renderTime t else ""
        ]

    bshow :: (Show a) => a -> B.ByteString
    bshow = B8.pack . show
