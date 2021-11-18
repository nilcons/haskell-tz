{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Parsing and rendering of POSIX-TZ style rules
module Data.Time.Zones.Internal.PosixTz where

import Prelude hiding (succ)
import Control.Applicative ( Alternative(..) )
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

#if !(MIN_VERSION_base(4,13,0))
import qualified Control.Monad.Fail as Fail
#endif

import Data.Time.Zones.Types

-- | Default time for DST rules
--
-- "The time fields specify when, in the local time currently in effect, the change to the other time occurs.
--  If omitted, the default is 02:00:00."
defaultDstRuleTime :: Int
defaultDstRuleTime = 7200


-- | Parse POSIX-TZ
--
-- >>> parsePosixTz "CET-1CEST,M3.5.0,M10.5.0"
-- Right (PosixTz {_posixTzStd = PosixZone {_pzName = "CET", _pzOffset = -3600}, _posixTzDst = Just (PosixZone {_pzName = "CEST", _pzOffset = -7200},TzRule {_tzrType = TzRuleM, _tzrMon = 3, _tzrNum = 5, _tzrDay = 0, _tzrTime = 7200},TzRule {_tzrType = TzRuleM, _tzrMon = 10, _tzrNum = 5, _tzrDay = 0, _tzrTime = 7200})})
-- >>> parsePosixTz "CET-1CEST,25/26:00:00,J75"
parsePosixTz :: ByteString -> Either String PosixTz
parsePosixTz = parse (posixTz <* endOfInput)
{-# INLINE parsePosixTz #-}

posixTz :: Parser PosixTz
posixTz = do
  std <- zone
  stdoff <- offset
  mdst <- (Nothing <$ endOfInput) <|> (Just <$> dstspec stdoff)
  pure $ PosixTz (PosixZone std stdoff) mdst

zone :: Parser ByteString
zone = do
  c <- lookupChar
  if c == '<'
    then anglezone
    else simplezone
  where
    anglezone = do
      _ <- char '<'
      s <- takeWhile1n 10 (\c -> c /= '>' && c /= '\NUL')
      _ <- char '>'
      pure $ B8.cons '<' $ B8.snoc s '>'
    simplezone = takeWhile1n 6 isSimpleChar
    isSimpleChar c =
         c /= ':' && c /= ',' && c /= '-' && c /= '+' -- is not explicitly banned
      && (c < '0' || c > '9') -- is not a digit
      && c /= '\NUL'

-- | Zone offset
-- zone offset hour part must be in range [-24,24]
-- note docs say [0, 24] but they don't treat the sign as part of the number
offset :: Parser Int
offset = hms (\h -> h >= -24 && h <= 24)

-- | TZ rule time
-- as an extension to POSIX time hour part can be in range [-167, 167]
time :: Parser Int
time = hms (\h -> h >= -167 && h <= 167)

hms :: (Int -> Bool) -> Parser Int
hms hpred = do
  h <- signed 3
  unless (hpred h) $ fail "hours out of range"
  m <- option 0 (char ':' *> decimal 2)
  unless (m >= 0 && m <= 59) $ fail "minutes out of range [0,59]"
  s <- option 0 (char ':' *> decimal 2)
  unless (s >= 0 && s <= 59) $ fail "seconds out of range [0,59]"
  pure $ 3600*h + (if h > 0 then 1 else (-1)) * (60*m + s)

-- | DST spec
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

-- | Jn
jnrule :: Parser TzRule
jnrule = do
  _ <- char 'J'
  n <- decimal 3
  unless (n >= 1 && n <= 365) $ fail "date Jn: Julian day out of range [1,365]"
  pure $ TzRule TzRuleJ 0 0 n 0

-- | n
nrule :: Parser TzRule
nrule = do
  n <- decimal 3
  unless (n >= 0 && n <= 365) $ fail "date n: Julian day out of range [0,365]"
  pure $ TzRule TzRuleN 0 0 n 0

-- | M.m.n.d
mrule :: Parser TzRule
mrule = do
  _ <- char 'M'
  m <- decimal 2
  unless (m >= 1 && m <= 12) $ fail "month number out of range [1,12]"
  _ <- char '.'
  n <- decimal 1
  unless (n >= 1 && n <= 5) $ fail "week number out of range [1,5]"
  _ <- char '.'
  d <- decimal 1
  unless (d >= 0 && d <= 6) $ fail "day number out of range [0,6]"
  pure $ TzRule TzRuleM m n d 0

-----------------------------------------------------------------
-- Parser

-- | Parser
--
-- This is a distilled version of attoparsec's Parser as we are not allowed to
-- depend on attoparsec itself.
newtype Parser a = Parser { runParser :: forall r. ByteString
                                      -> Int
                                      -> Failure r
                                      -> Success a r
                                      -> Result r
                          }

type Result    r = Either String r
type Failure a   = ByteString -> Int -> String -> Result a
type Success a r = ByteString -> Int -> a -> Result r

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s pos lose succ ->
    let succ' s' pos' a = succ s' pos' (f a)
    in p s pos lose succ'
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure x = Parser $ \s pos _lose succ -> succ s pos x
  {-# INLINE pure #-}

  ff <*> fa = ff >>= (<$> fa)
  {-# INLINE (<*>) #-}

instance Monad Parser where
#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif
  m >>= k = Parser $ \s pos lose succ ->
    let succ' s' pos' a = runParser (k a) s' pos' lose succ
    in runParser m s pos lose succ'
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail err = Parser $ \s pos lose _succ -> lose s pos err
  {-# INLINE fail #-}

instance Alternative Parser where
  empty = fail "empty"
  {-# INLINE empty #-}

  a <|> b = Parser $ \s pos lose succ ->
    let lose' s' pos' _msg = runParser b s' pos' lose succ
    in runParser a s pos lose' succ
  {-# INLINE (<|>) #-}

successK :: Success a a
successK _s _pos = Right
{-# INLINE successK #-}

failureK :: Failure a
failureK _s _pos = Left
{-# INLINE failureK #-}

parse :: Parser a -> ByteString -> Either String a
parse p s = runParser p s 0 failureK successK
{-# INLINE parse #-}


---------------------------------------------------------------
-- Utilities

option :: a -> Parser a -> Parser a
option a p = p <|> pure a
{-# INLINE option #-}

decimal :: Int -> Parser Int
decimal n = B.foldl' step 0 `fmap` takeWhile1n n isDigit
  where
    step a w = a * 10 + fromIntegral (w - 48)
    isDigit c = c >= '0' && c <= '9'
{-# INLINE decimal #-}

signed :: Int -> Parser Int
signed n = (negate <$> (char '-' *> decimal n))
       <|> (char '+' *> decimal n)
       <|> decimal n
{-# INLINE signed #-}

endOfInput :: Parser ()
endOfInput = Parser $ \s pos lose succ ->
  if pos >= B.length s
    then succ s pos ()
    else lose s pos "endOfInput: expected end of input"
{-# INLINE endOfInput #-}

lookupChar :: Parser Char
lookupChar = do
  ensurebytes 1
  Parser $ \s pos _lose succ ->
    succ s pos $ B8.index s pos
{-# INLINE lookupChar #-}

ensurebytes :: Int -> Parser ()
ensurebytes n = Parser $ \s pos lose succ ->
  if pos + n <= B.length s
    then succ s pos ()
    else lose s pos "unexpected end of input"
{-# INLINE ensurebytes #-}

char :: Char -> Parser Char
char w = do
  ensurebytes 1 
  Parser $ \s pos lose succ ->
    let x = B8.index s pos
    in if x == w
      then succ s (pos + 1) x
      else lose s pos "char: no match"
{-# INLINE char #-}

takeWhile1n :: Int -> (Char -> Bool) -> Parser ByteString
takeWhile1n n0 predicate = Parser $ \s pos lose succ -> go n0 [] s pos lose succ
  where
    go n rs s pos lose succ =
      if n > 0 && pos < B.length s
      then 
        let c = B8.index s pos
        in if predicate c
           then go (n - 1) (c:rs) s (pos + 1) lose succ
           else finish
      else finish
      where
        finish = case rs of
          [] -> lose s pos "takeWhile1n: predicate not matched"
          _ -> succ s pos $ B8.pack $ reverse rs
{-# INLINE takeWhile1n #-}

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
renderPosixTz :: PosixTz -> ByteString
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

    bshow :: (Show a) => a -> ByteString
    bshow = B8.pack . show
{-# INLINE renderPosixTz #-}