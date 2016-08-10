{-# LANGUAGE ViewPatterns #-}

module Main (main) where

-- Not exactly a bindings-posix, but the very same code
-- Helps with portability, i.e. we can build/run tests & benchmarks
-- on Windows and OSX
import TzPosixCompat
import Criterion.Main
import Foreign.Safe
import Foreign.C
import System.Posix.Env

setupTZ :: String -> IO ()
setupTZ zoneName = do
  setEnv "TZ" zoneName True
  c'tzset

data C_tm = C_tm {
  _tm_sec    :: {-# UNPACK #-} !CInt,
  _tm_min    :: {-# UNPACK #-} !CInt,
  _tm_hour   :: {-# UNPACK #-} !CInt,
  _tm_mday   :: {-# UNPACK #-} !CInt,
  _tm_mon    :: {-# UNPACK #-} !CInt,
  _tm_year   :: {-# UNPACK #-} !CInt,
  _tm_wday   :: {-# UNPACK #-} !CInt,
  _tm_yday   :: {-# UNPACK #-} !CInt,
  _tm_isdst  :: {-# UNPACK #-} !CInt
  -- _tm_gmtoff :: {-# UNPACK #-} !CInt
  -- _tm_zone :: {-# UNPACK #-} !Ptr CString (??)
  } deriving (Show)

instance Storable C_tm where
  -- Overestimate; it's 10 or 11 based on architecture.
  sizeOf _ = 16 * sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CLong)
  peek (castPtr -> p) = do
    s <- peekElemOff p 0
    mi <- peekElemOff p 1
    h <- peekElemOff p 2
    d <- peekElemOff p 3
    m <- peekElemOff p 4
    y <- peekElemOff p 5
    wd <- peekElemOff p 6
    yd <- peekElemOff p 7
    idst <- peekElemOff p 8
    return $ C_tm s mi h d m y wd yd idst
  poke (castPtr -> p) (C_tm s mi h d m y wd yd idst) = do
    pokeElemOff p 0 s
    pokeElemOff p 1 mi
    pokeElemOff p 2 h
    pokeElemOff p 3 d
    pokeElemOff p 4 m
    pokeElemOff p 5 y
    pokeElemOff p 6 wd
    pokeElemOff p 7 yd
    pokeElemOff p 8 idst

foreign import ccall unsafe "time.h localtime_r" c_localtime_r
  :: Ptr C'time_t -> Ptr C_tm -> IO (Ptr C_tm)

localtime :: Int -> IO C_tm
localtime t = with (fromIntegral t) $ \ptime ->
  alloca $ \ptm -> do
    res <- c_localtime_r ptime ptm
    if res /= nullPtr
      then peek res
      else fail "c_localtime_r failed"

-- This just wraps the binding from Bindings.Posix.Time.
--
-- Because it's foreign import ccall _safe_, it's more than two times
-- slower!
localtime' :: Int -> IO C'tm
localtime' t = with (fromIntegral t) $ \ptime ->
  alloca $ \ptm -> do
    res <- c'localtime_r ptime ptm
    if res /= nullPtr
      then peek res
      else fail "c'localtime_r failed"

benchmarks :: [Benchmark]
benchmarks = [
  bgroup "c'localtime_r" [
     bench "past" $ whnfIO $ localtime' (-2100000000),
     bench "epoch" $ whnfIO $ localtime' 0,
     bench "now" $ whnfIO $ localtime' 1395572400,
     bench "future" $ whnfIO $ localtime' 2100000000
     ],
  bgroup "our_localtime_r" [
     bench "past" $ whnfIO $ localtime (-2100000000),
     bench "epoch" $ whnfIO $ localtime 0,
     bench "now" $ whnfIO $ localtime 1395572400,
     bench "future" $ whnfIO $ localtime 2100000000
     ]
  ]

main :: IO ()
main = do
  setupTZ "Europe/Budapest"
  defaultMain benchmarks
