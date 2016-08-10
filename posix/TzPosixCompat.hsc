{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
#include <bindings.dsl.h>
#include <time.h>

module TzPosixCompat where
#strict_import

#integral_t time_t

#starttype struct tm
#field tm_sec , CInt
#field tm_min , CInt
#field tm_hour , CInt
#field tm_mday , CInt
#field tm_mon , CInt
#field tm_year , CInt
#field tm_wday , CInt
#field tm_yday , CInt
#field tm_isdst , CInt
#stoptype

#ccall localtime_r ,  Ptr <time_t> -> Ptr <tm> -> IO (Ptr <tm>)

#ccall tzset , IO ()
