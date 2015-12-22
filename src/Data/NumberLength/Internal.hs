{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Internally used utilities.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    unstable
-- Portability:  CPP, NoImplicitPrelude
module Data.NumberLength.Internal
  where

import Prelude (error)

#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(finiteBitSize))
#else
import Data.Bits (Bits(bitSize))
#endif

import Data.Function (($))
import Data.Monoid ((<>))
import Data.Word (Word)
import Text.Show (Show(show))


-- | Returns one of its arguments, depending on bit size of 'Word' type on
-- current hardware.
either32or64
    :: a
    -- ^ Used in case when 'Word' is 32bit long.
    -> a
    -- ^ Used in case when 'Word' is 64bit long.
    -> a
either32or64 on32bit on64bit = case wordSize (0 :: Word) of
    32 -> on32bit
    64 -> on64bit
    bs -> error $ "Data.NumberLength.either32or64: " <> show bs
        <> ": System uses Word size not supported by this library."
  where
    wordSize =
#if MIN_VERSION_base(4,7,0)
        finiteBitSize
#else
        bitSize
#endif
{-# INLINE either32or64 #-}
