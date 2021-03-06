{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Get number of digits of a number from a Word-family of numbers
--               in decimal or hexadecimal representation.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Get number of digits of a number from a 'Word'-family of numbers in decimal
-- or hexadecimal representation.
module Data.NumberLength.Word
    (
    -- * Decimal (base 10)
      lengthWord
    , lengthWord8
    , lengthWord16
    , lengthWord32
    , lengthWord64

    -- * Hexadecimal (base 16)
    , lengthWordHex
    , lengthWord8hex
    , lengthWord16hex
    , lengthWord32hex
    , lengthWord64hex
    )
  where

import Prelude
    ( Integral(quot)
    , Num((+))
    , fromIntegral
    )

import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Ord (Ord((<), (>=)))
import Data.Word (Word, Word16, Word32, Word64, Word8)

import Data.NumberLength.Internal (either32or64)


-- {{{ Decimal ----------------------------------------------------------------

-- | Number of digits in a @number :: 'Word8'@ in base 10.
lengthWord8 :: Word8 -> Int
lengthWord8 n   -- Maximum is 255.
  | n < 10     = 1
  | n < 100    = 2
  | otherwise  = 3
{-# INLINE lengthWord8 #-}

-- | Number of digits in a @number :: 'Word16'@ in base 10.
lengthWord16 :: Word16 -> Int
lengthWord16 n   -- Maximum is 65535.
  | n < 10     = 1
  | n < 100    = 2
  | n < 1000   = 3
  | n < 10000  = 4
  | otherwise  = 5
{-# INLINE lengthWord16 #-}

-- | Number of digits in a @number :: 'Word32'@ in base 10.
lengthWord32 :: Word32 -> Int
lengthWord32 n   -- Maximum is 4294967295.
  | n < 10         = 1
  | n < 100        = 2
  | n < 1000       = 3
  | n < 10000      = 4
  | n >= 100000000 = 8 + lengthWord32 (n `quot` 100000000)
  | otherwise      = 4 + lengthWord32 (n `quot` 10000)
    -- n >= 10000
{-# INLINE lengthWord32 #-}

-- | Number of digits in a @number :: 'Word64'@ in base 10.
lengthWord64 :: Word64 -> Int
lengthWord64 n   -- Maximum is 18446744073709551615.
  | n < 10                 = 1
  | n < 100                = 2
  | n < 1000               = 3
  | n < 10000              = 4
  | n >= 10000000000000000 = 16 + lengthWord64 (n `quot` 10000000000000000)
  | n >= 100000000         = 8  + lengthWord64 (n `quot` 100000000)
  | otherwise              = 4  + lengthWord64 (n `quot` 10000)
    -- n >= 10000
{-# INLINE lengthWord64 #-}

-- | Number of digits in a @number :: 'Word'@ in base 10.
lengthWord :: Word -> Int
lengthWord n =
    lengthWord32 (fromIntegral n) `either32or64` lengthWord64 (fromIntegral n)
{-# INLINE lengthWord #-}

-- }}} Decimal ----------------------------------------------------------------

-- {{{ Hexadecimal ------------------------------------------------------------

-- | Number of digits in a @number :: 'Word8'@ in base 16.
lengthWord8hex :: Word8 -> Int
lengthWord8hex n    -- Maximum is 255 = 0xff.
  | n < 16    = 1
  | otherwise = 2
{-# INLINE lengthWord8hex #-}

-- | Number of digits in a @number :: 'Word16'@ in base 16.
lengthWord16hex :: Word16 -> Int
lengthWord16hex n   -- Maximum is 65535 = 0xffff.
  | n < 0x10   = 1
  | n < 0x100  = 2
  | n < 0x1000 = 3
  | otherwise  = 4
{-# INLINE lengthWord16hex #-}

-- | Number of digits in a @number :: 'Word32'@ in base 16.
lengthWord32hex :: Word32 -> Int
lengthWord32hex n   -- Maximum is 4294967295 = 0xffffffff.
  | n < 0x10    = 1
  | n < 0x100   = 2
  | n < 0x1000  = 3
  | n < 0x10000 = 4
  | otherwise   = 4 + lengthWord32hex (n `quot` 0x10000)
    -- n >= 0x10000
{-# INLINE lengthWord32hex #-}

-- | Number of digits in a @number :: 'Word64'@ in base 16.
lengthWord64hex :: Word64 -> Int
lengthWord64hex n   -- Maximum is 18446744073709551615 = 0xffffffffffffffff.
  | n <  0x10        = 1
  | n <  0x100       = 2
  | n <  0x1000      = 3
  | n <  0x10000     = 4
  | n >= 0x100000000 = 8 + lengthWord64hex (n `quot` 0x100000000)
  | otherwise        = 4 + lengthWord64hex (n `quot` 0x10000)
    -- n >= 0x10000
{-# INLINE lengthWord64hex #-}

-- | Number of digits in a @number :: 'Word'@ in base 16.
lengthWordHex :: Word -> Int
lengthWordHex n = lengthWord32hex (fromIntegral n)
    `either32or64` lengthWord64hex (fromIntegral n)
{-# INLINE lengthWordHex #-}

-- }}} Hexadecimal ------------------------------------------------------------
