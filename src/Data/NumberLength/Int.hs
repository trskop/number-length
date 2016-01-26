{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Get number of digits of a number from a Int-family of numbers
--               in decimal or hexadecimal representation.
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Get number of digits of a number from a 'Int'-family of numbers in decimal
-- or hexadecimal representation.
module Data.NumberLength.Int
    (
    -- * Decimal
      lengthInt
    , lengthInt8
    , lengthInt16
    , lengthInt32
    , lengthInt64

    -- * Hexadecimal
    , lengthIntHex
    , lengthInt8hex
    , lengthInt16hex
    , lengthInt32hex
    , lengthInt64hex
    )
  where

import Prelude
    ( Bounded(minBound)
    , Integral(quot)
    , Num((+), negate)
    , fromIntegral
    )

import Data.Bool ((&&), otherwise)
import Data.Eq (Eq((==)))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ord (Ord((<), (>), (>=)))

import Data.NumberLength.Internal (either32or64)


-- {{{ Decimal ----------------------------------------------------------------

lengthInt8 :: Int8 -> Int
lengthInt8 n
  | n < 0         = go (negate (fromIntegral n))
  | otherwise     = go (fromIntegral n)
  where
    go :: Int -> Int
    go m
      -- Maximum is 127 for positive and 128 for negative integer.
      | m < 10     = 1
      | m < 100    = 2
      | otherwise  = 3
{-# INLINE lengthInt8 #-}

lengthInt16 :: Int16 -> Int
lengthInt16 n
  | n < 0     = go (negate (fromIntegral n))
  | otherwise = go (fromIntegral n)
  where
    -- Maximum is 32767 for positive and 32768 for negative integer.
    go :: Int -> Int
    go m
      | m < 10     = 1
      | m < 100    = 2
      | m < 1000   = 3
      | m < 10000  = 4
      | otherwise  = 5
{-# INLINE lengthInt16 #-}

lengthInt32 :: Int32 -> Int
lengthInt32 n
  | n == minBound = 10  -- "negate minBound" is out of range of Int32.
  | n < 0         = go (negate (fromIntegral n))
  | otherwise     = go (fromIntegral n)
  where
    -- Maximum is 2147483647 for positive and 2147483648 for negative integer.
    go :: Int -> Int
    go m
      | m < 10         = 1
      | m < 100        = 2
      | m < 1000       = 3
      | m < 10000      = 4
      | m >= 100000000 = 8 + go (m `quot` 100000000)
      | otherwise      = 4 + go (m `quot` 10000)
        -- m >= 10000
{-# INLINE lengthInt32 #-}

lengthInt64 :: Int64 -> Int
lengthInt64 n
  | n == minBound = 19 -- "negate minBound" is out of range of Int64
  | n < 0         = go (negate n)
  | otherwise     = go n
  where
    -- Maximum is 9223372036854775807 for positive and 9223372036854775808
    -- for negative integer.
    go m
      | m < 10                 = 1
      | m < 100                = 2
      | m < 1000               = 3
      | m < 10000              = 4
      | m >= 10000000000000000 = 16 + go (m `quot` 10000000000000000)
      | m >= 100000000         = 8  + go (m `quot` 100000000)
      | otherwise              = 4  + go (m `quot` 10000)
        -- m >= 10000
{-# INLINE lengthInt64 #-}

lengthInt :: Int -> Int
lengthInt n = l32 `either32or64` l64
  where
    -- Same code as lengthInt64:
    l64
      | n == minBound = 19 -- "negate minBound" is out of range of Int64
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 9223372036854775807 for positive and 9223372036854775808
        -- for negative integer.
        go m
          | m < 10                 = 1
          | m < 100                = 2
          | m < 1000               = 3
          | m < 10000              = 4
          | m >= 10000000000000000 = 16 + go (m `quot` 10000000000000000)
          | m >= 100000000         = 8  + go (m `quot` 100000000)
          | otherwise              = 4  + go (m `quot` 10000)
            -- m >= 10000

    -- Same code as lengthInt32:
    l32
      | n == minBound = 10  -- "negate minBound" is out of range of Int32.
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 2147483647 for positive and 2147483648 for negative integer.
        go m
          | m < 10         = 1
          | m < 100        = 2
          | m < 1000       = 3
          | m < 10000      = 4
          | m >= 100000000 = 8 + go (m `quot` 100000000)
          | otherwise      = 4 + go (m `quot` 10000)
            -- m >= 10000
{-# INLINE lengthInt #-}

-- }}} Decimal ----------------------------------------------------------------

-- {{{ Hexadecimal ------------------------------------------------------------

lengthInt8hex :: Int8 -> Int
lengthInt8hex n
  | n < 16 && n > -16 = 1
  | otherwise         = 2
  -- Maximum is 127 = 0x7f for positive and 128 = 0x80 for negative integer.
{-# INLINE lengthInt8hex #-}

lengthInt16hex :: Int16 -> Int
lengthInt16hex n
  | n < 0     = go (negate (fromIntegral n))
  | otherwise = go (fromIntegral n)
  where
    -- Maximum is 32767 = 0x7fff for positive and 32768 = 0x8000 for negative
    -- integer.
    go :: Int -> Int
    go m
      | m < 0x10   = 1
      | m < 0x100  = 2
      | m < 0x1000 = 3
      | otherwise  = 4
{-# INLINE lengthInt16hex #-}

lengthInt32hex :: Int32 -> Int
lengthInt32hex n
  | n == minBound = 8   -- "negate minBound" is out of range of Int32.
  | n < 0         = go (negate (fromIntegral n))
  | otherwise     = go (fromIntegral n)
  where
    -- Maximum is 2147483647 = 0x7fffffff for positive and
    -- 2147483648 = 0x80000000 for negative integer.
    go :: Int -> Int
    go m
      | m < 0x10    = 1
      | m < 0x100   = 2
      | m < 0x1000  = 3
      | m < 0x10000 = 4
      | otherwise   = 4 + go (m `quot` 0x10000)
        -- m >= 0x10000
{-# INLINE lengthInt32hex #-}

lengthInt64hex :: Int64 -> Int
lengthInt64hex n
  | n == minBound = 16 -- "negate minBound" is out of range of Int64
  | n < 0         = go (negate n)
  | otherwise     = go n
  where
    -- Maximum is 9223372036854775807 = 0x7fffffffffffffff for positive and
    -- 9223372036854775808 = 0x8000000000000000 for negative integer.
    go m
      | m <  0x10        = 1
      | m <  0x100       = 2
      | m <  0x1000      = 3
      | m <  0x10000     = 4
      | m >= 0x100000000 = 8  + go (m `quot` 0x100000000)
      | otherwise        = 4  + go (m `quot` 0x10000)
        -- m >= 0x10000
{-# INLINE lengthInt64hex #-}

lengthIntHex :: Int -> Int
lengthIntHex n = l32hex `either32or64` l64hex
  where
    -- Same code as lengthInt64hex:
    l64hex
      | n == minBound = 16 -- "negate minBound" is out of range of Int64
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 9223372036854775807 = 0x7fffffffffffffff for positive and
        -- 9223372036854775808 = 0x8000000000000000 for negative integer.
        go m
          | m <  0x10        = 1
          | m <  0x100       = 2
          | m <  0x1000      = 3
          | m <  0x10000     = 4
          | m >= 0x100000000 = 8  + go (m `quot` 0x100000000)
          | otherwise        = 4  + go (m `quot` 0x10000)
            -- m >= 0x10000

    -- Same code as lengthInt32hex:
    l32hex
      | n == minBound = 8   -- "negate minBound" is out of range of Int32.
      | n < 0         = go (negate n)
      | otherwise     = go n
      where
        -- Maximum is 2147483647 = 0x7fffffff for positive and
        -- 2147483648 = 0x80000000 for negative integer.
        go :: Int -> Int
        go m
          | m < 0x10    = 1
          | m < 0x100   = 2
          | m < 0x1000  = 3
          | m < 0x10000 = 4
          | otherwise   = 4 + go (m `quot` 0x10000)
            -- m >= 0x10000
{-# INLINE lengthIntHex #-}

-- }}} Hexadecimal ------------------------------------------------------------
