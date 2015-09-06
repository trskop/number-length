{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module Data.Word.Length
  where

import Prelude
    ( Bounded(maxBound)
    , Integral(quot)
    , Num((+))
    , fromIntegral
    )

import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Ord (Ord((<), (>=)))
import Data.Word (Word, Word16, Word32, Word64, Word8)


lengthWord8 :: Word8 -> Int
lengthWord8 n   -- Maximum is 255.
  | n < 10     = 1
  | n < 100    = 2
  | otherwise  = 3
{-# INLINE lengthWord8 #-}

lengthWord16 :: Word16 -> Int
lengthWord16 n   -- Maximum is 65535.
  | n < 10     = 1
  | n < 100    = 2
  | n < 1000   = 3
  | n < 10000  = 4
  | otherwise  = 5
{-# INLINE lengthWord16 #-}

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

lengthWord :: Word -> Int
lengthWord n
  | is64bit   = lengthWord64 (fromIntegral n)
  | otherwise = lengthWord32 (fromIntegral n)
  where
    is64bit = maxWord32 < maxWord

    maxWord32 = fromIntegral (maxBound :: Word32) :: Word64
    maxWord   = fromIntegral (maxBound :: Word)   :: Word64
{-# INLINE lengthWord #-}
