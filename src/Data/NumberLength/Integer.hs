{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Get number of digits of an Integer.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Get number of digits of an 'Integer'.
--
-- /Since 0.2.0.0/
module Data.NumberLength.Integer
    (
    -- * Decimal (base 10)
      lengthInteger

    -- * Hexadecimal (base 16)
    , lengthIntegerHex
    )
  where

import Prelude
    ( Bounded(maxBound)
    , Integer
    , Integral(quot)
    , Num((+), fromInteger, negate)
    , (^)
    , fromIntegral
    )

import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Ord (Ord((<)))

import Data.NumberLength.Int (lengthInt, lengthIntHex)
import Data.NumberLength.Internal (either32or64)


-- | Number of digits in a @number :: 'Integer'@ in base 10.
--
-- /Since 0.2.0.0/
lengthInteger :: Integer -> Int
lengthInteger n
  | n < 0         = go (negate (fromIntegral n))
  | otherwise     = go (fromIntegral n)
  where
    go :: Integer -> Int
    go m
      | m < maxInt = lengthInt (fromInteger m)
      | otherwise  =
        maxIntDigits + lengthInteger (m `quot` (10 ^ maxIntDigits))

    maxIntDigits :: Int
    maxIntDigits = 10 `either32or64` 19
{-# INLINE lengthInteger #-}

-- | Number of digits in a @number :: 'Integer'@ in base 16.
--
-- /Since 0.2.0.0/
lengthIntegerHex :: Integer -> Int
lengthIntegerHex n
  | n < 0     = go (negate n)
  | otherwise = go n
  where
    go :: Integer -> Int
    go m
      | m < maxInt = lengthIntHex (fromInteger m)
      | otherwise  =
        maxIntDigits + lengthIntegerHex (m `quot` (16 ^ maxIntDigits))

    maxIntDigits :: Int
    maxIntDigits = 8 `either32or64` 16
{-# INLINE lengthIntegerHex #-}

-- | Maximum value of type 'Int' cast in to 'Integral'.
--
-- /Do not export this function!/
maxInt :: Integer
maxInt = fromIntegral (maxBound :: Int)
