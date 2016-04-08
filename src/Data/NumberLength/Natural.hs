{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Get number of digits of a Natural.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Get number of digits of a 'Natural'.
--
-- /Since 0.2.0.0/
module Data.NumberLength.Natural
    (
    -- * Decimal (base 10)
      lengthNatural

    -- * Hexadecimal (base 16)
    , lengthNaturalHex
    )
  where

import Prelude
    ( Bounded(maxBound)
    , Integral(quot)
    , Num((+))
    , (^)
    , fromIntegral
    )

import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Ord (Ord((<)))
import Data.Word (Word)
import Numeric.Natural (Natural)

import Data.NumberLength.Internal (either32or64)
import Data.NumberLength.Word (lengthWord, lengthWordHex)


-- | Number of digits in a @number :: 'Natural'@ in base 10.
--
-- /Since 0.2.0.0/
lengthNatural :: Natural -> Int
lengthNatural n
  | n < maxWord = lengthWord (fromIntegral n)
  | otherwise   = maxWordDigits + lengthNatural (n `quot` (10 ^ maxWordDigits))
  where
    maxWordDigits :: Int
    maxWordDigits = 10 `either32or64` 20
{-# INLINE lengthNatural #-}

-- | Number of digits in a @number :: 'Natural'@ in base 16.
--
-- /Since 0.2.0.0/
lengthNaturalHex :: Natural -> Int
lengthNaturalHex n
  | n < maxWord = lengthWordHex (fromIntegral n)
  | otherwise   =
    maxWordDigits + lengthNaturalHex (n `quot` (16 ^ maxWordDigits))
  where
    maxWordDigits :: Int
    maxWordDigits = 8 `either32or64` 16
{-# INLINE lengthNaturalHex #-}

-- | Maximum value of type 'Word' cast in to 'Natural'.
--
-- /Do not export this function!/
maxWord :: Natural
maxWord = fromIntegral (maxBound :: Word)
