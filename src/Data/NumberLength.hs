{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module Data.NumberLength
  where

import Prelude (Bounded(maxBound), fromIntegral)

import Data.Bool (Bool, otherwise)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ord (Ord((>)))
import Data.Word (Word, Word16, Word32, Word64, Word8)

import Data.Proxy (Proxy)

import Data.Int.Length
    ( lengthInt
    , lengthInt16
    , lengthInt16hex
    , lengthInt32
    , lengthInt32hex
    , lengthInt64
    , lengthInt64hex
    , lengthInt8
    , lengthInt8hex
    , lengthIntHex
    )
import Data.Word.Length
    ( lengthWord
    , lengthWord16
    , lengthWord16hex
    , lengthWord32
    , lengthWord32hex
    , lengthWord64
    , lengthWord64hex
    , lengthWord8
    , lengthWord8hex
    , lengthWordHex
    )


class NumberLength a where
    numberLength :: a -> Int
    numberLengthHex :: a -> Int
    maxNumberLength :: Proxy a -> Int
    maxNumberLengthHex :: Proxy a -> Int

-- {{{ Int* -------------------------------------------------------------------

instance NumberLength Int where
    numberLength = lengthInt
    numberLengthHex = lengthIntHex

    maxNumberLength _
      | isInt64bit = 19
      | otherwise  = 10 -- 32bit

    maxNumberLengthHex _
      | isInt64bit = 16
      | otherwise  = 8  -- 32bit

isInt64bit :: Bool
isInt64bit = (maxBound :: Int) > fromIntegral (maxBound :: Int32)
{-# INLINE isInt64bit #-}

instance NumberLength Int64 where
    numberLength = lengthInt64
    numberLengthHex = lengthInt64hex
    maxNumberLength _ = 19
    maxNumberLengthHex _ = 16

instance NumberLength Int32 where
    numberLength = lengthInt32
    numberLengthHex = lengthInt32hex
    maxNumberLength _ = 10
    maxNumberLengthHex _ = 8

instance NumberLength Int16 where
    numberLength = lengthInt16
    numberLengthHex = lengthInt16hex
    maxNumberLength _ = 5
    maxNumberLengthHex _ = 4

instance NumberLength Int8 where
    numberLength = lengthInt8
    numberLengthHex = lengthInt8hex
    maxNumberLength _ = 3
    maxNumberLengthHex _ = 2

-- }}} Int* -------------------------------------------------------------------
-- {{{ Word* ------------------------------------------------------------------

instance NumberLength Word where
    numberLength = lengthWord
    numberLengthHex = lengthWordHex

    maxNumberLength _
      | isWord64bit = 20
      | otherwise   = 10    -- 32bit

    maxNumberLengthHex _
      | isWord64bit = 16
      | otherwise   = 8     -- 32bit

isWord64bit :: Bool
isWord64bit = (maxBound :: Word) > fromIntegral (maxBound :: Word32)
{-# INLINE isWord64bit #-}

instance NumberLength Word64 where
    numberLength = lengthWord64
    numberLengthHex = lengthWord64hex
    maxNumberLength _ = 20
    maxNumberLengthHex _ = 16

instance NumberLength Word32 where
    numberLength = lengthWord32
    numberLengthHex = lengthWord32hex
    maxNumberLength _ = 10
    maxNumberLengthHex _ = 8

instance NumberLength Word16 where
    numberLength = lengthWord16
    numberLengthHex = lengthWord16hex
    maxNumberLength _ = 5
    maxNumberLengthHex _ = 4

instance NumberLength Word8 where
    numberLength = lengthWord8
    numberLengthHex = lengthWord8hex
    maxNumberLength _ = 3
    maxNumberLengthHex _ = 2

-- }}} Word* ------------------------------------------------------------------
