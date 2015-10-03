{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Word
  where

import Prelude (Bounded(maxBound, minBound), Integral, fromIntegral)

import Control.Applicative (liftA2)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function ((.))
import Data.Int (Int)
import qualified Data.List as List (length)
import Data.String (String)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Text.Printf (PrintfArg, printf)
import Text.Show (Show(show))

import Test.HUnit (Assertion, (@?=))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.NumberLength.Word
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


tests :: [Test]
tests =
    [ testCase "lengthWord8 minBound" test_lengthWord8_minBound
    , testCase "lengthWord8 maxBound" test_lengthWord8_maxBound
    , testProperty "lengthWord8 = length . show" property_lengthWord8

    , testCase "lengthWord16 minBound" test_lengthWord16_minBound
    , testCase "lengthWord16 maxBound" test_lengthWord16_maxBound
    , testProperty "lengthWord16 = length . show" property_lengthWord16

    , testCase "lengthWord32 minBound" test_lengthWord32_minBound
    , testCase "lengthWord32 maxBound" test_lengthWord32_maxBound
    , testProperty "lengthWord32 = length . show" property_lengthWord32

    , testCase "lengthWord64 minBound" test_lengthWord64_minBound
    , testCase "lengthWord64 maxBound" test_lengthWord64_maxBound
    , testProperty "lengthWord64 = length . show" property_lengthWord64

    , testCase "lengthWord minBound" test_lengthWord_minBound
    , testCase "lengthWord maxBound" test_lengthWord_maxBound
    , testProperty "lengthWord = length . show" property_lengthWord

    , testCase "lengthWord8hex minBound" test_lengthWord8hex_minBound
    , testCase "lengthWord8hex maxBound" test_lengthWord8hex_maxBound
    , testProperty "lengthWord8hex = length . show" property_lengthWord8

    , testCase "lengthWord16hex minBound" test_lengthWord16hex_minBound
    , testCase "lengthWord16hex maxBound" test_lengthWord16hex_maxBound
    , testProperty "lengthWord16hex = length . show" property_lengthWord16

    , testCase "lengthWord32hex minBound" test_lengthWord32hex_minBound
    , testCase "lengthWord32hex maxBound" test_lengthWord32hex_maxBound
    , testProperty "lengthWord32hex = length . show" property_lengthWord32

    , testCase "lengthWord64hex minBound" test_lengthWord64hex_minBound
    , testCase "lengthWord64hex maxBound" test_lengthWord64hex_maxBound
    , testProperty "lengthWord64hex = length . show" property_lengthWord64hex

    , testCase "lengthWordHex minBound" test_lengthWordHex_minBound
    , testCase "lengthWordHex maxBound" test_lengthWordHex_maxBound
    , testProperty "lengthWordHex = length . show" property_lengthWordHex
    ]

numberLength :: Show n => n -> Int
numberLength = List.length . show

numberLengthHex :: (PrintfArg n, Integral n) => n -> Int
numberLengthHex n =
    List.length (printf "%x" (fromIntegral n :: Word64) :: String)

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

-- {{{ Decimal ----------------------------------------------------------------
-- {{{ lengthWord8 ------------------------------------------------------------

test_lengthWord8_minBound, test_lengthWord8_maxBound :: Assertion

test_lengthWord8_minBound =
    lengthWord8 minBound @?= numberLength (minBound :: Word8)

test_lengthWord8_maxBound =
    lengthWord8 maxBound @?= numberLength (maxBound :: Word8)

property_lengthWord8 :: Word8 -> Bool
property_lengthWord8 = lengthWord8 <==> numberLength

-- }}} lengthWord8 ------------------------------------------------------------
-- {{{ lengthWord16 -----------------------------------------------------------

test_lengthWord16_minBound, test_lengthWord16_maxBound :: Assertion

test_lengthWord16_minBound =
    lengthWord16 minBound @?= numberLength (minBound :: Word16)

test_lengthWord16_maxBound =
    lengthWord16 maxBound @?= numberLength (maxBound :: Word16)

property_lengthWord16 :: Word16 -> Bool
property_lengthWord16 = lengthWord16 <==> numberLength

-- }}} lengthWord16 -----------------------------------------------------------
-- {{{ lengthWord32 -----------------------------------------------------------

test_lengthWord32_minBound, test_lengthWord32_maxBound :: Assertion

test_lengthWord32_minBound =
    lengthWord32 minBound @?= numberLength (minBound :: Word32)

test_lengthWord32_maxBound =
    lengthWord32 maxBound @?= numberLength (maxBound :: Word32)

property_lengthWord32 :: Word32 -> Bool
property_lengthWord32 = lengthWord32 <==> numberLength

-- }}} lengthWord32 -----------------------------------------------------------
-- {{{ lengthWord64 -----------------------------------------------------------

test_lengthWord64_minBound, test_lengthWord64_maxBound :: Assertion

test_lengthWord64_minBound =
    lengthWord64 minBound @?= numberLength (minBound :: Word64)

test_lengthWord64_maxBound =
    lengthWord64 maxBound @?= numberLength (maxBound :: Word64)

property_lengthWord64 :: Word64 -> Bool
property_lengthWord64 = lengthWord64 <==> numberLength

-- }}} lengthWord64 -----------------------------------------------------------
-- {{{ lengthWord -------------------------------------------------------------

test_lengthWord_minBound, test_lengthWord_maxBound :: Assertion

test_lengthWord_minBound =
    lengthWord minBound @?= numberLength (minBound :: Word)

test_lengthWord_maxBound =
    lengthWord maxBound @?= numberLength (maxBound :: Word)

property_lengthWord :: Word -> Bool
property_lengthWord = lengthWord <==> numberLength

-- }}} lengthWord -------------------------------------------------------------
-- }}} Decimal ----------------------------------------------------------------

-- {{{ Hexadecimal ------------------------------------------------------------
-- {{{ lengthWord8hex ---------------------------------------------------------

test_lengthWord8hex_minBound, test_lengthWord8hex_maxBound :: Assertion

test_lengthWord8hex_minBound =
    lengthWord8hex minBound @?= numberLengthHex (minBound :: Word8)

test_lengthWord8hex_maxBound =
    lengthWord8hex maxBound @?= numberLengthHex (maxBound :: Word8)

property_lengthWord8hex :: Word8 -> Bool
property_lengthWord8hex = lengthWord8hex <==> numberLengthHex

-- }}} lengthWord8hex ---------------------------------------------------------
-- {{{ lengthWord16hex --------------------------------------------------------

test_lengthWord16hex_minBound, test_lengthWord16hex_maxBound :: Assertion

test_lengthWord16hex_minBound =
    lengthWord16hex minBound @?= numberLengthHex (minBound :: Word16)

test_lengthWord16hex_maxBound =
    lengthWord16hex maxBound @?= numberLengthHex (maxBound :: Word16)

property_lengthWord16hex :: Word16 -> Bool
property_lengthWord16hex = lengthWord16hex <==> numberLengthHex

-- }}} lengthWord16hex --------------------------------------------------------
-- {{{ lengthWord32hex --------------------------------------------------------

test_lengthWord32hex_minBound, test_lengthWord32hex_maxBound :: Assertion

test_lengthWord32hex_minBound =
    lengthWord32hex minBound @?= numberLengthHex (minBound :: Word32)

test_lengthWord32hex_maxBound =
    lengthWord32hex maxBound @?= numberLengthHex (maxBound :: Word32)

property_lengthWord32hex :: Word32 -> Bool
property_lengthWord32hex = lengthWord32hex <==> numberLengthHex

-- }}} lengthWord32hex --------------------------------------------------------
-- {{{ lengthWord64hex --------------------------------------------------------

test_lengthWord64hex_minBound, test_lengthWord64hex_maxBound :: Assertion

test_lengthWord64hex_minBound =
    lengthWord64hex minBound @?= numberLengthHex (minBound :: Word64)

test_lengthWord64hex_maxBound =
    lengthWord64hex maxBound @?= numberLengthHex (maxBound :: Word64)

property_lengthWord64hex :: Word64 -> Bool
property_lengthWord64hex = lengthWord64hex <==> numberLengthHex

-- }}} lengthWord64hex --------------------------------------------------------
-- {{{ lengthWordHex ----------------------------------------------------------

test_lengthWordHex_minBound, test_lengthWordHex_maxBound :: Assertion

test_lengthWordHex_minBound =
    lengthWordHex minBound @?= numberLengthHex (minBound :: Word)

test_lengthWordHex_maxBound =
    lengthWordHex maxBound @?= numberLengthHex (maxBound :: Word)

property_lengthWordHex :: Word -> Bool
property_lengthWordHex = lengthWordHex <==> numberLengthHex

-- }}} lengthWordHex ----------------------------------------------------------
-- }}} Hexadecimal ------------------------------------------------------------
