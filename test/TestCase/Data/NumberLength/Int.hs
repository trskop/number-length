{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Int (tests)
  where

import Prelude
    ( Bounded(maxBound, minBound)
    , Integral
    , Num(negate)
    , fromIntegral
    )

import Control.Applicative (liftA2)
import Data.Bool (Bool, otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), id)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.List as List (dropWhile, length)
import Data.Ord (Ord((<)))
import Data.String (String)
import Text.Printf (PrintfArg, printf)
import Text.Show (Show(show))

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))

import Data.NumberLength.Int
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


tests :: [Test]
tests =
    [ testCase "lengthInt8 minBound" test_lengthInt8_minBound
    , testCase "lengthInt8 maxBound" test_lengthInt8_maxBound
    , testProperty "lengthInt8 = length . show" property_lengthInt8

    , testCase "lengthInt16 minBound" test_lengthInt16_minBound
    , testCase "lengthInt16 maxBound" test_lengthInt16_maxBound
    , testProperty "lengthInt16 = length . show" property_lengthInt16

    , testCase "lengthInt32 minBound" test_lengthInt32_minBound
    , testCase "lengthInt32 maxBound" test_lengthInt32_maxBound
    , testProperty "lengthInt32 = length . show" property_lengthInt32

    , testCase "lengthInt64 minBound" test_lengthInt64_minBound
    , testCase "lengthInt64 maxBound" test_lengthInt64_maxBound
    , testProperty "lengthInt64 = length . show" property_lengthInt64

    , testCase "lengthInt minBound" test_lengthInt_minBound
    , testCase "lengthInt maxBound" test_lengthInt_maxBound
    , testProperty "lengthInt = length . show" property_lengthInt

    , testCase "lengthInt8hex minBound" test_lengthInt8hex_minBound
    , testCase "lengthInt8hex maxBound" test_lengthInt8hex_maxBound
    , testProperty "lengthInt8hex = length . show" property_lengthInt8hex

    , testCase "lengthInt16hex minBound" test_lengthInt16hex_minBound
    , testCase "lengthInt16hex maxBound" test_lengthInt16hex_maxBound
    , testProperty "lengthInt16hex = length . show" property_lengthInt16hex

    , testCase "lengthInt32hex minBound" test_lengthInt32hex_minBound
    , testCase "lengthInt32hex maxBound" test_lengthInt32hex_maxBound
    , testProperty "lengthInt32hex = length . show" property_lengthInt32hex

    , testCase "lengthInt64hex minBound" test_lengthInt64hex_minBound
    , testCase "lengthInt64hex maxBound" test_lengthInt64hex_maxBound
    , testProperty "lengthInt64hex = length . show" property_lengthInt64hex

    , testCase "lengthIntHex minBound" test_lengthIntHex_minBound
    , testCase "lengthIntHex maxBound" test_lengthIntHex_maxBound
    , testProperty "lengthIntHex = length . show" property_lengthIntHex
    ]

numberLengthDec :: Show n => n -> Int
numberLengthDec = List.length . List.dropWhile (== '-') . show

numberLengthHex :: (PrintfArg n, Integral n, Ord n) => n -> Int
numberLengthHex n
  | n < 0     = numberLengthHex' . negate . int64 $ fromIntegral n
  | otherwise = numberLengthHex' n
  where
    numberLengthHex' :: PrintfArg n => n -> Int
    numberLengthHex' m = List.length (printf "%x" m :: String)

    int64 :: Int64 -> Int64
    int64 = id

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

-- {{{ Decimal ----------------------------------------------------------------
-- {{{ lengthInt8 -------------------------------------------------------------

test_lengthInt8_minBound, test_lengthInt8_maxBound :: Assertion

test_lengthInt8_minBound =
    lengthInt8 minBound @?= numberLengthDec (minBound :: Int8)

test_lengthInt8_maxBound =
    lengthInt8 maxBound @?= numberLengthDec (maxBound :: Int8)

property_lengthInt8 :: Int8 -> Bool
property_lengthInt8 = lengthInt8 <==> numberLengthDec

-- }}} lengthInt8 -------------------------------------------------------------
-- {{{ lengthInt16 ------------------------------------------------------------

test_lengthInt16_minBound, test_lengthInt16_maxBound :: Assertion

test_lengthInt16_minBound =
    lengthInt16 minBound @?= numberLengthDec (minBound :: Int16)

test_lengthInt16_maxBound =
    lengthInt16 maxBound @?= numberLengthDec (maxBound :: Int16)

property_lengthInt16 :: Int16 -> Bool
property_lengthInt16 = lengthInt16 <==> numberLengthDec

-- }}} lengthInt16 ------------------------------------------------------------
-- {{{ lengthInt32 ------------------------------------------------------------

test_lengthInt32_minBound, test_lengthInt32_maxBound :: Assertion

test_lengthInt32_minBound =
    lengthInt32 minBound @?= numberLengthDec (minBound :: Int32)

test_lengthInt32_maxBound =
    lengthInt32 maxBound @?= numberLengthDec (maxBound :: Int32)

property_lengthInt32 :: Int32 -> Bool
property_lengthInt32 = lengthInt32 <==> numberLengthDec

-- }}} lengthInt32 ------------------------------------------------------------
-- {{{ lengthInt64 ------------------------------------------------------------

test_lengthInt64_minBound, test_lengthInt64_maxBound :: Assertion

test_lengthInt64_minBound =
    lengthInt64 minBound @?= numberLengthDec (minBound :: Int64)

test_lengthInt64_maxBound =
    lengthInt64 maxBound @?= numberLengthDec (maxBound :: Int64)

property_lengthInt64 :: Int64 -> Bool
property_lengthInt64 = lengthInt64 <==> numberLengthDec

-- }}} lengthInt64 ------------------------------------------------------------
-- {{{ lengthInt --------------------------------------------------------------

test_lengthInt_minBound, test_lengthInt_maxBound :: Assertion

test_lengthInt_minBound =
    lengthInt minBound @?= numberLengthDec (minBound :: Int)

test_lengthInt_maxBound =
    lengthInt maxBound @?= numberLengthDec (maxBound :: Int)

property_lengthInt :: Int -> Bool
property_lengthInt = lengthInt <==> numberLengthDec

-- }}} lengthInt --------------------------------------------------------------
-- }}} Decimal ----------------------------------------------------------------

-- {{{ Hexadecimal ------------------------------------------------------------
-- {{{ lengthInt8hex ----------------------------------------------------------

test_lengthInt8hex_minBound, test_lengthInt8hex_maxBound :: Assertion

test_lengthInt8hex_minBound =
    lengthInt8hex minBound @?= numberLengthHex (minBound :: Int8)

test_lengthInt8hex_maxBound =
    lengthInt8hex maxBound @?= numberLengthHex (maxBound :: Int8)

property_lengthInt8hex :: Int8 -> Bool
property_lengthInt8hex = lengthInt8hex <==> numberLengthHex

-- }}} lengthInt8hex ----------------------------------------------------------
-- {{{ lengthInt16hex ---------------------------------------------------------

test_lengthInt16hex_minBound, test_lengthInt16hex_maxBound :: Assertion

test_lengthInt16hex_minBound =
    lengthInt16hex minBound @?= numberLengthHex (minBound :: Int16)

test_lengthInt16hex_maxBound =
    lengthInt16hex maxBound @?= numberLengthHex (maxBound :: Int16)

property_lengthInt16hex :: Int16 -> Bool
property_lengthInt16hex = lengthInt16hex <==> numberLengthHex

-- }}} lengthInt16hex ---------------------------------------------------------
-- {{{ lengthInt32hex ---------------------------------------------------------

test_lengthInt32hex_minBound, test_lengthInt32hex_maxBound :: Assertion

test_lengthInt32hex_minBound =
    lengthInt32hex minBound @?= numberLengthHex (minBound :: Int32)

test_lengthInt32hex_maxBound =
    lengthInt32hex maxBound @?= numberLengthHex (maxBound :: Int32)

property_lengthInt32hex :: Int32 -> Bool
property_lengthInt32hex = lengthInt32hex <==> numberLengthHex

-- }}} lengthInt32hex ---------------------------------------------------------
-- {{{ lengthInt64hex ---------------------------------------------------------

test_lengthInt64hex_minBound, test_lengthInt64hex_maxBound :: Assertion

test_lengthInt64hex_minBound =
    lengthInt64hex minBound @?= 16 -- negate issue

test_lengthInt64hex_maxBound =
    lengthInt64hex maxBound @?= numberLengthHex (maxBound :: Int64)

property_lengthInt64hex :: Int64 -> Bool
property_lengthInt64hex = lengthInt64hex <==> numberLengthHex

-- }}} lengthInt64hex ---------------------------------------------------------
-- {{{ lengthIntHex -----------------------------------------------------------

test_lengthIntHex_minBound, test_lengthIntHex_maxBound :: Assertion

test_lengthIntHex_minBound = lengthIntHex minBound @?=
    -- Handling of "negate" issue:
    if (minBound :: Int) == fromIntegral (minBound :: Int32)
        then 8  -- 32bit
        else 16 -- 64bit

test_lengthIntHex_maxBound =
    lengthIntHex maxBound @?= numberLengthHex (maxBound :: Int)

property_lengthIntHex :: Int -> Bool
property_lengthIntHex = lengthIntHex <==> numberLengthHex

-- }}} lengthIntHex -----------------------------------------------------------
-- }}} Hexadecimal ------------------------------------------------------------
