{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2018, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Integer (tests)
  where

import Prelude
    ( Bounded(maxBound, minBound)
    , Integer
    , Num((-), (*), negate)
    , fromIntegral
    , (^)
    )

import Control.Applicative (liftA2)
import Data.Bool (Bool, otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($))
import Data.Int (Int, Int64)
import qualified Data.List as List (dropWhile, length)
import Data.Ord (Ord((<)))
import Data.String (String)
import Data.Word (Word64)
import Text.Printf (printf)
import Text.Show (Show(show))

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))

import Data.NumberLength.Integer
    ( lengthInteger
    , lengthIntegerHex
    )


tests :: [Test]
tests =
    [ testCase "lengthInteger minInt64" $ test_lengthInteger minInt64
    , testCase "lengthInteger maxInt64" $ test_lengthInteger maxInt64
    , testCase "lengthInteger minWord64" $ test_lengthInteger minWord64
    , testCase "lengthInteger maxWord64" $ test_lengthInteger maxWord64
    , testCase "lengthInteger (maxWord64 * 2)"
        $ test_lengthInteger (maxWord64 * 2)
    , testCase "lengthInteger (10 ^ maxWordDigits32 - 1)"
        $ test_lengthInteger (10 ^ maxWordDigits32 - 1)
    , testCase "lengthInteger (10 ^ maxWordDigits64 - 1)"
        $ test_lengthInteger (10 ^ maxWordDigits64 - 1)
    , testProperty "lengthInteger = length . show" property_lengthInteger

    , testCase "lengthIntegerHex minInt64" $ test_lengthIntegerHex minInt64
    , testCase "lengthIntegerHex maxInt64" $ test_lengthIntegerHex maxInt64
    , testCase "lengthIntegerHex minWord64" $ test_lengthIntegerHex minWord64
    , testCase "lengthIntegerHex maxWord64" $ test_lengthIntegerHex maxWord64
    , testCase "lengthIntegerHex (maxWord64 * 2)"
        $ test_lengthIntegerHex (maxWord64 * 2)
    , testCase "lengthIntegerHex (10 ^ maxWordDigits32hex - 1)"
        $ test_lengthIntegerHex (16 ^ maxWordDigits32hex - 1)
    , testCase "lengthIntegerHex (10 ^ maxWordDigits64hex - 1)"
        $ test_lengthIntegerHex (16 ^ maxWordDigits64hex - 1)
    , testProperty "lengthIntegerHex = length . show" property_lengthIntegerHex
    ]
  where
    minInt64 = fromIntegral (minBound :: Int64)
    maxInt64 = fromIntegral (maxBound :: Int64)
    minWord64 = fromIntegral (minBound :: Word64)
    maxWord64 = fromIntegral (maxBound :: Word64)
    maxWordDigits32 = 10 :: Int
    maxWordDigits64 = 20 :: Int
    maxWordDigits32hex = 8 :: Int
    maxWordDigits64hex = 16 :: Int

numberLengthDec :: Integer -> Int
numberLengthDec = List.length . List.dropWhile (== '-') . show

numberLengthHex :: Integer -> Int
numberLengthHex n
  | n < 0     = numberLengthHex' $ negate n
  | otherwise = numberLengthHex' n
  where
    numberLengthHex' :: Integer -> Int
    numberLengthHex' m = List.length (printf "%x" m :: String)

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

test_lengthInteger, test_lengthIntegerHex :: Integer -> Assertion
test_lengthInteger n = lengthInteger n @?= numberLengthDec n
test_lengthIntegerHex n = lengthIntegerHex n @?= numberLengthHex n

property_lengthInteger, property_lengthIntegerHex :: Integer -> Bool

property_lengthInteger = lengthInteger <==> numberLengthDec
{-# ANN property_lengthInteger "HLint: ignore Use camelCase" #-}

property_lengthIntegerHex = lengthIntegerHex <==> numberLengthHex
{-# ANN property_lengthIntegerHex "HLint: ignore Use camelCase" #-}
