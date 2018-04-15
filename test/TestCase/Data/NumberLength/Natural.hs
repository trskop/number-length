{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2018, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Natural (tests)
  where

import Prelude
    ( Bounded(maxBound)
    , Num((-), (*))
    , fromIntegral
    , (^)
    )

import Control.Applicative (liftA2)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($))
import Data.Int (Int, Int64)
import qualified Data.List as List (length)
import Data.String (String)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Text.Printf (printf)
import Text.Show (Show(show))

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck.Instances ()

import Data.NumberLength.Natural
    ( lengthNatural
    , lengthNaturalHex
    )


tests :: [Test]
tests =
    [ testCase "lengthNatural maxInt64" $ test_lengthNatural maxInt64
    , testCase "lengthNatural maxWord64" $ test_lengthNatural maxWord64
    , testCase "lengthNatural (maxWord64 * 2)"
        $ test_lengthNatural (maxWord64 * 2)
    , testCase "lengthNatural (10 ^ maxWordDigits32 - 1)"
        $ test_lengthNatural (10 ^ maxWordDigits32 - 1)
    , testCase "lengthNatural (10 ^ maxWordDigits64 - 1)"
        $ test_lengthNatural (10 ^ maxWordDigits64 - 1)
    , testProperty "lengthNatural = length . show" property_lengthNatural

    , testCase "lengthNaturalHex maxInt64" $ test_lengthNaturalHex maxInt64
    , testCase "lengthNaturalHex maxWord64" $ test_lengthNaturalHex maxWord64
    , testCase "lengthNaturalHex (maxWord64 * 2)"
        $ test_lengthNaturalHex (maxWord64 * 2)
    , testCase "lengthNaturalHex (10 ^ maxWordDigits32hex - 1)"
        $ test_lengthNaturalHex (16 ^ maxWordDigits32hex - 1)
    , testCase "lengthNaturalHex (10 ^ maxWordDigits64hex - 1)"
        $ test_lengthNaturalHex (16 ^ maxWordDigits64hex - 1)
    , testProperty "lengthNaturalHex = length . show" property_lengthNaturalHex
    ]
  where
    maxInt64 = fromIntegral (maxBound :: Int64)
    maxWord64 = fromIntegral (maxBound :: Word64)
    maxWordDigits32 = 10 :: Int
    maxWordDigits64 = 20 :: Int
    maxWordDigits32hex = 8 :: Int
    maxWordDigits64hex = 16 :: Int

numberLengthDec :: Natural -> Int
numberLengthDec = List.length . show

numberLengthHex :: Natural -> Int
numberLengthHex n = List.length (printf "%x" n :: String)

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

test_lengthNatural, test_lengthNaturalHex :: Natural -> Assertion
test_lengthNatural n = lengthNatural n @?= numberLengthDec n
test_lengthNaturalHex n = lengthNaturalHex n @?= numberLengthHex n

property_lengthNatural, property_lengthNaturalHex :: Natural -> Bool

property_lengthNatural = lengthNatural <==> numberLengthDec
{-# ANN property_lengthNatural "HLint: ignore Use camelCase" #-}

property_lengthNaturalHex = lengthNaturalHex <==> numberLengthHex
{-# ANN property_lengthNaturalHex "HLint: ignore Use camelCase" #-}
