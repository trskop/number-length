{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Natural (tests)
  where

import Prelude
    ( Bounded(maxBound)
    , Num((*))
    , fromIntegral
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
    , testProperty "lengthNatural = length . show" property_lengthNatural

    , testCase "lengthNaturalHex maxInt64" $ test_lengthNaturalHex maxInt64
    , testCase "lengthNaturalHex maxWord64" $ test_lengthNaturalHex maxWord64
    , testCase "lengthNaturalHex (maxWord64 * 2)"
        $ test_lengthNaturalHex (maxWord64 * 2)
    , testProperty "lengthNaturalHex = length . show" property_lengthNaturalHex
    ]
  where
    maxInt64 = fromIntegral (maxBound :: Int64)
    maxWord64 = fromIntegral (maxBound :: Word64)

numberLengthDec :: Natural -> Int
numberLengthDec = List.length . show

numberLengthHex :: Natural -> Int
numberLengthHex n = List.length (printf "%x" n :: String)

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

test_lengthNatural, test_lengthNaturalHex :: Natural -> Assertion
test_lengthNatural n = lengthNatural n @?= numberLengthDec n
test_lengthNaturalHex n = lengthNaturalHex n @?= numberLengthHex n

property_lengthNatural :: Natural -> Bool
property_lengthNatural = lengthNatural <==> numberLengthDec

property_lengthNaturalHex :: Natural -> Bool
property_lengthNaturalHex = lengthNaturalHex <==> numberLengthHex
