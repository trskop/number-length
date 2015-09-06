{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.Word.Length
  where

import Prelude (Bounded(maxBound, minBound))

import Control.Applicative (liftA2)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function ((.))
import Data.Int (Int)
import qualified Data.List as List (length)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Text.Show (Show(show))

import Test.HUnit (Assertion, (@?=))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word.Length
    ( lengthWord
    , lengthWord16
    , lengthWord32
    , lengthWord64
    , lengthWord8
    )


tests :: [Test]
tests =
    [ testCase     "lengthWord8 minBound"         test_lengthWord8_minBound
    , testCase     "lengthWord8 maxBound"         test_lengthWord8_maxBound
    , testProperty "lengthWord8 = length . show"  property_lengthWord8

    , testCase     "lengthWord16 minBound"        test_lengthWord16_minBound
    , testCase     "lengthWord16 maxBound"        test_lengthWord16_maxBound
    , testProperty "lengthWord16 = length . show" property_lengthWord16

    , testCase     "lengthWord32 minBound"        test_lengthWord32_minBound
    , testCase     "lengthWord32 maxBound"        test_lengthWord32_maxBound
    , testProperty "lengthWord32 = length . show" property_lengthWord32

    , testCase     "lengthWord64 minBound"        test_lengthWord64_minBound
    , testCase     "lengthWord64 maxBound"        test_lengthWord64_maxBound
    , testProperty "lengthWord64 = length . show" property_lengthWord64

    , testCase     "lengthWord minBound"          test_lengthWord_minBound
    , testCase     "lengthWord maxBound"          test_lengthWord_maxBound
    , testProperty "lengthWord = length . show"   property_lengthWord
    ]

numberLength :: Show n => n -> Int
numberLength = List.length . show

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

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
