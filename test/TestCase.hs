{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.Int.Length as Int.Length (tests)
import qualified TestCase.Data.NumberLength as NumberLength (tests)
import qualified TestCase.Data.Word.Length as Word.Length (tests)


tests :: [Test]
tests =
    [ testGroup "Data.Int.Length" Int.Length.tests
    , testGroup "Data.Word.Length" Word.Length.tests
    , testGroup "Data.NumberLength" NumberLength.tests
    ]
