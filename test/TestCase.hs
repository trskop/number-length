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

import qualified TestCase.Data.NumberLength as NumberLength (tests)
import qualified TestCase.Data.NumberLength.Int as Int (tests)
import qualified TestCase.Data.NumberLength.Integer as Integer (tests)
import qualified TestCase.Data.NumberLength.Word as Word (tests)


tests :: [Test]
tests =
    [ testGroup "Data.NumberLength.Int" Int.tests
    , testGroup "Data.NumberLength.Word" Word.tests
    , testGroup "Data.NumberLength.Integer" Integer.tests
    , testGroup "Data.NumberLength" NumberLength.tests
    ]
