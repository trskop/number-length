{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength.Integer (tests)
  where

import Prelude
    ( Integer
    , Num(negate)
    )

import Control.Applicative (liftA2)
import Data.Bool (Bool, otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($))
import Data.Int (Int)
import qualified Data.List as List (dropWhile, length)
import Data.Ord (Ord((<)))
import Data.String (String)
import Text.Printf (PrintfArg, printf)
import Text.Show (Show(show))

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.NumberLength.Integer
    ( lengthInteger
    , lengthIntegerHex
    )


tests :: [Test]
tests =
    [ testProperty "lengthInteger = length . show" property_lengthInteger
    , testProperty "lengthIntegerHex = length . show" property_lengthIntegerHex
    ]

numberLengthDec :: Show n => n -> Int
numberLengthDec = List.length . List.dropWhile (== '-') . show

numberLengthHex :: Integer -> Int
numberLengthHex n
  | n < 0     = numberLengthHex' $ negate n
  | otherwise = numberLengthHex' n
  where
    numberLengthHex' :: PrintfArg n => n -> Int
    numberLengthHex' m = List.length (printf "%x" m :: String)

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)

property_lengthInteger :: Integer -> Bool
property_lengthInteger = lengthInteger <==> numberLengthDec

property_lengthIntegerHex :: Integer -> Bool
property_lengthIntegerHex = lengthIntegerHex <==> numberLengthHex
