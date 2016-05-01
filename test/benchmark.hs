{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |
-- Module:       Main
-- Description:  Benchmarks for low-level functions.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- Benchmarks for low-level functions.
module Main (main)
  where

import Prelude (Bounded(minBound, maxBound), Num((*)), fromIntegral)

import Data.Function (($))
import Data.Int (Int64)
import Data.Word (Word64)
import System.IO (IO)

import Criterion.Main (bench, defaultMain, nf)

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
import Data.NumberLength.Integer
    ( lengthInteger
    , lengthIntegerHex
    )
#ifdef HAVE_NATURAL
import Data.NumberLength.Natural
    ( lengthNatural
    , lengthNaturalHex
    )
#endif
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


main :: IO ()
main = defaultMain
    [ bench "lengthInt minBound" $ nf lengthInt minBound
    , bench "lengthInt 0"        $ nf lengthInt 0
    , bench "lengthInt maxBound" $ nf lengthInt maxBound

    , bench "lengthInt8 minBound" $ nf lengthInt8 minBound
    , bench "lengthInt8 0"        $ nf lengthInt8 0
    , bench "lengthInt8 maxBound" $ nf lengthInt8 maxBound

    , bench "lengthInt16 minBound" $ nf lengthInt16 minBound
    , bench "lengthInt16 0"        $ nf lengthInt16 0
    , bench "lengthInt16 maxBound" $ nf lengthInt16 maxBound

    , bench "lengthInt32 minBound" $ nf lengthInt32 minBound
    , bench "lengthInt32 0"        $ nf lengthInt32 0
    , bench "lengthInt32 maxBound" $ nf lengthInt32 maxBound

    , bench "lengthInt64 minBound" $ nf lengthInt64 minBound
    , bench "lengthInt64 0"        $ nf lengthInt64 0
    , bench "lengthInt64 maxBound" $ nf lengthInt64 maxBound

    , bench "lengthIntHex minBound" $ nf lengthIntHex minBound
    , bench "lengthIntHex 0"        $ nf lengthIntHex 0
    , bench "lengthIntHex maxBound" $ nf lengthIntHex maxBound

    , bench "lengthInt8hex minBound" $ nf lengthInt8hex minBound
    , bench "lengthInt8hex 0"        $ nf lengthInt8hex 0
    , bench "lengthInt8hex maxBound" $ nf lengthInt8hex maxBound

    , bench "lengthInt16hex minBound" $ nf lengthInt16hex minBound
    , bench "lengthInt16hex 0"        $ nf lengthInt16hex 0
    , bench "lengthInt16hex maxBound" $ nf lengthInt16hex maxBound

    , bench "lengthInt32hex minBound" $ nf lengthInt32hex minBound
    , bench "lengthInt32hex 0"        $ nf lengthInt32hex 0
    , bench "lengthInt32hex maxBound" $ nf lengthInt32hex maxBound

    , bench "lengthInt64hex minBound" $ nf lengthInt64hex minBound
    , bench "lengthInt64hex 0"        $ nf lengthInt64hex 0
    , bench "lengthInt64hex maxBound" $ nf lengthInt64hex maxBound


    , bench "lengthInteger minInt64"           $ nf lengthInteger minInt64
    , bench "lengthInteger 0"                  $ nf lengthInteger 0
    , bench "lengthInteger maxInt64"           $ nf lengthInteger maxInt64
    , bench "lengthInteger maxWord64"          $ nf lengthInteger maxWord64
    , bench "lengthInteger (maxWord64 * 2)"    $ nf lengthInteger maxWord64x2

    , bench "lengthIntegerHex minInt64"        $ nf lengthIntegerHex minInt64
    , bench "lengthIntegerHex 0"               $ nf lengthIntegerHex 0
    , bench "lengthIntegerHex minInt64"        $ nf lengthIntegerHex maxInt64
    , bench "lengthIntegerHex maxWord64"       $ nf lengthIntegerHex maxWord64
    , bench "lengthIntegerHex (maxWord64 * 2)" $ nf lengthIntegerHex maxWord64x2


#ifdef HAVE_NATURAL
    , bench "lengthNatural 0"                  $ nf lengthNatural 0
    , bench "lengthNatural maxInt64"           $ nf lengthNatural maxInt64
    , bench "lengthNatural maxWord64"          $ nf lengthNatural maxInt64
    , bench "lengthNatural (maxWord64 * 2)"    $ nf lengthNatural maxWord64x2

    , bench "lengthNaturalHex 0"               $ nf lengthNaturalHex 0
    , bench "lengthNaturalHex minInt64"        $ nf lengthNaturalHex maxInt64
    , bench "lengthNaturalHex maxWord64"       $ nf lengthNaturalHex maxWord64
    , bench "lengthNaturalHex (maxWord64 * 2)" $ nf lengthNaturalHex maxWord64x2
#endif


    , bench "lengthWord 0"        $ nf lengthWord 0
    , bench "lengthWord maxBound" $ nf lengthWord maxBound

    , bench "lengthWord8 0"        $ nf lengthWord8 0
    , bench "lengthWord8 maxBound" $ nf lengthWord8 maxBound

    , bench "lengthWord16 0"        $ nf lengthWord16 0
    , bench "lengthWord16 maxBound" $ nf lengthWord16 maxBound

    , bench "lengthWord32 0"        $ nf lengthWord32 0
    , bench "lengthWord32 maxBound" $ nf lengthWord32 maxBound

    , bench "lengthWord64 0"        $ nf lengthWord64 0
    , bench "lengthWord64 maxBound" $ nf lengthWord64 maxBound

    , bench "lengthWordHex 0"        $ nf lengthWordHex 0
    , bench "lengthWordHex maxBound" $ nf lengthWordHex maxBound

    , bench "lengthWord8hex 0"        $ nf lengthWord8hex 0
    , bench "lengthWord8hex maxBound" $ nf lengthWord8hex maxBound

    , bench "lengthWord16hex 0"        $ nf lengthWord16hex 0
    , bench "lengthWord16hex maxBound" $ nf lengthWord16hex maxBound

    , bench "lengthWord32hex 0"        $ nf lengthWord32hex 0
    , bench "lengthWord32hex maxBound" $ nf lengthWord32hex maxBound

    , bench "lengthWord64hex 0"        $ nf lengthWord64hex 0
    , bench "lengthWord64hex maxBound" $ nf lengthWord64hex maxBound
    ]
  where
    maxInt64 = fromIntegral (maxBound :: Int64)
    minInt64 = fromIntegral (minBound :: Int64)
    maxWord64 = fromIntegral (maxBound :: Word64)
    maxWord64x2 = maxWord64 * 2
