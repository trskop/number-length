{-# LANGUAGE NoImplicitPrelude #-}
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

import Prelude (Bounded(minBound, maxBound))

import Data.Function (($))
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
