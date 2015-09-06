{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
module TestCase.Data.NumberLength
  where

import Prelude (Bounded(maxBound))

import Control.Applicative (liftA2)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function (($))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word, Word16, Word32, Word64, Word8)

import Test.HUnit ((@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.NumberLength
    ( NumberLength
        ( numberLength
        , numberLengthHex
        , maxNumberLength
        , maxNumberLengthHex
        )
    )
import Data.Int.Length
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
import Data.Word.Length
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


tests :: [Test]
tests =
    [ testGroup "numberLengthHex"
        [ testProperty "Int"   $ numberLength <==> lengthInt
        , testProperty "Int8"  $ numberLength <==> lengthInt8
        , testProperty "Int16" $ numberLength <==> lengthInt16
        , testProperty "Int32" $ numberLength <==> lengthInt32
        , testProperty "Int32" $ numberLength <==> lengthInt64
        ]

    , testGroup "numberLengthHex"
        [ testProperty "Int"   $ numberLengthHex <==> lengthIntHex
        , testProperty "Int8"  $ numberLengthHex <==> lengthInt8hex
        , testProperty "Int16" $ numberLengthHex <==> lengthInt16hex
        , testProperty "Int32" $ numberLengthHex <==> lengthInt32hex
        , testProperty "Int32" $ numberLengthHex <==> lengthInt64hex
        ]

    , testGroup "maxNumberLength"
        [ testCase "Int"
            $ maxNumberLength int    @?= lengthInt    maxBound
        , testCase "Int8"
            $ maxNumberLength int8   @?= lengthInt8   maxBound
        , testCase "Int16"
            $ maxNumberLength int16  @?= lengthInt16  maxBound
        , testCase "Int32"
            $ maxNumberLength int32  @?= lengthInt32  maxBound
        , testCase "Int64"
            $ maxNumberLength int64  @?= lengthInt64  maxBound
        , testCase "Word"
            $ maxNumberLength word   @?= lengthWord   maxBound
        , testCase "Word8"
            $ maxNumberLength word8  @?= lengthWord8  maxBound
        , testCase "Word16"
            $ maxNumberLength word16 @?= lengthWord16 maxBound
        , testCase "Word32"
            $ maxNumberLength word32 @?= lengthWord32 maxBound
        , testCase "Word64"
            $ maxNumberLength word64 @?= lengthWord64 maxBound
        ]

    , testGroup "maxNumberLengthHex"
        [ testCase "Int"
            $ maxNumberLengthHex int    @?= lengthIntHex    maxBound
        , testCase "Int8"
            $ maxNumberLengthHex int8   @?= lengthInt8hex   maxBound
        , testCase "Int16"
            $ maxNumberLengthHex int16  @?= lengthInt16hex  maxBound
        , testCase "Int32"
            $ maxNumberLengthHex int32  @?= lengthInt32hex  maxBound
        , testCase "Int64"
            $ maxNumberLengthHex int64  @?= lengthInt64hex  maxBound
        , testCase "Word"
            $ maxNumberLengthHex word   @?= lengthWordHex   maxBound
        , testCase "Word8"
            $ maxNumberLengthHex word8  @?= lengthWord8hex  maxBound
        , testCase "Word16"
            $ maxNumberLengthHex word16 @?= lengthWord16hex maxBound
        , testCase "Word32"
            $ maxNumberLengthHex word32 @?= lengthWord32hex maxBound
        , testCase "Word64"
            $ maxNumberLengthHex word64 @?= lengthWord64hex maxBound
        ]
    ]
  where
    int    = Proxy :: Proxy Int
    int8   = Proxy :: Proxy Int8
    int16  = Proxy :: Proxy Int16
    int32  = Proxy :: Proxy Int32
    int64  = Proxy :: Proxy Int64

    word   = Proxy :: Proxy Word
    word8  = Proxy :: Proxy Word8
    word16 = Proxy :: Proxy Word16
    word32 = Proxy :: Proxy Word32
    word64 = Proxy :: Proxy Word64

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)
