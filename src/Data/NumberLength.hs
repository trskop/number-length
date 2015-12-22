{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Polymorphic interface for getting number of digits of a number
--               in decimal or hexadecimal representation.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DefaultSignatures, NoImplicitPrelude
--
-- Polymorphic interface for getting number of digits of a number in decimal or
-- hexadecimal representation.
module Data.NumberLength
    (
      NumberLength(..)
    , SignedNumberLength(..)
    , BoundedNumberLength(..)
    )
  where

import Prelude(Num((+)), fromIntegral)

import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ord (Ord((<)))
import Data.Proxy (Proxy)
import Data.Word (Word, Word16, Word32, Word64, Word8)

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
import Data.NumberLength.Internal (either32or64)
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


-- | Get number of digits of a number in base 10 and base 16. Note the
-- following:
--
-- * There is no 'Num' constraint, so that type wrappers aren't forced to
--   provide instance for it. This is because there are things represented
--   using numbers, but they aren't numbers, e.g. telephone numbers.
--
-- * This type class doesn't handle signed numbers, in an intuitive way. See
--   also 'SignedNumberLength'.
--
-- * There is a special class for bounded numbers, see 'BoundedNumberLength',
--   that provides similar functionality as 'Prelude.Bounded', but for number
--   of digits in a number.
class NumberLength a where
    -- | Get number of digits in base 10 for specified number. Note that if
    -- number is signed, then this function will return length of absolute
    -- number.
    --
    -- >>> numberLength (123 :: Int)
    -- 3
    -- >>> numberLength (-123 :: Int)
    -- 3
    --
    -- See also 'signedNumberLength'.
    numberLength :: a -> Int

    -- | Get number of digits in base 16 for specified number. Note that if
    -- number is signed, then this function will return length of absolute
    -- number.
    --
    -- >>> numberLengthHex (123 :: Int)  -- 123 = 7b in hex
    -- 2
    -- >>> numberLengthHex (-123 :: Int)
    -- 2
    --
    -- See also 'signedNumberLengthHex'.
    numberLengthHex :: a -> Int

-- | Get number of digits of a signed number in base 10 and base 16.
class NumberLength a => SignedNumberLength a where
    {-# MINIMAL signedNumberLengthHex #-}

    -- | Get number of digits in base 10 for specified number.
    --
    -- >>> signedNumberLength (123 :: Int)
    -- 3
    -- >>> signedNumberLength (-123 :: Int)
    -- 4
    --
    -- Default implementation provided if @a@ has also 'Num' and 'Ord'
    -- instances:
    --
    -- @
    -- 'signedNumberLength' n = signLength + 'numberLength' n
    --   where
    --     signLength = if n < 0 then 1 else 0
    -- @
    signedNumberLength :: a -> Int

    default signedNumberLength :: (Num a, Ord a) => a -> Int
    signedNumberLength n = signLength + numberLength n
      where
        signLength = if n < 0 then 1 else 0

    -- | Get number of digits in base 16 for specified number.
    --
    -- >>> signedNumberLengthHex (123 :: Int)
    -- 2
    -- >>> signedNumberLengthHex (-123 :: Int)
    -- 16
    --
    -- Negative number is shown as ones' complement, e.g. @(-123 :: Int) =
    -- ffffffffffffff85@ on 64 bit platform.
    signedNumberLengthHex :: a -> Int

-- | Get maximum number of digits of a number in base 10 and 16. Minimal number
-- of digits is considered to be always 1, and therefore there is no method for
-- it.
class NumberLength a => BoundedNumberLength a where
    -- | Get maximum number of digits of a number in base 10.
    maxNumberLength :: Proxy a -> Int

    -- | Get maximum number of digits of a number in base 16.
    maxNumberLengthHex :: Proxy a -> Int

-- {{{ Int* -------------------------------------------------------------------

instance NumberLength Int where
    numberLength = lengthInt
    numberLengthHex = lengthIntHex

instance SignedNumberLength Int where
    signedNumberLengthHex n = numberLengthHex (fromIntegral n :: Word)

instance BoundedNumberLength Int where
    maxNumberLength _ = 10 `either32or64` 19
    maxNumberLengthHex _ = 8 `either32or64` 16

instance NumberLength Int64 where
    numberLength = lengthInt64
    numberLengthHex = lengthInt64hex

instance SignedNumberLength Int64 where
    signedNumberLengthHex n = numberLengthHex (fromIntegral n :: Word64)

instance BoundedNumberLength Int64 where
    maxNumberLength _ = 19
    maxNumberLengthHex _ = 16

instance NumberLength Int32 where
    numberLength = lengthInt32
    numberLengthHex = lengthInt32hex

instance SignedNumberLength Int32 where
    signedNumberLengthHex n = numberLengthHex (fromIntegral n :: Word32)

instance BoundedNumberLength Int32 where
    maxNumberLength _ = 10
    maxNumberLengthHex _ = 8

instance NumberLength Int16 where
    numberLength = lengthInt16
    numberLengthHex = lengthInt16hex

instance SignedNumberLength Int16 where
    signedNumberLengthHex n = numberLengthHex (fromIntegral n :: Word16)

instance BoundedNumberLength Int16 where
    maxNumberLength _ = 5
    maxNumberLengthHex _ = 4

instance NumberLength Int8 where
    numberLength = lengthInt8
    numberLengthHex = lengthInt8hex

instance SignedNumberLength Int8 where
    signedNumberLengthHex n = numberLengthHex (fromIntegral n :: Word8)

instance BoundedNumberLength Int8 where
    maxNumberLength _ = 3
    maxNumberLengthHex _ = 2

-- }}} Int* -------------------------------------------------------------------
-- {{{ Word* ------------------------------------------------------------------

instance NumberLength Word where
    numberLength = lengthWord
    numberLengthHex = lengthWordHex

instance BoundedNumberLength Word where
    maxNumberLength _ = 10 `either32or64` 20
    maxNumberLengthHex _ = 8 `either32or64` 16

instance NumberLength Word64 where
    numberLength = lengthWord64
    numberLengthHex = lengthWord64hex

instance BoundedNumberLength Word64 where
    maxNumberLength _ = 20
    maxNumberLengthHex _ = 16

instance NumberLength Word32 where
    numberLength = lengthWord32
    numberLengthHex = lengthWord32hex

instance BoundedNumberLength Word32 where
    maxNumberLength _ = 10
    maxNumberLengthHex _ = 8

instance NumberLength Word16 where
    numberLength = lengthWord16
    numberLengthHex = lengthWord16hex

instance BoundedNumberLength Word16 where
    maxNumberLength _ = 5
    maxNumberLengthHex _ = 4

instance NumberLength Word8 where
    numberLength = lengthWord8
    numberLengthHex = lengthWord8hex

instance BoundedNumberLength Word8 where
    maxNumberLength _ = 3
    maxNumberLengthHex _ = 2

-- }}} Word* ------------------------------------------------------------------
