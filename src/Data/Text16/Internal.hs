{-# LANGUAGE CPP, DeriveDataTypeable, UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text16.Internal
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A module containing private 'Text16' internals. This exposes the
-- 'Text16' representation and low level construction functions.
-- Modules which extend the 'Text16' system may need to use this module.
--
-- You should not use this module unless you are determined to monkey
-- with the internals, as the functions here do just about nothing to
-- preserve data invariants.  You have been warned!

module Data.Text16.Internal
    (
    -- * Types
    -- $internals
      Text16(..)
    -- * Construction
    , text
    , textP
    -- * Safety
    , safe
    -- * Code that must be here for accessibility
    , empty
    , empty_
    -- * Utilities
    , firstf
    -- * Checked multiplication
    , mul
    , mul32
    , mul64
    -- * Debugging
    , showText16
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
#endif
import Data.Bits
import Data.Int (Int32, Int64)
import Data.Text16.Internal.Unsafe.Char (ord)
import Data.Typeable (Typeable)
import qualified Data.Text16.Array as A

-- | A space efficient, packed, unboxed Unicode text type.
data Text16 = Text16
    {-# UNPACK #-} !A.Array          -- payload (Word16 elements)
    {-# UNPACK #-} !Int              -- offset (units of Word16, not Char)
    {-# UNPACK #-} !Int              -- length (units of Word16, not Char)
    deriving (Typeable)

-- | Smart constructor.
text_ ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  A.Array -> Int -> Int -> Text16
text_ arr off len =
#if defined(ASSERTS)
  let c    = A.unsafeIndex arr off
  in assert (len >= 0) .
     assert (off >= 0) .
     assert (len == 0 || c < 0xDC00 || c > 0xDFFF) $
#endif
     Text16 arr off len
{-# INLINE text_ #-}

-- | /O(1)/ The empty 'Text16'.
empty :: Text16
empty = Text16 A.empty 0 0
{-# INLINE [1] empty #-}

-- | A non-inlined version of 'empty'.
empty_ :: Text16
empty_ = Text16 A.empty 0 0
{-# NOINLINE empty_ #-}

-- | Construct a 'Text16' without invisibly pinning its byte array in
-- memory if its length has dwindled to zero.
text ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  A.Array -> Int -> Int -> Text16
text arr off len | len == 0  = empty
                 | otherwise = text_ arr off len
{-# INLINE text #-}

textP :: A.Array -> Int -> Int -> Text16
{-# DEPRECATED textP "Use text instead" #-}
textP = text

-- | A useful 'show'-like function for debugging purposes.
showText16 :: Text16 -> String
showText16 (Text16 arr off len) =
    "Text16 " ++ show (A.toList arr off len) ++ ' ' :
            show off ++ ' ' : show len

-- | Map a 'Char' to a 'Text16'-safe value.
--
-- UTF-16 surrogate code points are not included in the set of Unicode
-- scalar values, but are unfortunately admitted as valid 'Char'
-- values by Haskell.  They cannot be represented in a 'Text16'.  This
-- function remaps those code points to the Unicode replacement
-- character (U+FFFD, \'&#xfffd;\'), and leaves other code points
-- unchanged.
safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'
{-# INLINE [0] safe #-}

-- | Apply a function to the first element of an optional pair.
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul :: Int -> Int -> Int
mul a b
  | finiteBitSize (0 :: Word) == 64
  = int64ToInt $ intToInt64 a `mul64` intToInt64 b
  | otherwise
  = int32ToInt $ intToInt32 a `mul32` intToInt32 b
{-# INLINE mul #-}
infixl 7 `mul`

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul64 :: Int64 -> Int64 -> Int64
mul64 a b
  | a >= 0 && b >= 0 =  mul64_ a b
  | a >= 0           = -mul64_ a (-b)
  | b >= 0           = -mul64_ (-a) b
  | otherwise        =  mul64_ (-a) (-b)
{-# INLINE mul64 #-}
infixl 7 `mul64`

mul64_ :: Int64 -> Int64 -> Int64
mul64_ a b
  | ahi > 0 && bhi > 0 = error "overflow"
  | top > 0x7fffffff   = error "overflow"
  | total < 0          = error "overflow"
  | otherwise          = total
  where (# ahi, alo #) = (# a `shiftR` 32, a .&. 0xffffffff #)
        (# bhi, blo #) = (# b `shiftR` 32, b .&. 0xffffffff #)
        top            = ahi * blo + alo * bhi
        total          = (top `shiftL` 32) + alo * blo
{-# INLINE mul64_ #-}

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul32 :: Int32 -> Int32 -> Int32
mul32 a b = case int32ToInt64 a * int32ToInt64 b of
              ab | ab < min32 || ab > max32 -> error "overflow"
                 | otherwise                -> int64ToInt32 ab
  where min32 = -0x80000000 :: Int64
        max32 =  0x7fffffff
{-# INLINE mul32 #-}
infixl 7 `mul32`

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral

intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

int32ToInt64 :: Int32 -> Int64
int32ToInt64 = fromIntegral

int64ToInt32 :: Int64 -> Int32
int64ToInt32 = fromIntegral

-- $internals
--
-- Internally, the 'Text16' type is represented as an array of 'Word16'
-- UTF-16 code units. The offset and length fields in the constructor
-- are in these units, /not/ units of 'Char'.
--
-- Invariants that all functions must maintain:
--
-- * Since the 'Text16' type uses UTF-16 internally, it cannot represent
--   characters in the reserved surrogate code point range U+D800 to
--   U+DFFF. To maintain this invariant, the 'safe' function maps
--   'Char' values in this range to the replacement character (U+FFFD,
--   \'&#xfffd;\').
--
-- * A leading (or \"high\") surrogate code unit (0xD800???0xDBFF) must
--   always be followed by a trailing (or \"low\") surrogate code unit
--   (0xDC00-0xDFFF). A trailing surrogate code unit must always be
--   preceded by a leading surrogate code unit.
