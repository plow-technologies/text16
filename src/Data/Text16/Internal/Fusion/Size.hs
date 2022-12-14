{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- |
-- Module      : Data.Text16.Internal.Fusion.Internal
-- Copyright   : (c) Roman Leshchinskiy 2008,
--               (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Size hints.

module Data.Text16.Internal.Fusion.Size
    (
      Size
      -- * Sizes
    , exactSize
    , maxSize
    , betweenSize
    , unknownSize
    , unionSize
    , charSize
    , codePointsSize
      -- * Querying sizes
    , exactly
    , smaller
    , larger
    , upperBound
    , lowerBound
    , compareSize
    , isEmpty
    ) where

import Data.Char (ord)
import Data.Text16.Internal (mul)
#if defined(ASSERTS)
import Control.Exception (assert)
#endif

-- | A size in UTF-16 code units.
data Size = Between {-# UNPACK #-} !Int {-# UNPACK #-} !Int -- ^ Lower and upper bounds on size.
          | Unknown                                         -- ^ Unknown size.
            deriving (Eq, Show)

exactly :: Size -> Maybe Int
exactly (Between na nb) | na == nb = Just na
exactly _ = Nothing
{-# INLINE exactly #-}

-- | The 'Size' of the given code point.
charSize :: Char -> Size
charSize c
  | ord c < 0x10000 = exactSize 1
  | otherwise       = exactSize 2

-- | The 'Size' of @n@ code points.
codePointsSize :: Int -> Size
codePointsSize n =
#if defined(ASSERTS)
    assert (n >= 0)
#endif
    Between n (2*n)
{-# INLINE codePointsSize #-}

exactSize :: Int -> Size
exactSize n =
#if defined(ASSERTS)
    assert (n >= 0)
#endif
    Between n n
{-# INLINE exactSize #-}

maxSize :: Int -> Size
maxSize n =
#if defined(ASSERTS)
    assert (n >= 0)
#endif
    Between 0 n
{-# INLINE maxSize #-}

betweenSize :: Int -> Int -> Size
betweenSize m n =
#if defined(ASSERTS)
    assert (m >= 0)
    assert (n >= m)
#endif
    Between m n
{-# INLINE betweenSize #-}

unionSize :: Size -> Size -> Size
unionSize (Between a b) (Between c d) = Between (min a c) (max b d)
unionSize _ _ = Unknown

unknownSize :: Size
unknownSize = Unknown
{-# INLINE unknownSize #-}

instance Num Size where
    (+) = addSize
    (-) = subtractSize
    (*) = mulSize

    fromInteger = f where f = exactSize . fromInteger
                          {-# INLINE f #-}

add :: Int -> Int -> Int
add m n | mn >=   0 = mn
        | otherwise = overflowError
  where mn = m + n
{-# INLINE add #-}

addSize :: Size -> Size -> Size
addSize (Between ma mb) (Between na nb) = Between (add ma na) (add mb nb)
addSize _               _               = Unknown
{-# INLINE addSize #-}

subtractSize :: Size -> Size -> Size
subtractSize (Between ma mb) (Between na nb) = Between (max (ma-nb) 0) (max (mb-na) 0)
subtractSize a@(Between 0 _) Unknown         = a
subtractSize (Between _ mb)  Unknown         = Between 0 mb
subtractSize _               _               = Unknown
{-# INLINE subtractSize #-}

mulSize :: Size -> Size -> Size
mulSize (Between ma mb) (Between na nb) = Between (mul ma na) (mul mb nb)
mulSize _               _               = Unknown
{-# INLINE mulSize #-}

-- | Minimum of two size hints.
smaller :: Size -> Size -> Size
smaller a@(Between ma mb) b@(Between na nb)
    | mb <= na  = a
    | nb <= ma  = b
    | otherwise = Between (ma `min` na) (mb `min` nb)
smaller a@(Between 0 _) Unknown         = a
smaller (Between _ mb)  Unknown         = Between 0 mb
smaller Unknown         b@(Between 0 _) = b
smaller Unknown         (Between _ nb)  = Between 0 nb
smaller Unknown         Unknown         = Unknown
{-# INLINE smaller #-}

-- | Maximum of two size hints.
larger :: Size -> Size -> Size
larger a@(Between ma mb) b@(Between na nb)
    | ma >= nb  = a
    | na >= mb  = b
    | otherwise = Between (ma `max` na) (mb `max` nb)
larger _ _ = Unknown
{-# INLINE larger #-}

-- | Compute the maximum size from a size hint, if possible.
upperBound :: Int -> Size -> Int
upperBound _ (Between _ n) = n
upperBound k _             = k
{-# INLINE upperBound #-}

-- | Compute the maximum size from a size hint, if possible.
lowerBound :: Int -> Size -> Int
lowerBound _ (Between n _) = n
lowerBound k _             = k
{-# INLINE lowerBound #-}

-- | Determine the ordering relationship between two 'Size's, or 'Nothing' in
-- the indeterminate case.
compareSize :: Size -> Size -> Maybe Ordering
compareSize (Between ma mb) (Between na nb)
  | mb < na            = Just LT
  | ma > nb            = Just GT
  | ma == mb
  , ma == na
  , ma == nb           = Just EQ
compareSize _ _        = Nothing


isEmpty :: Size -> Bool
isEmpty (Between _ n) = n <= 0
isEmpty _             = False
{-# INLINE isEmpty #-}

overflowError :: Int
overflowError = error "Data.Text16.Internal.Fusion.Size: size overflow"
