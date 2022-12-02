{-# LANGUAGE CPP, MagicHash #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Text16.Show
-- Copyright   : (c) 2009-2015 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text16.Show
    (
      singleton
    , unpack
    , unpackCString#
    ) where

import Control.Monad.ST (ST)
import Data.Text16.Internal (Text16(..), empty_, safe)
import Data.Text16.Internal.Fusion (stream, unstream)
import Data.Text16.Internal.Unsafe.Char (unsafeWrite)
import GHC.Prim (Addr#)
import qualified Data.Text16.Array as A
import qualified Data.Text16.Internal.Fusion.Common as S

import qualified GHC.CString as GHC

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

instance Show Text16 where
    showsPrec p ps r = showsPrec p (unpack ps) r

-- | /O(n)/ Convert a 'Text16' into a 'String'.  Subject to fusion.
unpack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text16 -> String
unpack = S.unstreamList . stream
{-# INLINE [1] unpack #-}

-- | /O(n)/ Convert a literal string into a 'Text16'.
--
-- This is exposed solely for people writing GHC rewrite rules.
--
-- @since 1.2.1.1
unpackCString# :: Addr# -> Text16
unpackCString# addr# = unstream (S.streamCString# addr#)
{-# NOINLINE unpackCString# #-}

{-# RULES "TEXT literal" [1] forall a.
    unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
      = unpackCString# a #-}

{-# RULES "TEXT literal UTF8" [1] forall a.
    unstream (S.map safe (S.streamList (GHC.unpackCStringUtf8# a)))
      = unpackCString# a #-}

{-# RULES "TEXT empty literal" [1]
    unstream (S.map safe (S.streamList []))
      = empty_ #-}

{-# RULES "TEXT singleton literal" [1] forall a.
    unstream (S.map safe (S.streamList [a]))
      = singleton_ a #-}

-- | /O(1)/ Convert a character into a Text16.  Subject to fusion.
-- Performs replacement on invalid scalar values.
singleton ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> Text16
singleton = unstream . S.singleton . safe
{-# INLINE [1] singleton #-}

{-# RULES "TEXT singleton" forall a.
    unstream (S.singleton (safe a))
      = singleton_ a #-}

-- This is intended to reduce inlining bloat.
singleton_ ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> Text16
singleton_ c = Text16 (A.run x) 0 len
  where x :: ST s (A.MArray s)
        x = do arr <- A.new len
               _ <- unsafeWrite arr 0 d
               return arr
        len | d < '\x10000' = 1
            | otherwise     = 2
        d = safe c
{-# NOINLINE singleton_ #-}
