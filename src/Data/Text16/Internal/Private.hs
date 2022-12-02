{-# LANGUAGE BangPatterns, CPP, Rank2Types, UnboxedTuples #-}

-- |
-- Module      : Data.Text16.Internal.Private
-- Copyright   : (c) 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text16.Internal.Private
    (
      runText16
    , span_
    ) where

import Control.Monad.ST (ST, runST)
import Data.Text16.Internal (Text16(..), text)
import Data.Text16.Unsafe (Iter(..), iter)
import qualified Data.Text16.Array as A

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

span_ :: (Char -> Bool) -> Text16 -> (# Text16, Text16 #)
span_ p t@(Text16 arr off len) = (# hd,tl #)
  where hd = text arr off k
        tl = text arr (off+k) (len-k)
        !k = loop 0
        loop !i | i < len && p c = loop (i+d)
                | otherwise      = i
            where Iter c d       = iter t i
{-# INLINE span_ #-}

runText16 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (forall s. (A.MArray s -> Int -> ST s Text16) -> ST s Text16) -> Text16
runText16 act = runST (act $ \ !marr !len -> do
                             arr <- A.unsafeFreeze marr
                             return $! text arr 0 len)
{-# INLINE runText16 #-}
