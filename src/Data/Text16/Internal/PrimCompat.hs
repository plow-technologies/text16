{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.Text16.Internal.PrimCompat
  ( word8ToWord#
  , wordToWord8#

  , word16ToWord#
  , wordToWord16#

  , wordToWord32#
  , word32ToWord#
  ) where

#if MIN_VERSION_base(4,16,0)

import GHC.Base

#else

import GHC.Prim (Word#)

wordToWord8#,  word8ToWord#  :: Word# -> Word#
wordToWord16#, word16ToWord# :: Word# -> Word#
wordToWord32#, word32ToWord# :: Word# -> Word#
word8ToWord#  w = w
word16ToWord# w = w
word32ToWord# w = w
wordToWord8#  w = w
wordToWord16# w = w
wordToWord32# w = w
{-# INLINE wordToWord16# #-}
{-# INLINE word16ToWord# #-}
{-# INLINE wordToWord32# #-}
{-# INLINE word32ToWord# #-}

#endif
