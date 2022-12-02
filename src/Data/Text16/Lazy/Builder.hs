{-# LANGUAGE BangPatterns, CPP, Rank2Types #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Text16.Lazy.Builder
-- Copyright   : (c) 2013 Bryan O'Sullivan
--               (c) 2010 Johan Tibell
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : Johan Tibell <johan.tibell@gmail.com>
-- Portability : portable to Hugs and GHC
--
-- Efficient construction of lazy @Text16@ values.  The principal
-- operations on a @Builder@ are @singleton@, @fromText16@, and
-- @fromLazyText16@, which construct new builders, and 'mappend', which
-- concatenates two builders.
--
-- To get maximum performance when building lazy @Text16@ values using a
-- builder, associate @mappend@ calls to the right.  For example,
-- prefer
--
-- > singleton 'a' `mappend` (singleton 'b' `mappend` singleton 'c')
--
-- to
--
-- > singleton 'a' `mappend` singleton 'b' `mappend` singleton 'c'
--
-- as the latter associates @mappend@ to the left. Or, equivalently,
-- prefer
--
--  > singleton 'a' <> singleton 'b' <> singleton 'c'
--
-- since the '<>' from recent versions of 'Data.Monoid' associates
-- to the right.

-----------------------------------------------------------------------------

module Data.Text16.Lazy.Builder
   ( -- * The Builder type
     Builder
   , toLazyText16
   , toLazyText16With

     -- * Constructing Builders
   , singleton
   , fromText16
   , fromLazyText16
   , fromString

     -- * Flushing the buffer state
   , flush
   ) where

import Data.Text16.Internal.Builder
