{-# LANGUAGE BangPatterns, CPP, Rank2Types #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Text16.Internal.Builder
-- Copyright   : (c) 2013 Bryan O'Sullivan
--               (c) 2010 Johan Tibell
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : Johan Tibell <johan.tibell@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
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
-- as the latter associates @mappend@ to the left.
--
-----------------------------------------------------------------------------

module Data.Text16.Internal.Builder
   ( -- * Public API
     -- ** The Builder type
     Builder
   , toLazyText16
   , toLazyText16With

     -- ** Constructing Builders
   , singleton
   , fromText16
   , fromLazyText16
   , fromString

     -- ** Flushing the buffer state
   , flush

     -- * Internal functions
   , append'
   , ensureFree
   , writeN
   ) where

import Control.Monad.ST (ST, runST)
import Data.Monoid (Monoid(..))
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Text16.Internal (Text16(..), safe)
import Data.Text16.Internal.Lazy (smallChunkSize)
import Data.Text16.Unsafe (inlineInterleaveST)
import Data.Text16.Internal.Unsafe.Char (unsafeWrite)
import Prelude hiding (map, putChar)

import qualified Data.String as String
import qualified Data.Text16 as S
import qualified Data.Text16.Array as A
import qualified Data.Text16.Lazy as L

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

------------------------------------------------------------------------

-- | A @Builder@ is an efficient way to build lazy @Text16@ values.
-- There are several functions for constructing builders, but only one
-- to inspect them: to extract any data, you have to turn them into
-- lazy @Text16@ values using @toLazyText16@.
--
-- Internally, a builder constructs a lazy @Text16@ by filling arrays
-- piece by piece.  As each buffer is filled, it is \'popped\' off, to
-- become a new chunk of the resulting lazy @Text16@.  All this is
-- hidden from the user of the @Builder@.
newtype Builder = Builder {
     -- Invariant (from Data.Text16.Lazy):
     --      The lists include no null Text16s.
     runBuilder :: forall s. (Buffer s -> ST s [S.Text16])
                -> Buffer s
                -> ST s [S.Text16]
   }

instance Semigroup Builder where
   (<>) = append
   {-# INLINE (<>) #-}

instance Monoid Builder where
   mempty  = empty
   {-# INLINE mempty #-}
   mappend = (<>)
   {-# INLINE mappend #-}
   mconcat = foldr mappend Data.Monoid.mempty
   {-# INLINE mconcat #-}

instance String.IsString Builder where
    fromString = fromString
    {-# INLINE fromString #-}

instance Show Builder where
    show = show . toLazyText16

instance Eq Builder where
    a == b = toLazyText16 a == toLazyText16 b

instance Ord Builder where
    a <= b = toLazyText16 a <= toLazyText16 b

------------------------------------------------------------------------

-- | /O(1)./ The empty @Builder@, satisfying
--
--  * @'toLazyText16' 'empty' = 'L.empty'@
--
empty :: Builder
empty = Builder (\ k buf -> k buf)
{-# INLINE empty #-}

-- | /O(1)./ A @Builder@ taking a single character, satisfying
--
--  * @'toLazyText16' ('singleton' c) = 'L.singleton' c@
--
singleton ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> Builder
singleton c = writeAtMost 2 $ \ marr o -> unsafeWrite marr o (safe c)
{-# INLINE singleton #-}

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two builders, an associative
-- operation with identity 'empty', satisfying
--
--  * @'toLazyText16' ('append' x y) = 'L.append' ('toLazyText16' x) ('toLazyText16' y)@
--
append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE [0] append #-}

-- TODO: Experiment to find the right threshold.
copyLimit :: Int
copyLimit = 128

-- This function attempts to merge small @Text16@ values instead of
-- treating each value as its own chunk.  We may not always want this.

-- | /O(1)./ A @Builder@ taking a 'S.Text16', satisfying
--
--  * @'toLazyText16' ('fromText16' t) = 'L.fromChunks' [t]@
--
fromText16 :: S.Text16 -> Builder
fromText16 t@(Text16 arr off l)
    | S.null t       = empty
    | l <= copyLimit = writeN l $ \marr o -> A.copyI marr o arr off (l+o)
    | otherwise      = flush `append` mapBuilder (t :)
{-# INLINE [1] fromText16 #-}

{-# RULES
"fromText16/pack" forall s .
        fromText16 (S.pack s) = fromString s
 #-}

-- | /O(1)./ A Builder taking a @String@, satisfying
--
--  * @'toLazyText16' ('fromString' s) = 'L.fromChunks' [S.pack s]@
--
fromString :: String -> Builder
fromString str = Builder $ \k (Buffer p0 o0 u0 l0) ->
    let loop !marr !o !u !l [] = k (Buffer marr o u l)
        loop marr o u l s@(c:cs)
            | l <= 1 = do
                arr <- A.unsafeFreeze marr
                let !t = Text16 arr o u
                marr' <- A.new chunkSize
                ts <- inlineInterleaveST (loop marr' 0 0 chunkSize s)
                return $ t : ts
            | otherwise = do
                n <- unsafeWrite marr (o+u) (safe c)
                loop marr o (u+n) (l-n) cs
    in loop p0 o0 u0 l0 str
  where
    chunkSize = smallChunkSize
{-# INLINE fromString #-}

-- | /O(1)./ A @Builder@ taking a lazy @Text16@, satisfying
--
--  * @'toLazyText16' ('fromLazyText16' t) = t@
--
fromLazyText16 :: L.Text16 -> Builder
fromLazyText16 ts = flush `append` mapBuilder (L.toChunks ts ++)
{-# INLINE fromLazyText16 #-}

------------------------------------------------------------------------

-- Our internal buffer type
data Buffer s = Buffer {-# UNPACK #-} !(A.MArray s)
                       {-# UNPACK #-} !Int  -- offset
                       {-# UNPACK #-} !Int  -- used units
                       {-# UNPACK #-} !Int  -- length left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy @Text16@ from a @Builder@ with a default
-- buffer size.  The construction work takes place if and when the
-- relevant part of the lazy @Text16@ is demanded.
toLazyText16 :: Builder -> L.Text16
toLazyText16 = toLazyText16With smallChunkSize

-- | /O(n)./ Extract a lazy @Text16@ from a @Builder@, using the given
-- size for the initial buffer.  The construction work takes place if
-- and when the relevant part of the lazy @Text16@ is demanded.
--
-- If the initial buffer is too small to hold all data, subsequent
-- buffers will be the default buffer size.
toLazyText16With :: Int -> Builder -> L.Text16
toLazyText16With chunkSize m = L.fromChunks (runST $
  newBuffer chunkSize >>= runBuilder (m `append` flush) (const (return [])))

-- | /O(1)./ Pop the strict @Text16@ we have constructed so far, if any,
-- yielding a new chunk in the result lazy @Text16@.
flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
    then k buf
    else do arr <- A.unsafeFreeze p
            let !b = Buffer p (o+u) 0 l
                !t = Text16 arr o u
            ts <- inlineInterleaveST (k b)
            return $! t : ts
{-# INLINE [1] flush #-}
-- defer inlining so that flush/flush rule may fire.

------------------------------------------------------------------------

-- | Sequence an ST operation on the buffer
withBuffer :: (forall s. Buffer s -> ST s (Buffer s)) -> Builder
withBuffer f = Builder $ \k buf -> f buf >>= k
{-# INLINE withBuffer #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf
{-# INLINE withSize #-}

-- | Map the resulting list of texts.
mapBuilder :: ([S.Text16] -> [S.Text16]) -> Builder
mapBuilder f = Builder (fmap f .)

------------------------------------------------------------------------

-- | Ensure that there are at least @n@ many elements available.
ensureFree :: Int -> Builder
ensureFree !n = withSize $ \ l ->
    if n <= l
    then empty
    else flush `append'` withBuffer (const (newBuffer (max n smallChunkSize)))
{-# INLINE [0] ensureFree #-}

writeAtMost :: Int -> (forall s. A.MArray s -> Int -> ST s Int) -> Builder
writeAtMost n f = ensureFree n `append'` withBuffer (writeBuffer f)
{-# INLINE [0] writeAtMost #-}

-- | Ensure that @n@ many elements are available, and then use @f@ to
-- write some elements into the memory.
writeN :: Int -> (forall s. A.MArray s -> Int -> ST s ()) -> Builder
writeN n f = writeAtMost n (\ p o -> f p o >> return n)
{-# INLINE writeN #-}

writeBuffer :: (A.MArray s -> Int -> ST s Int) -> Buffer s -> ST s (Buffer s)
writeBuffer f (Buffer p o u l) = do
    n <- f p (o+u)
    return $! Buffer p o (u+n) (l-n)
{-# INLINE writeBuffer #-}

newBuffer :: Int -> ST s (Buffer s)
newBuffer size = do
    arr <- A.new size
    return $! Buffer arr 0 0 size
{-# INLINE newBuffer #-}

------------------------------------------------------------------------
-- Some nice rules for Builder

-- This function makes GHC understand that 'writeN' and 'ensureFree'
-- are *not* recursive in the precense of the rewrite rules below.
-- This is not needed with GHC 7+.
append' :: Builder -> Builder -> Builder
append' (Builder f) (Builder g) = Builder (f . g)
{-# INLINE append' #-}

{-# RULES

"append/writeAtMost" forall a b (f::forall s. A.MArray s -> Int -> ST s Int)
                           (g::forall s. A.MArray s -> Int -> ST s Int) ws.
    append (writeAtMost a f) (append (writeAtMost b g) ws) =
        append (writeAtMost (a+b) (\marr o -> f marr o >>= \ n ->
                                    g marr (o+n) >>= \ m ->
                                    let s = n+m in s `seq` return s)) ws

"writeAtMost/writeAtMost" forall a b (f::forall s. A.MArray s -> Int -> ST s Int)
                           (g::forall s. A.MArray s -> Int -> ST s Int).
    append (writeAtMost a f) (writeAtMost b g) =
        writeAtMost (a+b) (\marr o -> f marr o >>= \ n ->
                            g marr (o+n) >>= \ m ->
                            let s = n+m in s `seq` return s)

"ensureFree/ensureFree" forall a b .
    append (ensureFree a) (ensureFree b) = ensureFree (max a b)

"flush/flush"
    append flush flush = flush

 #-}
