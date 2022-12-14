-- | Test basic text functions

{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-warn-missing-signatures #-}
module Tests.Properties.Basics
    ( testBasics
    ) where

import Control.Arrow (first, second)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils
import Text.Show.Functions ()
import qualified Data.List as L
import qualified Data.Text16 as T
import qualified Data.Text16.Internal.Fusion as S
import qualified Data.Text16.Internal.Fusion.Common as S
import qualified Data.Text16.Internal.Lazy.Fusion as SL
import qualified Data.Text16.Lazy as TL

s_cons x          = (x:)     `eqP` (unpackS . S.cons x)
s_cons_s x        = (x:)     `eqP` (unpackS . S.unstream . S.cons x)
sf_cons p x       = ((x:) . L.filter p) `eqP` (unpackS . S.cons x . S.filter p)
t_cons x          = (x:)     `eqP` (unpackS . T.cons x)
tl_cons x         = (x:)     `eqP` (unpackS . TL.cons x)
s_snoc x          = (++ [x]) `eqP` (unpackS . (flip S.snoc) x)
t_snoc x          = (++ [x]) `eqP` (unpackS . (flip T.snoc) x)
tl_snoc x         = (++ [x]) `eqP` (unpackS . (flip TL.snoc) x)
s_append s        = (s++)    `eqP` (unpackS . S.append (S.streamList s))
s_append_s s      = (s++)    `eqP`
                    (unpackS . S.unstream . S.append (S.streamList s))
sf_append p s     = (L.filter p s++) `eqP`
                    (unpackS . S.append (S.filter p $ S.streamList s))
t_append s        = (s++)    `eqP` (unpackS . T.append (packS s))

uncons (x:xs) = Just (x,xs)
uncons _      = Nothing

s_uncons          = uncons   `eqP` (fmap (second unpackS) . S.uncons)
sf_uncons p       = (uncons . L.filter p) `eqP`
                    (fmap (second unpackS) . S.uncons . S.filter p)
t_uncons          = uncons   `eqP` (fmap (second unpackS) . T.uncons)
tl_uncons         = uncons   `eqP` (fmap (second unpackS) . TL.uncons)

unsnoc xs@(_:_) = Just (init xs, last xs)
unsnoc []       = Nothing

t_unsnoc          = unsnoc   `eqP` (fmap (first unpackS) . T.unsnoc)
tl_unsnoc         = unsnoc   `eqP` (fmap (first unpackS) . TL.unsnoc)

s_head            = head   `eqP` S.head
sf_head p         = (head . L.filter p) `eqP` (S.head . S.filter p)
t_head            = head   `eqP` T.head
tl_head           = head   `eqP` TL.head
s_last            = last   `eqP` S.last
sf_last p         = (last . L.filter p) `eqP` (S.last . S.filter p)
t_last            = last   `eqP` T.last
tl_last           = last   `eqP` TL.last
s_tail            = tail   `eqP` (unpackS . S.tail)
s_tail_s          = tail   `eqP` (unpackS . S.unstream . S.tail)
sf_tail p         = (tail . L.filter p) `eqP` (unpackS . S.tail . S.filter p)
t_tail            = tail   `eqP` (unpackS . T.tail)
tl_tail           = tail   `eqP` (unpackS . TL.tail)
s_init            = init   `eqP` (unpackS . S.init)
s_init_s          = init   `eqP` (unpackS . S.unstream . S.init)
sf_init p         = (init . L.filter p) `eqP` (unpackS . S.init . S.filter p)
t_init            = init   `eqP` (unpackS . T.init)
tl_init           = init   `eqP` (unpackS . TL.init)
s_null            = null   `eqP` S.null
sf_null p         = (null . L.filter p) `eqP` (S.null . S.filter p)
t_null            = null   `eqP` T.null
tl_null           = null   `eqP` TL.null
s_length          = length `eqP` S.length
sf_length p       = (length . L.filter p) `eqP` (S.length . S.filter p)
sl_length         = (fromIntegral . length) `eqP` SL.length
t_length          = length `eqP` T.length
tl_length         = L.genericLength `eqP` TL.length
t_compareLength t = (compare (T.length t)) `eq` T.compareLength t
tl_compareLength t= (compare (TL.length t)) `eq` TL.compareLength t

-- Regression tests.
s_filter_eq s = S.filter p t == S.streamList (filter p s)
    where p = (/= S.last t)
          t = S.streamList s

testBasics :: TestTree
testBasics =
  testGroup "basics" [
    testProperty "s_cons" s_cons,
    testProperty "s_cons_s" s_cons_s,
    testProperty "sf_cons" sf_cons,
    testProperty "t_cons" t_cons,
    testProperty "tl_cons" tl_cons,
    testProperty "s_snoc" s_snoc,
    testProperty "t_snoc" t_snoc,
    testProperty "tl_snoc" tl_snoc,
    testProperty "s_append" s_append,
    testProperty "s_append_s" s_append_s,
    testProperty "sf_append" sf_append,
    testProperty "t_append" t_append,
    testProperty "s_uncons" s_uncons,
    testProperty "sf_uncons" sf_uncons,
    testProperty "t_uncons" t_uncons,
    testProperty "tl_uncons" tl_uncons,
    testProperty "t_unsnoc" t_unsnoc,
    testProperty "tl_unsnoc" tl_unsnoc,
    testProperty "s_head" s_head,
    testProperty "sf_head" sf_head,
    testProperty "t_head" t_head,
    testProperty "tl_head" tl_head,
    testProperty "s_last" s_last,
    testProperty "sf_last" sf_last,
    testProperty "t_last" t_last,
    testProperty "tl_last" tl_last,
    testProperty "s_tail" s_tail,
    testProperty "s_tail_s" s_tail_s,
    testProperty "sf_tail" sf_tail,
    testProperty "t_tail" t_tail,
    testProperty "tl_tail" tl_tail,
    testProperty "s_init" s_init,
    testProperty "s_init_s" s_init_s,
    testProperty "sf_init" sf_init,
    testProperty "t_init" t_init,
    testProperty "tl_init" tl_init,
    testProperty "s_null" s_null,
    testProperty "sf_null" sf_null,
    testProperty "t_null" t_null,
    testProperty "tl_null" tl_null,
    testProperty "s_length" s_length,
    testProperty "sf_length" sf_length,
    testProperty "sl_length" sl_length,
    testProperty "t_length" t_length,
    testProperty "tl_length" tl_length,
    testProperty "t_compareLength" t_compareLength,
    testProperty "tl_compareLength" tl_compareLength,

    testGroup "regressions" [
      testProperty "s_filter_eq" s_filter_eq
    ]
  ]
