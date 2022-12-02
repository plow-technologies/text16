-- | This benchmark sorts the lines of a file, like the @sort@ unix utility.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into lines
--
-- * Sorting the lines
--
-- * Joining the lines
--
-- * Writing back to a handle
--
{-# LANGUAGE CPP, OverloadedStrings #-}
module Benchmarks.Programs.Sort
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.Builder as TLB
import qualified Data.Text16.Lazy.Encoding as TL
import qualified Data.Text16.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Benchmark
benchmark i o = bgroup "Sort"
    [ bench "Text16" $ whnfIO $ T.readFile i >>= T.hPutStr o . text
    , bench "LazyText16" $ whnfIO $ TL.readFile i >>= TL.hPutStr o . lazyText16
    , bench "Text16ByteString" $ whnfIO $ B.readFile i >>=
        B.hPutStr o . T.encodeUtf8 . text . T.decodeUtf8
    , bench "LazyText16ByteString" $ whnfIO $ BL.readFile i >>=
        BL.hPutStr o . TL.encodeUtf8 . lazyText16 .  TL.decodeUtf8
    , bench "Text16Builder" $ whnfIO $ B.readFile i >>=
        BL.hPutStr o . TL.encodeUtf8 . textBuilder . T.decodeUtf8
    ]

text :: T.Text16 -> T.Text16
text = T.unlines . L.sort . T.lines

lazyText16 :: TL.Text16 -> TL.Text16
lazyText16 = TL.unlines . L.sort . TL.lines

-- | Text16 variant using a builder monoid for the final concatenation
--
textBuilder :: T.Text16 -> TL.Text16
textBuilder = TLB.toLazyText16 . mconcat . L.intersperse (TLB.singleton '\n') .
    map TLB.fromText16 . L.sort . T.lines
