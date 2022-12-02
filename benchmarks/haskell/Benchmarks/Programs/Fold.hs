-- | Benchmark which formats paragraph, like the @sort@ unix utility.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into paragraphs
--
-- * Reformatting the paragraphs to a certain line width
--
-- * Concatenating the results using the text builder
--
-- * Writing back to a handle
--
{-# LANGUAGE CPP, OverloadedStrings #-}
module Benchmarks.Programs.Fold
    ( benchmark
    ) where

import Data.List (foldl')
import Data.List (intersperse)
import System.IO (Handle)
import Test.Tasty.Bench (Benchmark, bench, whnfIO)
import qualified Data.Text16 as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy.Builder as TLB
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Benchmark
benchmark i o =
    bench "Fold" $ whnfIO $ T.readFile i >>= TL.hPutStr o . fold 80

-- | We represent a paragraph by a word list
--
type Paragraph = [T.Text16]

-- | Fold a text
--
fold :: Int -> T.Text16 -> TL.Text16
fold maxWidth = TLB.toLazyText16 . mconcat .
    intersperse "\n\n" . map (foldParagraph maxWidth) . paragraphs

-- | Fold a paragraph
--
foldParagraph :: Int -> Paragraph -> TLB.Builder
foldParagraph _    []       = mempty
foldParagraph max' (w : ws) = fst $ foldl' go (TLB.fromText16 w, T.length w) ws
  where
    go (builder, width) word
        | width + len + 1 <= max' =
            (builder `mappend` " " `mappend` word', width + len + 1)
        | otherwise =
            (builder `mappend` "\n" `mappend` word', len)
      where
        word' = TLB.fromText16 word
        len = T.length word

-- | Divide a text into paragraphs
--
paragraphs :: T.Text16 -> [Paragraph]
paragraphs = splitParagraphs . map T.words . T.lines
  where
    splitParagraphs ls = case break null ls of
        ([], []) -> []
        (p,  []) -> [concat p]
        (p,  lr) -> concat p : splitParagraphs (dropWhile null lr)
