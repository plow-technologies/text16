-- | Read a file line-by-line using handles, and perform a fold over the lines.
-- The fold is used here to calculate the number of lines in the file.
--
-- Tested in this benchmark:
--
-- * Buffered, line-based IO
--
{-# LANGUAGE BangPatterns #-}
module Benchmarks.FoldLines
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO
import qualified Data.Text16 as T
import qualified Data.Text16.IO as T

benchmark :: FilePath -> Benchmark
benchmark fp = bgroup "ReadLines"
    [ bench "Text16"       $ withHandle $ foldLinesT (\n _ -> n + 1) (0 :: Int)
    ]
  where
    withHandle f = whnfIO $ do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 16384))
        x <- f h
        hClose h
        return x

-- | Text16 line fold
--
foldLinesT :: (a -> T.Text16 -> a) -> a -> Handle -> IO a
foldLinesT f z0 h = go z0
  where
    go !z = do
        eof <- hIsEOF h
        if eof
            then return z
            else do
                l <- T.hGetLine h
                let z' = f z l in go z'
{-# INLINE foldLinesT #-}
