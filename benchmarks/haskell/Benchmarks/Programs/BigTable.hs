-- | Create a large HTML table and dump it to a handle
--
-- Tested in this benchmark:
--
-- * Creating a large HTML document using a builder
--
-- * Writing to a handle
--
{-# LANGUAGE CPP, OverloadedStrings #-}
module Benchmarks.Programs.BigTable
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bench, whnfIO)
import Data.Text16.Lazy.Builder (Builder, fromText16, toLazyText16)
import Data.Text16.Lazy.IO (hPutStr)
import System.IO (Handle)
import qualified Data.Text16 as T

benchmark :: Handle -> Benchmark
benchmark sink = bench "BigTable" $ whnfIO $ do
    hPutStr sink "Content-Type: text/html\n\n<table>"
    hPutStr sink . toLazyText16 . makeTable =<< rows
    hPutStr sink "</table>"
  where
    -- We provide the number of rows in IO so the builder value isn't shared
    -- between the benchmark samples.
    rows :: IO Int
    rows = return 20000
    {-# NOINLINE rows #-}

makeTable :: Int -> Builder
makeTable n = mconcat $ replicate n $ mconcat $ map makeCol [1 .. 50]

makeCol :: Int -> Builder
makeCol 1 = fromText16 "<tr><td>1</td>"
makeCol 50 = fromText16 "<td>50</td></tr>"
makeCol i = fromText16 "<td>" `mappend` (fromInt i `mappend` fromText16 "</td>")

fromInt :: Int -> Builder
fromInt = fromText16 . T.pack . show
