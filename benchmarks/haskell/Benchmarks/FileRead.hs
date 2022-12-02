-- | Benchmarks simple file reading
--
-- Tested in this benchmark:
--
-- * Reading a file from the disk
--

{-# LANGUAGE CPP #-}

module Benchmarks.FileRead
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy as LT
import qualified Data.Text16.Lazy.Encoding as LT
import qualified Data.Text16.Lazy.IO as LT

benchmark :: FilePath -> Benchmark
benchmark p = bgroup "FileRead"
    [ bench "Text16" $ whnfIO $ T.length <$> T.readFile p
    , bench "LazyText16" $ whnfIO $ LT.length <$> LT.readFile p
    , bench "Text16ByteString" $ whnfIO $
        (T.length . T.decodeUtf8) <$> SB.readFile p
    , bench "LazyText16ByteString" $ whnfIO $
        (LT.length . LT.decodeUtf8) <$> LB.readFile p
    ]
