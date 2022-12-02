{-# LANGUAGE BangPatterns, OverloadedStrings, RankNTypes #-}

module Benchmarks.Multilang (benchmark) where

import qualified Data.ByteString as B
import qualified Data.Text16 as Text16
import Data.Text16.Encoding (decodeUtf8)
import Data.Text16 (Text16)
import Test.Tasty.Bench (Benchmark, bgroup, bench, env, nf)

readYiwiki :: IO Text16
readYiwiki = decodeUtf8 `fmap` B.readFile "benchmarks/text-test-data/yiwiki.xml"

benchmark :: Benchmark
benchmark = env readYiwiki $ \content -> bgroup "Multilang"
  [ bench "find_first" $ nf (Text16.isInfixOf "en:Benin") content
  , bench "find_index" $ nf (Text16.findIndex (=='c')) content
  ]
