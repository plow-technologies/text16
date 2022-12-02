{-# LANGUAGE OverloadedStrings #-}

module Benchmarks.Concat (benchmark) where

import Control.Monad.Trans.Writer
import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import Data.Text16 as T

benchmark :: Benchmark
benchmark = bgroup "Concat"
  [ bench "append" $ whnf (append4 "Text16 1" "Text16 2" "Text16 3") "Text16 4"
  , bench "concat" $ whnf (concat4 "Text16 1" "Text16 2" "Text16 3") "Text16 4"
  , bench "write"  $ whnf (write4  "Text16 1" "Text16 2" "Text16 3") "Text16 4"
  ]

append4, concat4, write4 :: Text16 -> Text16 -> Text16 -> Text16 -> Text16

{-# NOINLINE append4 #-}
append4 x1 x2 x3 x4 = x1 `append` x2 `append` x3 `append` x4

{-# NOINLINE concat4 #-}
concat4 x1 x2 x3 x4 = T.concat [x1, x2, x3, x4]

{-# NOINLINE write4 #-}
write4 x1 x2 x3 x4 = execWriter $ tell x1 >> tell x2 >> tell x3 >> tell x4
