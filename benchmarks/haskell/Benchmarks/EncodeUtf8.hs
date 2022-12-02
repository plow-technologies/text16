-- | UTF-8 encode a text
--
-- Tested in this benchmark:
--
-- * Replicating a string a number of times
--
-- * UTF-8 encoding it
--
module Benchmarks.EncodeUtf8
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.Encoding as TL

benchmark :: String -> String -> Benchmark
benchmark name string =
    bgroup "EncodeUtf8"
        [ bench ("Text16 (" ++ name ++ ")")     $ whnf (B.length . T.encodeUtf8)   text
        , bench ("LazyText16 (" ++ name ++ ")") $ whnf (BL.length . TL.encodeUtf8) lazyText16
        ]
  where
    -- The string in different formats
    text = T.replicate k $ T.pack string
    lazyText16 = TL.replicate (fromIntegral k) $ TL.pack string

    -- Amount
    k = 100000
