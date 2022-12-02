-- | Compare a string with a copy of itself that is identical except
-- for the last character.
--
-- Tested in this benchmark:
--
-- * Comparison of strings (Eq instance)
--
module Benchmarks.Equality
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.Encoding as TL

type Env = (T.Text16, TL.Text16)

initEnv :: FilePath -> IO Env
initEnv fp = do
  b <- B.readFile fp
  bl1 <- BL.readFile fp
  return (T.decodeUtf8 b, TL.decodeUtf8 bl1)

benchmark :: Env -> Benchmark
benchmark ~(t, tl) =
  bgroup "Equality"
    [ bench "Text16" $ whnf (== T.init t `T.snoc` '\xfffd') t
    , bench "LazyText16" $ whnf (== TL.init tl `TL.snoc` '\xfffd') tl
    ]
