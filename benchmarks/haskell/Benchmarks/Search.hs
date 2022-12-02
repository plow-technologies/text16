-- | Search for a pattern in a file, find the number of occurences
--
-- Tested in this benchmark:
--
-- * Searching all occurences of a pattern using library routines
--
module Benchmarks.Search
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.ByteString.Search as B
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.IO as TL

type Env = (B.ByteString, BL.ByteString, T.Text16, TL.Text16)

initEnv :: FilePath -> IO Env
initEnv fp = do
    b  <- B.readFile fp
    bl <- BL.readFile fp
    t  <- T.readFile fp
    tl <- TL.readFile fp
    return (b, bl, t, tl)

benchmark :: T.Text16 -> Env -> Benchmark
benchmark needleT ~(b, bl, t, tl) =
    bgroup "FileIndices"
        [ bench "ByteString"     $ whnf (byteString needleB)     b
        , bench "LazyByteString" $ whnf (lazyByteString needleB) bl
        , bench "Text16"           $ whnf (text needleT)           t
        , bench "LazyText16"       $ whnf (lazyText16 needleTL)      tl
        ]
  where
    needleB = T.encodeUtf8 needleT
    needleTL = TL.fromChunks [needleT]

byteString :: B.ByteString -> B.ByteString -> Int
byteString needle = length . B.indices needle

lazyByteString :: B.ByteString -> BL.ByteString -> Int
lazyByteString needle = length . BL.indices needle

text :: T.Text16 -> T.Text16 -> Int
text = T.count

lazyText16 :: TL.Text16 -> TL.Text16 -> Int
lazyText16 needle = fromIntegral . TL.count needle
