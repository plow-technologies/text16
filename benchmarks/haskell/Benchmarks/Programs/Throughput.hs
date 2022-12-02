-- | This benchmark simply reads and writes a file using the various string
-- libraries. The point of it is that we can make better estimations on how
-- much time the other benchmarks spend doing IO.
--
-- Note that we expect ByteStrings to be a whole lot faster, since they do not
-- do any actual encoding/decoding here, while String and Text16 do have UTF-8
-- encoding/decoding.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Replacing text between HTML tags (<>) with whitespace
--
-- * Writing back to a handle
--
module Benchmarks.Programs.Throughput
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy.Encoding as TL
import qualified Data.Text16.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Benchmark
benchmark fp sink = bgroup "Throughput"
    [ bench "Text16" $ whnfIO $ T.readFile fp >>= T.hPutStr sink
    , bench "LazyText16" $ whnfIO $ TL.readFile fp >>= TL.hPutStr sink
    , bench "Text16ByteString" $ whnfIO $
        B.readFile fp >>= B.hPutStr sink . T.encodeUtf8 .  T.decodeUtf8
    , bench "LazyText16ByteString" $ whnfIO $
        BL.readFile fp >>= BL.hPutStr sink . TL.encodeUtf8 . TL.decodeUtf8
    ]
