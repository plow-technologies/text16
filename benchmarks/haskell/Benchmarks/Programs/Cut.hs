-- | Cut into a file, selecting certain columns (e.g. columns 10 to 40)
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into lines
--
-- * Taking a number of characters from the lines
--
-- * Joining the lines
--
-- * Writing back to a handle
--
module Benchmarks.Programs.Cut
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text16 as T
import qualified Data.Text16.Encoding as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.Encoding as TL
import qualified Data.Text16.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Int -> Int -> Benchmark
benchmark p sink from to = bgroup "Cut"
    [ bench' "Text16" text
    , bench' "LazyText16" lazyText16
    , bench' "Text16ByteString" textByteString
    , bench' "LazyText16ByteString" lazyText16ByteString
    ]
  where
    bench' n s = bench n $ whnfIO (s p sink from to)

text :: FilePath -> Handle -> Int -> Int -> IO ()
text fp sink from to = do
    t <- T.readFile fp
    T.hPutStr sink $ cut t
  where
    cut = T.unlines . map (T.take (to - from) . T.drop from) . T.lines

lazyText16 :: FilePath -> Handle -> Int -> Int -> IO ()
lazyText16 fp sink from to = do
    t <- TL.readFile fp
    TL.hPutStr sink $ cut t
  where
    cut = TL.unlines . map (TL.take (to' - from') . TL.drop from') . TL.lines
    from' = fromIntegral from
    to' = fromIntegral to

textByteString :: FilePath -> Handle -> Int -> Int -> IO ()
textByteString fp sink from to = do
    t <- T.decodeUtf8 `fmap` B.readFile fp
    B.hPutStr sink $ T.encodeUtf8 $ cut t
  where
    cut = T.unlines . map (T.take (to - from) . T.drop from) . T.lines

lazyText16ByteString :: FilePath -> Handle -> Int -> Int -> IO ()
lazyText16ByteString fp sink from to = do
    t <- TL.decodeUtf8 `fmap` BL.readFile fp
    BL.hPutStr sink $ TL.encodeUtf8 $ cut t
  where
    cut = TL.unlines . map (TL.take (to' - from') . TL.drop from') . TL.lines
    from' = fromIntegral from
    to' = fromIntegral to
