-- | Read numbers from a file with a just a number on each line, find the
-- minimum of those numbers. The file contains different kinds of numbers:
--
-- * Decimals
--
-- * Hexadecimals
--
-- * Floating point numbers
--
-- * Floating point numbers in scientific notation
--
-- The different benchmarks will only take into account the values they can
-- parse.
--
-- Tested in this benchmark:
--
-- * Lexing/parsing of different numerical types
--
module Benchmarks.ReadNumbers
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import Data.List (foldl')
import Numeric (readDec, readFloat, readHex)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lex.Fractional as B
import qualified Data.Text16 as T
import qualified Data.Text16.IO as T
import qualified Data.Text16.Lazy as TL
import qualified Data.Text16.Lazy.IO as TL
import qualified Data.Text16.Lazy.Read as TL
import qualified Data.Text16.Read as T

type Env = ([String], [T.Text16], [TL.Text16], [B.ByteString], [BL.ByteString])

initEnv :: FilePath -> IO Env
initEnv fp = do
    -- Read all files into lines: string, text, lazy text, bytestring, lazy
    -- bytestring
    s <- lines `fmap` readFile fp
    t <- T.lines `fmap` T.readFile fp
    tl <- TL.lines `fmap` TL.readFile fp
    b <- B.lines `fmap` B.readFile fp
    bl <- BL.lines `fmap` BL.readFile fp
    return (s, t, tl, b, bl)

benchmark :: Env -> Benchmark
benchmark ~(s, t, tl, b, bl) =
    bgroup "ReadNumbers"
        [ bench "DecimalString"     $ whnf (int . string readDec) s
        , bench "HexadecimalString" $ whnf (int . string readHex) s
        , bench "DoubleString"      $ whnf (double . string readFloat) s

        , bench "DecimalText16"     $ whnf (int . text (T.signed T.decimal)) t
        , bench "HexadecimalText16" $ whnf (int . text (T.signed T.hexadecimal)) t
        , bench "DoubleText16"      $ whnf (double . text T.double) t
        , bench "RationalText16"    $ whnf (double . text T.rational) t

        , bench "DecimalLazyText16" $
            whnf (int . text (TL.signed TL.decimal)) tl
        , bench "HexadecimalLazyText16" $
            whnf (int . text (TL.signed TL.hexadecimal)) tl
        , bench "DoubleLazyText16" $
            whnf (double . text TL.double) tl
        , bench "RationalLazyText16" $
            whnf (double . text TL.rational) tl

        , bench "DecimalByteString" $ whnf (int . byteString B.readInt) b
        , bench "DoubleByteString"  $ whnf (double . byteString B.readDecimal) b

        , bench "DecimalLazyByteString" $
            whnf (int . byteString BL.readInt) bl
        ]
  where
    -- Used for fixing types
    int :: Int -> Int
    int = id
    double :: Double -> Double
    double = id

string :: (Ord a, Num a) => (t -> [(a, t)]) -> [t] -> a
string reader = foldl' go 1000000
  where
    go z t = case reader t of [(n, _)] -> min n z
                              _        -> z

text :: (Ord a, Num a) => (t -> Either String (a,t)) -> [t] -> a
text reader = foldl' go 1000000
  where
    go z t = case reader t of Left _       -> z
                              Right (n, _) -> min n z

byteString :: (Ord a, Num a) => (t -> Maybe (a,t)) -> [t] -> a
byteString reader = foldl' go 1000000
  where
    go z t = case reader t of Nothing     -> z
                              Just (n, _) -> min n z
