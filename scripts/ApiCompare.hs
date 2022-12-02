-- This script compares the strict and lazy Text16 APIs to ensure that
-- they're reasonably in sync.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text16 as T
import System.Process

main = do
  let tidy pkg = (S.fromList . filter (T.isInfixOf "::") . T.lines .
                  T.replace "GHC.Int.Int64" "Int" .
                  T.replace "\n " "" .
                  T.replace (T.append (T.pack pkg) ".") "" . T.pack) `fmap`
                 readProcess "ghci" [] (":browse " ++ pkg)
  let diff a b = mapM_ (putStrLn . ("  "++) . T.unpack) . S.toList $
                 S.difference a b
  text <- tidy "Data.Text16"
  lazy <- tidy "Data.Text16.Lazy"
  list <- tidy "Data.List"
  putStrLn "Text16 \\ List:"
  diff text list
  putStrLn ""
  putStrLn "Text16 \\ Lazy:"
  diff text lazy
  putStrLn ""
  putStrLn "Lazy \\ Text16:"
  diff lazy text
