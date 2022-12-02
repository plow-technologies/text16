module Arsec
    (
      Comment
    , comment
    , semi
    , showC
    , unichar
    , unichars
    , module Control.Applicative
    , module Control.Monad
    , module Data.Char
    , module Text16.ParserCombinators.Parsec.Char
    , module Text16.ParserCombinators.Parsec.Combinator
    , module Text16.ParserCombinators.Parsec.Error
    , module Text16.ParserCombinators.Parsec.Prim
    ) where

import Control.Monad
import Control.Applicative
import Data.Char
import Numeric
import Text16.ParserCombinators.Parsec.Char hiding (lower, upper)
import Text16.ParserCombinators.Parsec.Combinator hiding (optional)
import Text16.ParserCombinators.Parsec.Error
import Text16.ParserCombinators.Parsec.Prim hiding ((<|>), many)

type Comment = String

unichar :: Parser Char
unichar = chr . fst . head . readHex <$> many1 hexDigit

unichars :: Parser [Char]
unichars = manyTill (unichar <* spaces) semi

semi :: Parser ()
semi = char ';' *> spaces *> pure ()

comment :: Parser Comment
comment = (char '#' *> manyTill anyToken (char '\n')) <|> string "\n"

showC :: Char -> String
showC c = "'\\x" ++ d ++ "'"
    where h = showHex (ord c) ""
          d = replicate (4 - length h) '0' ++ h
