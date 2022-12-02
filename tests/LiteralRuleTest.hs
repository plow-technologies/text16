{-# LANGUAGE OverloadedStrings #-}

module LiteralRuleTest where

import Data.Text16 (Text16)

-- This should produce 8 firings of the "TEXT literal" rule
strings :: [Text16]
strings = [ "abstime", "aclitem", "bit", "bool", "box", "bpchar", "bytea", "char" ]

-- This should produce 7 firings of the "TEXT literal UTF8" rule
utf8Strings :: [Text16]
utf8Strings = [ "\0abstime", "\0aclitem", "\xfefe bit", "\0bool", "\0box", "\0bpchar", "\0bytea" ]

-- This should produce 4 firings of the "TEXT empty literal" rule
empties :: [Text16]
empties = [ "", "", "", "" ]

-- This should produce 5 firings of the "TEXT empty literal" rule
--singletons :: [Text16]
--singletons = [ "a", "b", "c", "d", "e" ]
