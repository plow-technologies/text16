{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.Lift
  ( tests
  )
  where

import qualified Data.Text16 as S
import qualified Data.Text16.Lazy as L
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "TH lifting Text16"
  [ testCase "strict" $ assertEqual "strict"
      $(lift ("foo" :: S.Text16))
      ("foo" :: S.Text16)
  , testCase "lazy" $ assertEqual "lazy"
      $(lift ("foo" :: L.Text16))
      ("foo" :: L.Text16)
  ]
