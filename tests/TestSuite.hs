{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestSuite where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

suite :: Spec
suite = do
    describe "Hashing a bytestring" $ do
        it "works on known values" $ do undefined
