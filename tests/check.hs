{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec (hspec)
import TestSuite (suite)

main :: IO ()
main = do
    hspec suite
