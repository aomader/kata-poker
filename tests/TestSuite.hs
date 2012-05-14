module Main where

import Test.Framework (defaultMain)

import qualified Dojo.Poker.Tests

main :: IO ()
main = defaultMain [ Dojo.Poker.Tests.tests ]