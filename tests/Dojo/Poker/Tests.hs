{-# LANGUAGE TemplateHaskell #-}

module Dojo.Poker.Tests where

import Test.Framework                       (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.TH                    (testGroupGenerator)
import Test.HUnit                           (Assertion, (@?=))
import Test.QuickCheck                      (Arbitrary, Gen, arbitrary, elements, vector)

import Data.List  (sort)
import Data.Maybe (fromJust)

import Dojo.Poker (rank, toHand, Rank(..), Hand(..), Value(..), Card(..), Suite(..))


tests :: Test
tests = $(testGroupGenerator)

--
-- test cases according to the kata description
--

case_example1 :: Assertion
case_example1 = example "2C 3H 4S 8C AH" "2H 3D 5S 9C KD" $ HighCard [Ace, Number 8, Number 4, Number 3, Number 2]

case_example2 :: Assertion
case_example2 = example "2H 4S 4C 2D 4H" "2S 8S AS QS 3S" $ FullHouse (Number 4)

case_example3 :: Assertion
case_example3 = example "2H 3D 5S 9C KD" "2C 3H 4S 8C KH" $ HighCard [King, Number 9, Number 5, Number 3, Number 2]

case_example4 :: Assertion
case_example4 = do
    black == white @?= True
    rank black @?= (Just $ HighCard [King, Number 9, Number 5, Number 3, Number 2])
  where
    black = fromJust $ toHand "2H 3D 5S 9C KD"
    white = fromJust $ toHand "2D 3H 5C 9S KH"

--
-- properties
--

prop_sorted :: Hand -> Bool
prop_sorted h = rank h == rank (Hand $ sort $ hCards h)

--
-- helper stuff
--

example :: String -> String -> Rank -> Assertion
example w l r = do
    w' > l' @?= True
    rank w' @?= (Just $ r)
  where
    w' = fromJust $ toHand w
    l' = fromJust $ toHand l

deck :: [Card]
deck = let vs = [Number n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
        in [Card v s | v <- vs, s <- [Clubs, Diamonds, Hearts, Spades]]

instance Arbitrary Card where
    arbitrary = elements deck

instance Arbitrary Hand where
    arbitrary = (vector 5 :: Gen [Card]) >>= (\cs -> return $ Hand cs)