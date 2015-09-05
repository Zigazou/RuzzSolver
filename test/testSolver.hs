{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : testSolver
Description : Tests for Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Tests for Solver
-}
module Main where

import Control.Monad (unless, liftM)
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Solver (translate, around, readGrid, walk)
import Dictionary

-- Tests for translate function
prop_translateEmpty1 :: (Eq a, Eq b) => [a] -> a -> b -> Bool
prop_translateEmpty1 tos def value = translate [] tos def value == def

prop_translateEmpty2 :: (Eq a, Eq b) => [b] -> a -> b -> Bool
prop_translateEmpty2 froms def value = translate froms [] def value == def

prop_translateIdem :: (Eq a) => [a] -> a -> a -> Bool
prop_translateIdem froms def value = translate froms froms def value == value'
    where value' = if value `elem` froms then value else def

-- Tests for around function
newtype TestAround = TestAround Int deriving Show
instance Arbitrary TestAround where
    arbitrary = TestAround <$> choose (0, 3)

prop_aroundLength :: TestAround -> TestAround -> Bool
prop_aroundLength (TestAround x) (TestAround y) = l >= 3 && l <= 8
    where l = length $ around (x, y)

-- Tests for walk function
testWalk :: String -> Int -> Property
testWalk problemFilePath nbResult =
    once $ monadicIO $ do
        results <- run $ do
            grid <- liftM (readGrid . lines) (readFile problemFilePath)
            dictionary <- getDictionary "dictionary/ruzzdictionary.txt"
            let solutions = walk grid dictionary
            return solutions
        assert $ length results == nbResult

prop_problem00 :: Property
prop_problem00 = testWalk "test/problem-00.txt" 453

prop_problem01 :: Property
prop_problem01 = testWalk "test/problem-01.txt" 480

prop_problem02 :: Property
prop_problem02 = testWalk "test/problem-02.txt" 454

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
