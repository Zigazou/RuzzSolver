{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : testDictionary
Description : Tests for Dictionary
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Tests for Dictionary
-}
module Main where

import           Control.Monad       (unless)
import           System.Exit         (exitFailure)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import           Control.Applicative ((<$>))
import           Data.List (nub, (\\), find)
import           Data.Maybe (isNothing)

import           Dictionary

-- Tests for checkWord function
newtype TestWord = TestWord String deriving Show
instance Arbitrary TestWord where
    arbitrary = TestWord <$> (listOf $ elements ['A' .. 'Z'])

prop_checkWordComplete :: TestWord -> Bool
prop_checkWordComplete (TestWord word) =
    checkWord word word == Complete

prop_checkWordPartial :: TestWord -> Bool
prop_checkWordPartial (TestWord word) =
    checkWord word (word ++ "A") == Partial

prop_checkWordNone :: TestWord -> Bool
prop_checkWordNone (TestWord []  ) = True
prop_checkWordNone (TestWord word) = checkWord (word) ('x':word) == None

-- Tests for getDictionary

prop_DictionaryLength :: Property
prop_DictionaryLength =
    once $ monadicIO $ run getDictionary >>= assert . (== 386264) . length

prop_validDictionary :: Property
prop_validDictionary =
    once $ monadicIO $ run getDictionary >>= assert . validDictionary
    where validDictionary = isNothing . find invalidWord
          invalidWord word = [] /= nub word \\ ['A' .. 'Z']

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
