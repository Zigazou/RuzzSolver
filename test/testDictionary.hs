{-# LANGUAGE TemplateHaskell #-}
{- |
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

import Control.Monad (unless, liftM)
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Dictionary

getResults :: [String] -> Dictionary -> [Found]
getResults inputs dict = lookUp dict <$> inputs

testFound :: Found -> [String] -> Property
testFound result inputs =
    once $ monadicIO $ do
        results <- run $ liftM (getResults inputs)
                               (getDictionary "dictionary/ruzzdictionary.txt")
        assert $ results == replicate (length results) result

prop_wordNone :: Property
prop_wordNone =
    testFound None
              [ "XXX", "APOFILFJ", "ZERPIOVV", "MLKOPQIGDZ", "QMLD" ]

prop_wordPartial :: Property
prop_wordPartial =
    testFound Partial
              [ "ASSENTI", "LESIONN", "DEZINGU", "RETEINDR", "TITILLASS" ]

prop_wordComplete :: Property
prop_wordComplete =
    testFound Complete
              [ "BISE", "MONTE", "FLASH", "HERBAGE", "ORAL" ]

prop_wordFinal :: Property
prop_wordFinal =
    testFound Final
              [ "PANEZ", "PRONEES", "ANGORS", "BRADES", "EXTRUSIFS" ]

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
