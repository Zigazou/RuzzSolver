{-|
Module      : Dictionary
Description : French dictionary functions for use with Ruzzle Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

French dictionary functions for use with Ruzzle Solver
-}
module Dictionary
    ( Word
    , Dictionary
    , Found(None, Partial, Complete)
    , mayStart
    , checkWord
    , check
    , getDictionary
    ) where

import Data.List (isPrefixOf, find, nub, take)
import Control.Applicative ((<$>))
import Data.Maybe (isJust)

type Word       = String
type Dictionary = [Word]

data Found      = None | Partial | Complete deriving (Show, Eq, Ord)

mayStart :: String -> Dictionary -> Bool
mayStart s dict = isJust $ find (== True) (isPrefixOf s <$> dict)

{-
getPrefixes :: Int -> Dictionary -> [String]
getPrefixes len = nub (take len
-}

checkWord :: String -> Word -> Found
checkWord [] [] = Complete
checkWord [] _  = Partial
checkWord (x:xs) (w:ws)
    | x == w    = checkWord xs ws
    | otherwise = None
checkWord _ [] = None

check :: String -> Dictionary -> Found
check word dictionary = check' dictionary None
    where check' :: Dictionary -> Found -> Found
          check' []                 found = found
          check' (wordDict:remains) found =
                case checkWord word wordDict of
                    None     -> check' remains (if found == Partial then Partial else None)
                    Partial  -> check' remains Partial
                    Complete -> Complete

getDictionary :: IO Dictionary
getDictionary = do
    content <- readFile "dictionary/ruzzdictionary.txt"
    return $ lines content

