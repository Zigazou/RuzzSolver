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
    , Found(None, Partial, Complete, Final)
    , getDictionary
    , lookUp
    ) where

import Data.Tree (Forest, Tree(Node), rootLabel, subForest)
import Control.Applicative ((<$>))

{-|
A TChar contains one character and a boolean indicating if a word can be
created when stopping at this character in the dictionary tree.
-}
data TChar = TChar Char Bool deriving (Eq, Show)

{-|
The dictionary is contained in a 'Tree'. It starts with a 'Forest' which
contains 'Tree's.
-}
type DictForest = Forest TChar
type DictTree = Tree TChar

{-|
A 'Word' is simply a 'String' which can be searched in a dictionary
-}
type Word = String

{-|
The 'Found' type tells how a lookup in a dictionary went :

  * None: the lookup found no word
  * Partial: the lookup found the start of a word, which itself is not a word
  * Complete: the lookup found a word which is also the start of another word
  * Final: the lookup found a word which is not the start of another word
-}
data Found = None | Partial | Complete | Final deriving (Show, Eq, Ord)

{-|
A 'Dictionary' is in fact a 'Forest' of 'TChar'
-}
type Dictionary = DictForest

{-|
'~=' is an equal operator which doesn’t take the boolean of a TChar
into account.
-}
(~=) :: TChar -> TChar -> Bool
(~=) (TChar a _) (TChar b _) = a == b

{-|
Find a 'Tree' in the 'Forest' of 'TChar' for which the root label contains
a 'TChar' with a specific 'Char', but without any interest for the boolean
value.
-}
findChar :: DictForest -> TChar -> Maybe DictTree
findChar [] _ = Nothing
findChar (n:ns) tc
    | rootLabel n ~= tc = Just n
    | otherwise         = findChar ns tc

{-|
Split a 'Forest' of 'TChar' at a specific 'TChar', returning a tuple of
three elements:

  * a 'Forest' of the 'Tree's before the requested element,
  * the requested element,
  * a 'Forest' of the 'Tree's after the requested element.

These three elements contains exactly the same elements than the source.
-}
splitAtD :: DictForest -> TChar -> (DictForest, Maybe DictTree, DictForest)
splitAtD [] _ = ([], Nothing, [])
splitAtD (tree:remains) tc = splitAtD' ([], Just tree, remains)
    where
        splitAtD' :: (DictForest, Maybe DictTree, DictForest)
                  -> (DictForest, Maybe DictTree, DictForest)
        splitAtD' (start, Just item, next:end)
            | rootLabel item ~= tc = (start, Just item, next:end)
            | otherwise            = splitAtD' (item:start, Just next, end)
        splitAtD' (start, Just item, [])
            | rootLabel item ~= tc = (start, Just item, [])
            | otherwise            = (item:start, Nothing, [])

{-|
Create a 'Tree' of 'TChar' from a 'String'
-}
createTree :: String -> DictTree
createTree (c:[]) =
    Node { rootLabel = TChar c True, subForest = [] }
createTree (c:cs) =
    Node { rootLabel = TChar c False, subForest = [createTree cs] }
createTree [] =
    error "createTree cannot be called with empty string"

{-|
Merge a 'Tree' into a 'Forest'
-}
(+++) :: DictForest -> DictTree -> DictForest
(+++) dict tree = case splitAtD dict (rootLabel tree) of
    (s, Just t , e) -> t { subForest=(subForest t) +++ (head $ subForest tree) }
                       : s ++ e
    (_, Nothing, _) -> tree:dict

{-|
Search a 'Word' in a 'Dictionary'. It returns a 'Found' value:

  * None: the word is not in the dictionary
  * Partial: the lookup found the start of a word, which itself is not a word
  * Complete: the lookup found a word which is also the start of another word
  * Final: the lookup found a word which is not the start of another word
-}
lookUp :: Dictionary -> Word -> Found
lookUp [] _  = None
lookUp _  [] = None
lookUp dict (first:remains) = found
    where
        treeM = findChar dict (TChar first False)
        found = case (remains, treeM) of
            ([], Just t)  -> case (subForest t, rootLabel t) of 
                ([], _            ) -> Final
                (_ , TChar _ True ) -> Complete
                (_ , TChar _ False) -> Partial
            (_ , Just t)  -> lookUp (subForest t) remains
            (_ , Nothing) -> None

{-|
Loads a specific dictionary (ASCII format, one word per line) and returns a
dictionary optimised for future searches.
-}
getDictionary :: IO Dictionary
getDictionary = do
    content <- readFile "dictionary/ruzzdictionary.txt"

    return $ foldl (+++) [] (createTree <$> lines content) 

