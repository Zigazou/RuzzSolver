{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Dictionary
Description : French dictionary functions for use with Ruzzle Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

French dictionary functions for use with Ruzzle Solver. The 'Dictionary' uses
a 'Tree' structure to optimize lookups and is able to tell if a 'DictWord' is
complete (but is also the start of other words), partial or final (word is
valid but is not the start of other words).
-}
module Dictionary
    ( DictWord
    , Dictionary
    , Found (None, Partial, Complete, Final)
    , getDictionary
    , getDictionaryQ
    , lookUp
    ) where

import Data.Tree (Forest, Tree (Node), rootLabel, subForest)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

{- |
A TChar contains one character and a boolean indicating if a word can be
created when stopping at this character in the dictionary tree.
-}
data TChar = TChar Char Bool deriving (Eq, Show)

{- |
The dictionary is contained in a 'Tree'. It starts with a 'Forest' which
contains 'Tree's.
-}
type DictForest = Forest TChar
type DictTree = Tree TChar

{- |
A 'DictWord' is simply a 'String' which can be searched in a dictionary
-}
type DictWord = String

{- |
The 'Found' type tells how a lookup in a dictionary went :
-}
data Found = None     -- ^ the lookup found no word
           | Partial  {- ^ the lookup found the start of a word, which itself
                      is not a word -}
           | Complete {- ^ the lookup found a word which is also the start of
                      another word -}
           | Final    {- ^ the lookup found a word which is not the start of
                      another word -}
           deriving (Show, Eq, Ord)

{- |
A 'Dictionary' is in fact a 'Forest' of 'TChar'
-}
type Dictionary = DictForest

$(deriveLift ''TChar)
$(deriveLift ''Tree)

{- |
'~=' is an equal operator which doesn’t take the boolean of a TChar
into account.
-}
(~=) :: TChar -> TChar -> Bool
(~=) (TChar a _) (TChar b _) = a == b

{- |
Find a 'Tree' in the 'Forest' of 'TChar' for which the root label contains
a 'TChar' with a specific 'Char', but without any interest for the boolean
value.
-}
findChar :: DictForest -> TChar -> Maybe DictTree
findChar [] _ = Nothing
findChar (n : ns) tc
    | rootLabel n ~= tc = Just n
    | otherwise         = findChar ns tc

{- |
Split a 'Forest' of 'TChar' at a specific 'TChar', returning a tuple of
three elements:

  * a 'Forest' of the 'Tree's before the requested element,
  * the requested element,
  * a 'Forest' of the 'Tree's after the requested element.

These three elements contains exactly the same elements than the source.
-}
splitAtD :: DictForest -> TChar -> (DictForest, Maybe DictTree, DictForest)
splitAtD (tree : remains) tc = splitAtD' ([], tree, remains)
    where
        splitAtD' :: (DictForest, DictTree, DictForest)
                  -> (DictForest, Maybe DictTree, DictForest)
        splitAtD' (start, item, next : end)
            | rootLabel item ~= tc = (start, Just item, next : end)
            | otherwise            = splitAtD' (item : start, next, end)
        splitAtD' (start, item, [])
            | rootLabel item ~= tc = (start, Just item, [])
            | otherwise            = (item : start, Nothing, [])
splitAtD [] _ = ([], Nothing, [])

{- |
Create a 'Tree' of 'TChar' from a 'String'
-}
createTree :: String -> DictTree
createTree [c] =
    Node { rootLabel = TChar c True, subForest = [] }
createTree (c : cs) =
    Node { rootLabel = TChar c False, subForest = [createTree cs] }
createTree [] =
    error "createTree cannot be called with empty string"

{- |
Merge a 'Tree' into a 'Forest'
-}
(+++) :: DictForest -> DictTree -> DictForest
(+++) dict tree = case splitAtD dict (rootLabel tree) of
    (s, Just t , e) -> t { subForest = subForest t +++ head (subForest tree) }
                       : s ++ e
    (_, Nothing, _) -> tree : dict

{- |
Search a 'DictWord' in a 'Dictionary'. It returns a 'Found' value.
-}
lookUp :: Dictionary -> DictWord -> Found
lookUp [] _  = None
lookUp _  [] = None
lookUp dict (first : remains) = found
    where
        treeM = findChar dict (TChar first False)
        found = case (remains, treeM) of
            -- The last letter of the word has been found
            ([], Just t)  -> case (subForest t, rootLabel t) of
                -- The word is complete and there is no longer word
                ([], _            ) -> Final
                -- The word is complete but longer words exists
                (_ , TChar _ True ) -> Complete
                -- The word is not complete
                (_ , TChar _ False) -> Partial
            -- The letter has been found, go on with the remaining letters
            (_ , Just t)  -> lookUp (subForest t) remains
            -- The letter has not been found, the word is not in the dictionary
            (_ , Nothing) -> None

{- |
Loads a specific dictionary (ASCII format, one word per line) and returns a
dictionary optimised for future searches.
-}
getDictionary :: String -> IO Dictionary
getDictionary dictionaryFilePath = do
    content <- readFile dictionaryFilePath
    return $ foldl (+++) [] (createTree <$> lines content)

{- |
Embeds a specific dictionary (ASCII format, one word per line) and in the
executable file.
-}
getDictionaryQ :: String -> Q Exp
getDictionaryQ dictionaryFilePath = do
    content <- runIO $ readFile dictionaryFilePath
    lift $ foldl (+++) [] (createTree <$> lines content)
