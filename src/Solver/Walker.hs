{- |
Module      : Walker
Description : The solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The Solver can take a 'Grid' and a 'Dictionary' and searches for all 'DictWord's
that can be constructed in the 'Grid' that appears in the 'Dictionary'.
-}
module Solver.Walker (walk) where

import Solver.Types (Grid, Path)
import Solver.Helper (around)
import Solver.Path (initPaths, pathToString)

import Data.List ((\\))
import Dictionary

{- |
Given a 'Grid' and a 'Dictionary', finds all the words from the 'Dictionary'
that can be drawn in the 'Grid'. It returns them in the form of 'List' of
'Path's, allowing afterwards an easy conversion to a 'DictWord' or a 'Score'.

'DictWord's can be constructed from the letters of sequentially adjacent 'Cell's,
where "adjacent" 'Cell's are those horizontally, vertically, and diagonally
neighboring. 'Word's must be at least two letters long, may include singular
and plural (or other derived forms) separately, but may not use the same 'Cell'
more than once per word.
-}
walk :: Grid -> Dictionary -> [Path]
walk grid dictionary = concatMap walker initPaths
    where walker :: Path -> [Path]
          walker path = case lookUp dictionary (pathToString grid path) of
                            None     -> []
                            Partial  -> nextPaths
                            Complete -> path : nextPaths
                            Final    -> [path]
              where arounds   = around (last path) \\ path
                    nextPaths = concat
                              $ (walker . (path ++) . (: [])) <$> arounds
