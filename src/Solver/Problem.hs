{- |
Module      : Solver
Description : Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The Solver can take a 'Grid' and a 'Dictionary' and searches for all 'DictWord's
that can be constructed in the 'Grid' that appears in the 'Dictionary'.
-}
module Solver.Problem (readCell, readRow, readGrid) where

import Solver.Types (Multiplier (..), Cell (..), Grid, Problem)
import Solver.Helper (translate, readLetter)
import Data.Array
    
{- |
Read the 'Multiplier' letter in a 'Problem'.
-}
readMultiplier :: Char -> Multiplier
readMultiplier = translate
     "dtDT"
     [MultiplyLetter 2, MultiplyLetter 3, MultiplyWord 2, MultiplyWord 3]
     (MultiplyLetter 1)

{- |
Read a 'Cell' in a 'Problem'. The function eats 2 characters from a 'String'
and returns a tuple composed from the created 'Cell' and the remaining
characters.
-}
readCell :: String -> (Cell, String)
readCell (l : m : remains) =
    ( Cell { letter = readLetter l, multiplier = readMultiplier m }, remains )
readCell _ = error "Invalid input given to coupleToCell"

{- |
Read a complete row from a 'Problem' and returns a list of 'Cell's.
-}
readRow :: String -> [Cell]
readRow []     = []
readRow string = oneCell : readRow remains
    where (oneCell, remains) = readCell string

{- |
Generate a 'Grid' of 'Cell's from a 'Problem'
-}
readGrid :: Problem -> Grid
readGrid = listArray ((0, 0), (3, 3)) . readRow . concat
