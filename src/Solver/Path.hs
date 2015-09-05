{- |
Module      : Path
Description : Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

'Path' related functions.
-}
module Solver.Path (pathToString, pathToCells, initPaths) where

import Solver.Types (Cell (..), Grid, Path)
import Solver.Helper (showLetter, around)

import Data.Array

{- |
Convert a 'Path' in a 'Grid' to a 'String'.
-}
pathToString :: Grid -> Path -> String
pathToString grid = map (showLetter . letter . (grid !))

{- |
Convert a 'Path' in a 'Grid' to a 'List' of 'Cell's.
-}
pathToCells :: Grid -> Path -> [Cell]
pathToCells = map . (!)

{- |
Initial 'Path's for the 'walk' function are a 'List' of 'Position' couples
giving all the possible combinations on a 4×4 'Grid'.
-}
initPaths :: [Path]
initPaths = [[(x, y), p] | x <- [0 .. 3], y <- [0 .. 3], p <- around (x, y)]
