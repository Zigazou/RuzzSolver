{- |
Module      : Helper
Description : Helper functions
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Helper functions
-}
module Solver.Helper (translate, around, showLetter, readLetter) where

import Solver.Types (Position, Letter (..))
    
{- |
'Translate' is close to the 'tr' Unix command line tool.
-}
translate :: (Eq a, Eq b) => [a] -> [b] -> b -> a -> b
translate (from : froms) (to : tos) def value
    | from == value = to
    | otherwise     = translate froms tos def value
translate _ _ def _ = def

{- |
Gets the 'Position's around a particular 'Position' in a 4×4 'Grid'.
-}
around :: Position -> [Position]
around (x, y) =
    [(x', y') | x' <- [x - 1 .. x + 1] -- Look for left to right
              , x' >= 0 && x' <= 3     -- Coordinates must be on the grid
              , y' <- [y - 1 .. y + 1] -- Look for top to bottom
              , y' >= 0 && y' <= 3     -- Coordinates must be on the grid
              , (x', y') /= (x, y)     -- it cannot be the position itself
    ]

{- |
Converts a 'Letter' to a 'Char'
-}
showLetter :: Letter -> Char
showLetter = translate [La .. Lz] ['A' .. 'Z'] (error "Unknown error")

{- |
Converts a 'Char' to a 'Letter'
-}
readLetter :: Char -> Letter
readLetter = translate ['A' .. 'Z'] [La .. Lz] (error "Invalid char")
