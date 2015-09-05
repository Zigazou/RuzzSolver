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
module Solver.Score (letterScores, lengthScores, evalScore) where

import Solver.Types ( Letter (..), Multiplier (..), Cell (..)
                    , Score, Grid, LetterScores, Path
                    )

import Solver.Path (pathToCells)

import Data.Array

{- |
This function returns an 'Array' containing the 'Score's for each letter. The
'Array' is indexed by the 'Letter'.
-}
letterScores :: LetterScores
letterScores = array
                (La, Lz)
                (concat
                  [ [ (l,  1) | l <- [La, Le, Li, Ll, Ln, Lo, Lr, Ls, Lt, Lu] ]
                  , [ (l,  2) | l <- [Ld, Lg, Lm] ]
                  , [ (l,  3) | l <- [Lb, Lc, Lp] ]
                  , [ (l,  4) | l <- [Lf, Lh, Lv] ]
                  , [ (l,  8) | l <- [Lj, Lq] ]
                  , [ (l, 10) | l <- [Lk, Lw, Lx, Ly, Lz] ]
                  ]
                )

{- |
This function returns an additional length 'Score' based on the length of a
word.
-}
lengthScores :: Int -> Score
lengthScores 15 = 25
lengthScores 14 = 25
lengthScores 13 = 25
lengthScores 12 = 25
lengthScores 11 = 25
lengthScores 10 = 25
lengthScores 9  = 25
lengthScores 8  = 20
lengthScores 7  = 15
lengthScores 6  = 10
lengthScores 5  = 5
lengthScores _  = 0

{- |
Calculates an intermediate score in the form of a tuple ('Score', 'Int') where
the first is the current score and the second the current word multiplier.
-}
plus :: (Score, Int) -> Cell -> (Score, Int)
plus (s, m) cell = (s + (letterScores ! letter cell) * mletter, m * mword)
    where (mletter, mword) = case multiplier cell of
                                MultiplyLetter m' -> (m', 1)
                                MultiplyWord   m' -> (1, m')

{- |
Calculates the 'Score' from a 'Path' in a 'Grid'.
-}
evalScore :: Grid -> Path -> Score
evalScore grid path = score * mult + (lengthScores . length) path
    where (score, mult) = (foldl plus (0, 1) . pathToCells grid) path
