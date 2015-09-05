{- |
Module      : Types
Description : Types used by the solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Solver.Types
    ( Letter (..), Multiplier (..), Cell (..), Score, Position, Grid
    , LetterScores, Problem, Path
    ) where

import Data.Array

{- |
The solver use only the alphabetic letters, independently of their case and
without any accent.
-}
data Letter = La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm
            | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv | Lw | Lx | Ly | Lz
            deriving (Eq, Show, Enum, Ord, Bounded, Ix)

{- |
A 'Cell' may have a multiplier bonus which is applied to the whole word or to
the letter.
-}
data Multiplier = MultiplyLetter Int
                | MultiplyWord Int
                deriving (Show, Eq)

{- |
A 'Grid' is composed of 'Cell's. A 'Cell' holds a 'Letter' and a 'Multiplier'.
-}
data Cell = Cell { letter     :: Letter
                 , multiplier :: Multiplier
                 } deriving (Show)

{- |
A 'Score' is calculated using an 'Int'
-}
type Score        = Int

{- |
A 'Grid' is a 2-dimensional array. A tuple of two 'Int' is used to address each
'Cell'.
-}
type Position     = (Int, Int)

{- |
A 'Grid' is a 2-dimensional array containing 'Cell's indexed by their
'Position'.
-}
type Grid         = Array Position Cell

{- |
A 'Letter' has a value determined by its frequency in the french language. The
values are common to the Scrabble game in french.
-}
type LetterScores = Array Letter Score

{- |
As a 'Grid' is not easily interfaced with human, a 'Problem' is used.
A 'Problem' is simply a 'List' of 'String'.

Here is an example:

    [ "UdA U Tt"
    , "S UDNTCt"
    , "T A R S "
    , "E I TDC "
    ]

'Cell's are represented by two consecutive characters. The first is the
uppercase letter. The second indicates the multiplier bonus for this particular
'Cell': d for double letter, t for triple letter, D for double word and T for
triple word. Everything is considered a multiplier of 1.
-}
type Problem      = [String]

{- |
A 'Path' is a 'List' of 'Position's used to keep track of a walk through the
'Grid'. It can then be converted to a 'DictWord' or used to compute the score.
-}
type Path         = [Position]
