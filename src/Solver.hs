{-|
Module      : Solver
Description : Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The Solver can take a 'Grid' and a 'Dictionary' and searches for all 'Word's
that can be constructed in the 'Grid' that appears in the 'Dictionary'.
-}
module Solver
    ( -- * Types used by the 'Solver'
      Letter(..), Multiplier(..), Cell(letter, multiplier), Score
    , Position, Grid, LetterScores, Problem, Path

      -- * Helper functions
    , translate, around
    
      -- * 'Score' functions
    , letterScores, cellScore, evalScore', evalScore

      -- * 'Problem' functions
    , readCell, readRow, readGrid

      -- * 'Path' functions
    , pathToString, pathToCells

      -- * The Solver
    , walk
    ) where

import Data.Array
import Data.List ((\\))
import Dictionary

{-|
The solver use only the alphabetic letters, independently of their case and
without any accent.
-}
data Letter = La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm
            | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv | Lw | Lx | Ly | Lz
            deriving (Eq, Show, Enum, Ord, Bounded, Ix)

{-|
A 'Cell' may have a multiplier bonus which is applied to the whole word or to
the letter.
-}
data Multiplier = MultiplyLetter Int
                | MultiplyWord Int
                deriving (Show, Eq)

{-|
A 'Grid' is composed of 'Cell's. A 'Cell' holds a 'Letter' and a 'Multiplier'.
-}
data Cell = Cell { letter     :: Letter
                 , multiplier :: Multiplier
                 } deriving (Show)

{-|
A 'Score' is calculated using an 'Int'
-}
type Score        = Int

{-|
A 'Grid' is a 2-dimensional array. A tuple of two 'Int' is used to address each
'Cell'.
-}
type Position     = (Int, Int)

{-|
A 'Grid' is a 2-dimensional array containing 'Cell's indexed by their
'Position'.
-}
type Grid         = Array Position Cell

{-|
A 'Letter' has a value determined by its frequency in the french language. The
values are common to the Scrabble game in french.
-}
type LetterScores = Array Letter Score

{-|
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

{-|
A 'Path' is a 'List' of 'Position's used to keep track of a walk through the
'Grid'. It can then be converted to a 'Word' or used to compute the score.
-}
type Path         = [Position]

{-|
'Translate' is close to the 'tr' Unix command line tool.
-}
translate :: (Eq a, Eq b) => [a] -> [b] -> b -> a -> b
translate (from:froms) (to:tos) def value
    | from == value = to
    | otherwise     = translate froms tos def value
translate _ _ def _ = def

{-|
Gets the 'Position's around a particular 'Position' in a 4×4 'Grid'.
-}
around :: Position -> [Position]
around (x, y) =
    [(x', y') | x' <- [x-1 .. x+1] -- Look for left to right
              , y' <- [y-1 .. y+1] -- Look for top to bottom
              , (x',y') /= (x,y)   -- it cannot be the position itself
              , x' >= 0 && x' <= 3 -- Coordinates must be on the grid
              , y' >= 0 && y' <= 3 -- Coordinates must be on the grid
    ]

{-|
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

{-|
Calculate the 'Score' of a single 'Cell'
-}
cellScore :: Cell -> Score
cellScore cell = (letterScores ! letter cell) * mult
    where mult = case multiplier cell of MultiplyLetter m -> m
                                         MultiplyWord   _ -> 1

{-|
Calculates the 'Score' from a list of 'Cell's.
-}
evalScore' :: Score  -- ^ Initial 'Score'
           -> Int    -- ^ Current word multiplier
           -> [Cell] -- ^ List of 'Cell's
           -> Score  -- ^ Calculated 'Score'
evalScore' score mult [] = score * mult
evalScore' score mult (cell:remains) =
    evalScore' (score + cellScore cell) mult' remains
    where mult' = case multiplier cell of
                    MultiplyLetter _ -> mult
                    MultiplyWord   m -> mult * m

{-|
Calculates the 'Score' from a 'Path' in a 'Grid'.
-}
evalScore :: Grid -> Path -> Score
evalScore grid = (evalScore' 0 1) . pathToCells grid

{-|
Converts a 'Letter' to a 'Char'
-}
showLetter :: Letter -> Char
showLetter = translate [La .. Lz] ['A' .. 'Z'] (error "Unknown error")

{-|
Converts a 'Char' to a 'Letter'
-}
readLetter :: Char -> Letter
readLetter = translate ['A' .. 'Z'] [La .. Lz] (error "Invalid char")

{-|
Read the 'Multiplier' letter in a 'Problem'.
-}
readMultiplier :: Char -> Multiplier
readMultiplier = translate
     ['d', 't', 'D', 'T']
     [MultiplyLetter 2, MultiplyLetter 3, MultiplyWord 2, MultiplyWord 3]
     (MultiplyLetter 1)

{-|
Read a 'Cell' in a 'Problem'. The function eats 2 characters from a 'String'
and returns a tuple composed from the created 'Cell' and the remaining
characters.
-}
readCell :: String -> (Cell, String)
readCell (oneLetter:oneMultiplier:remains) =
    ( Cell { letter     = readLetter oneLetter
           , multiplier = readMultiplier oneMultiplier
           }
    , remains
    )
readCell _ = error "Invalid input given to coupleToCell"

{-|
Read a complete row from a 'Problem' and returns a list of 'Cell's.
-}
readRow :: String -> [Cell]
readRow []     = []
readRow string = oneCell:readRow remains
    where (oneCell, remains) = readCell string

{-|
Generate a 'Grid' of 'Cell's from a 'Problem'
-}
readGrid :: Problem -> Grid
readGrid = listArray ((0, 0), (3, 3)) . readRow . concat

{-|
Convert a 'Path' in a 'Grid' to a 'String'.
-}
pathToString :: Grid -> Path -> String
pathToString grid = map (showLetter . letter . (grid !))

{-|
Convert a 'Path' in a 'Grid' to a 'List' of 'Cell's.
-}
pathToCells :: Grid -> Path -> [Cell]
--pathToCells grid = map (grid !)
pathToCells = map . (!)

{-|
Given a 'Grid' and a 'Dictionary', finds all the words from the 'Dictionary'
that can be drawn in the 'Grid'. It returns them in the form of 'List' of
'Path's, allowing afterwards an easy conversion to a 'Word' or a 'Score'.

'Word's can be constructed from the letters of sequentially adjacent 'Cell's,
where "adjacent" 'Cell's are those horizontally, vertically, and diagonally
neighboring. 'Word's must be at least two letters long, may include singular
and plural (or other derived forms) separately, but may not use the same 'Cell'
more than once per word.
-}
walk :: Grid -> Dictionary -> [Path]
walk grid dictionary = concat $ map walker initialPaths
    where initialPaths :: [Path]
          initialPaths = concat $ map (\p1 -> map (\p2 -> p1:p2:[]) (around p1)) [(x, y) | x <- [0 .. 3], y <- [0 .. 3]]
          walker :: Path -> [Path]
          walker path = case lookUp dictionary (pathToString grid path) of
                            None     -> []
                            Partial  -> concat nextPaths
                            Complete -> path: concat nextPaths
                            Final    -> path: []
              where position  = last path
                    arounds   = around position \\ path
                    nextPaths = map (walker . (path ++) . (: [])) arounds

