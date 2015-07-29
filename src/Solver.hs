{-|
Module      : Solver
Description : Solver
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Solver
-}
module Solver
    ( Letter(..)
    , Multiplier(..)
    , Cell(letter, multiplier, used)
    , Score
    , Position
    , Grid
    , LetterScores
    , Problem
    , Path
    , pathToString
    , readGrid
    , walk
    ) where

import Data.Array
import Data.List ((\\))
import Control.Applicative ((<$>))
import Dictionary

data Letter = La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm
            | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv | Lw | Lx | Ly | Lz
            deriving (Eq, Show, Enum, Ord, Bounded, Ix)

data Multiplier = MultiplyLetter Int
                | MultiplyWord Int
                deriving (Show, Eq)

data Cell = Cell { letter     :: Letter
                 , multiplier :: Multiplier
                 , used       :: Bool
                 } deriving (Show)

type Score        = Int
type Position     = (Int, Int)
type Grid         = Array Position Cell
type LetterScores = Array Letter Score

type Problem      = [String]
type Path         = [Position]

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

cellScore :: Cell -> Score
cellScore cell = (letterScores ! letter cell) * mult
    where mult = case multiplier cell of MultiplyLetter m -> m
                                         MultiplyWord   _ -> 1

translate :: (Eq a, Eq b) => [a] -> [b] -> b -> a -> b
translate (from:froms) (to:tos) def value
    | from == value = to
    | otherwise     = translate froms tos def value
translate _ _ def _ = def

showLetter :: Letter -> Char
showLetter = translate [La .. Lz] ['A' .. 'Z'] (error "Unknown error")

readLetter :: Char -> Letter
readLetter = translate ['A' .. 'Z'] [La .. Lz] (error "Invalid char")

readMultiplier :: Char -> Multiplier
readMultiplier = translate
     ['d', 't', 'D', 'T']
     [MultiplyLetter 2, MultiplyLetter 3, MultiplyWord 2, MultiplyWord 3]
     (MultiplyLetter 1)

readCell :: String -> (Cell, String)
readCell (oneLetter:oneMultiplier:remains) =
    ( Cell { letter     = readLetter oneLetter
           , multiplier = readMultiplier oneMultiplier
           , used       = False
           }
    , remains
    )
readCell _ = error "Invalid input given to coupleToCell"

readRow :: String -> [Cell]
readRow []     = []
readRow string = oneCell:readRow remains
    where (oneCell, remains) = readCell string

readGrid :: Problem -> Grid
readGrid = listArray ((0, 0), (3, 3)) . readRow . concat

pathToString :: Grid -> Path -> String
pathToString grid = map (showLetter . letter . (grid !))

pathToCells :: Grid -> Path -> [Cell]
pathToCells grid = map (grid !)

evalScore' :: Score -> Int -> [Cell] -> Score
evalScore' score mult [] = score * mult
evalScore' score mult (cell:remains) =
    evalScore' (score + cellScore cell) mult' remains
    where mult' = case multiplier cell of
                    MultiplyLetter _ -> mult
                    MultiplyWord   m -> mult * m

evalScore :: Grid -> Path -> Score
evalScore grid = (evalScore' 0 1) . pathToCells grid

around :: Position -> [Position]
around (x, y) =
    [(x', y') | x' <- [x-1 .. x+1] -- Look for left to right
              , y' <- [y-1 .. y+1] -- Look for top to bottom
              , (x',y') /= (x,y)   -- it cannot be the position itself
              , x' >= 0 && x' <= 3 -- Coordinates must be on the grid
              , y' >= 0 && y' <= 3 -- Coordinates must be on the grid
    ]

walk :: Grid -> Dictionary -> [Path]
walk grid dictionary = concat $ map walker initialPaths
    where initialPaths :: [Path]
          initialPaths = concat $ map (\p1 -> map (\p2 -> p1:p2:[]) (around p1)) [(x, y) | x <- [0 .. 3], y <- [0 .. 3]]
          walker :: Path -> [Path]
          walker path = case check (pathToString grid path) dictionary of
                            None     -> []
                            Partial  -> concat nextPaths
                            Complete -> path: concat nextPaths
              where position  = last path
                    arounds   = around position \\ path
                    nextPaths = map (walker . (path ++) . (: [])) arounds

