{- |
Module      : ruzzSolver
Description : Reads Ruzzle problems and solves them
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
import Control.Monad (liftM)

import Dictionary (Dictionary, DictWord, getDictionary)
import Solver.Types (Score, Grid, Path)
import Solver.Walker (walk)
import Solver.Problem (readGrid)
import Solver.Path (pathToString)
import Solver.Score (evalScore)

import Data.List (sortBy, nubBy, intercalate)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Data.Function (on)
import System.Environment (getArgs)

type Result = ([Path], [(DictWord, Score)])

{- |
Prepares 'Result' for printing.
-}
showResult :: Result -> String
showResult (solutions, scores) =
    intercalate "\n" $
        (prettyPrint <$> scores)
        ++ [ (show . length) scores ++ " words found." ]
        ++ [ (show . length) solutions ++ " solutions found." ]
    where
        prettyPrint (word, score) = word ++ ": " ++ show score

{- |
Solve a 'Grid': finds all solutions and best 'Score's for each solution.
-}
solveGrid :: Dictionary -> Grid -> Result
solveGrid dictionary grid = (solutions, scores)
    where
        -- Find all solutions to the grid
        solutions = walk grid dictionary

        -- Evaluate scores of all solutions and keep only the best score when
        -- a word has multiple scores
        scores = nubBy ((==) `on` fst)
               $ sortBy (flip $ comparing snd)
               $ (pathToString grid &&& evalScore grid) <$> solutions

{- |
Load a 'Grid' from a file.
-}
loadGrid :: String -> IO Grid
loadGrid = liftM (readGrid . lines) . readFile

main :: IO ()
main = do
    -- Check arguments
    problemFilePaths <- getArgs

    -- Get the dictionary
    dictionary <- getDictionary "dictionary/ruzzdictionary.txt"

    -- Get a list of 'Grid's to solve
    grids <- mapM loadGrid problemFilePaths

    -- Solve every 'Grid'
    let results = solveGrid dictionary <$> grids

    -- Display 'Result's
    putStrLn $ intercalate (replicate 79 '-') $ showResult <$> results
