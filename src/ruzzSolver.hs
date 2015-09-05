import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Dictionary (DictWord, getDictionary)
import Solver (Score, readGrid, walk, pathToString, evalScore)
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Data.Function (on)
import System.Environment (getArgs)

prettyPrint :: (DictWord, Score) -> String
prettyPrint (word, score) = word ++ ": " ++ show score

main :: IO ()
main = do
    -- Check arguments
    args <- getArgs

    let problemFilePath =
            case args of
                arg : _ -> arg
                _       -> error "Need to specify the problem file"

    -- Get a grid from a problem
    grid <- liftM (readGrid . lines) (readFile problemFilePath)

    -- Get the dictionary
    dictionary <- getDictionary "dictionary/ruzzdictionary.txt"

    let -- Find all solutions to the grid
        solutions = walk grid dictionary

        -- Evaluate scores of all solutions
        scores = nubBy ((==) `on` fst)
               $ sortBy (flip $ comparing snd)
               $ (pathToString grid &&& evalScore grid) <$> solutions

    -- Pretty print each solution with its associated score
    mapM_ (putStrLn . prettyPrint) scores

    -- Show how many solutions were found
    putStrLn $ (show . length) scores ++ " words found."
    putStrLn $ (show . length) solutions ++ " solutions found."
