import Dictionary
import Solver

oneProblem :: Problem
oneProblem = [ "UdA U Tt"
             , "S UDNTCt"
             , "T A R S "
             , "E I TDC "
             ]

main :: IO ()
main = do
    dictionary <- getDictionary

    let grid = readGrid oneProblem

    print $ map (pathToString grid) (walk grid dictionary)

