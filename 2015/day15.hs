import qualified Data.Map as M
import Data.Maybe

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day15.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Ingridient = (Int, Int, Int, Int, Int)
type Problem = [Ingridient]

parse :: String -> Problem
parse = map parseLine . lines

parseLine :: String -> Ingridient
parseLine line = (a, b, c, d, e)
    where
        tokens = split ' ' line
        a      = read $ init $ tokens !! 2
        b      = read $ init $ tokens !! 4
        c      = read $ init $ tokens !! 6
        d      = read $ init $ tokens !! 8
        e      = read $ tokens !! 10

solvePartOne :: Problem -> Integer
solvePartOne problem = maximum [ x | (x, _) <- assessAll problem ]

solvePartTwo :: Problem -> Integer
solvePartTwo problem = maximum [ x | (x, y) <- assessAll problem, y == 500 ]

assessAll :: Problem -> [(Integer, Integer)]
assessAll problem = map (assess problem) (counts (length problem) 100)

counts :: Int -> Int -> [[Int]]
counts 1 sm = [[sm]]
counts n sm = [ x:xs | x <- [0..sm], xs <- counts (n - 1) (sm - x) ]

assess :: Problem -> [Int] -> (Integer, Integer)
assess problem counts = (a * b * c * d, e)
    where
        a = cook [ a | (a, _, _, _, _) <- problem ]
        b = cook [ b | (_, b, _, _, _) <- problem ]
        c = cook [ c | (_, _, c, _, _) <- problem ]
        d = cook [ d | (_, _, _, d, _) <- problem ]
        e = cook [ e | (_, _, _, _, e) <- problem ]
        cook = toInteger . max 0 . sum . zipWith (*) counts

