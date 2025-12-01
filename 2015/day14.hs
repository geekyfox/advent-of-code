import qualified Data.Map as M
import Data.Maybe

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day14.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Entrant = (Int, Int, Int)
type Problem = [Entrant]

parse :: String -> Problem
parse = map parseLine . lines

parseLine :: String -> Entrant
parseLine line = (speed, fly, pause)
    where
        tokens = split ' ' line
        speed = read $ tokens !! 3
        fly   = read $ tokens !! 6
        pause = read $ tokens !! 13

solvePartOne :: Problem -> Int
solvePartOne = maximum . map raceResult

raceResult :: Entrant -> Int
raceResult rule = mkPlace rule !! 2503

mkPlace :: Entrant -> [Int]
mkPlace = emulate 0 . mkSteps
    where
        emulate n (x:xs) = n : emulate (n + x) xs

mkSteps :: Entrant -> [Int]
mkSteps (speed, fly, pause) = result
    where
        cycle = replicate fly speed ++ replicate pause 0
        result = cycle ++ result

mkBest :: Problem -> [Int]
mkBest = map maximum . transpose . map mkPlace

transpose :: [[a]] -> [[a]]
transpose xs = map head xs : transpose (map tail xs)

solvePartTwo :: Problem -> Int
solvePartTwo problem = maximum $ map eval problem
    where
        best = take 2503 $ mkBest problem
        eval entrant = sum [ 1 | (x, y) <- zip best (mkPlace entrant)
                               , x == y
                               , x > 0 ]

