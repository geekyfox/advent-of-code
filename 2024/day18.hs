import Data.Array
import qualified Data.Map as M
import Data.Maybe

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day18.txt"
    let puzzle = map (parsePair ',') $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Puzzle = [(Int, Int)]

solvePartOne :: Puzzle -> Int
solvePartOne = fromJust . solve . take 1024

solvePartTwo :: Puzzle -> (Int, Int)
solvePartTwo puzzle = bisect 1024 (length puzzle)
    where
        bisect good bad
            | bad == good + 1 = puzzle !! good
        bisect good bad =
            let mid = (good + bad) `div` 2
            in if blocked mid
                then bisect good mid
                else bisect mid bad
        blocked n = isNothing $ solve $ take n puzzle

solve :: Puzzle -> Maybe Int
solve pairs = M.lookup finish scores
    where
        start = (0, 0)
        finish = (70, 70)
        free = [ ((i, j), True) | i <- [0..70], j <- [0..70] ]
        taken = [ (p, False) | p <- pairs ]
        maze = array (start, finish) $ free ++ taken
        scores = fixMap advance (M.singleton start 0)
        advance cell v m = [ (cell', v + 1) |
            cell' <- adjacentAvailableCells maze cell,
            M.notMember cell' m ]

