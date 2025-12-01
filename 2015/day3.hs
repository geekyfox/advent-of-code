import Data.List(sort)

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day3.txt"
    let puzzle = map charToSide contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

solvePartOne :: [Side] -> Int
solvePartOne = countUnique . trace

trace :: [Side] -> [(Int, Int)]
trace = f (0, 0)
    where
        f x [] = [x]
        f x (y:ys) = x:(f (shift y x) ys)

solvePartTwo :: [Side] -> Int
solvePartTwo puzzle = countUnique (xs ++ ys)
    where
        xs = trace as
        ys = trace bs
        (as, bs) = chop puzzle

chop :: [a] -> ([a], [a])
chop [] = ([], [])
chop [a] = ([a], [])
chop (a:b:rest) = (a:as, b:bs)
    where
        (as, bs) = chop rest

