import qualified Data.Map as M

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

parse :: String -> [Int]
parse "\n" = []
parse (x:xs) = read [x] : parse xs

solvePartOne :: [Int] -> Int
solvePartOne = solve 40

solvePartTwo :: [Int] -> Int
solvePartTwo = solve 50

solve :: Int -> [Int] -> Int
solve 0 xs = length xs
solve n xs = solve (n - 1) (rewrite xs)

rewrite :: [Int] -> [Int]
rewrite [] = []
rewrite (x:xs) = impl x 1 xs
    where
        impl x n (y:ys)
            | x == y = impl x (n + 1) ys
        impl x n ys = n:x:rewrite ys

