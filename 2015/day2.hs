import Data.List(sort)

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day2.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

parse :: String -> [(Int, Int, Int)]
parse = map parseLine . lines

parseLine :: String -> (Int, Int, Int)
parseLine line =
    let
        tokens = split 'x' line
        [a, b, c] = map read tokens
    in
        (a, b, c)

solvePartOne :: [(Int, Int, Int)] -> Int
solvePartOne = sum . map f
    where
        f (a, b, c) =
            let
                [p, q, t] = sort [a, b, c]
            in
                p*q*3 + p*t*2 + q*t*2

solvePartTwo :: [(Int, Int, Int)] -> Int
solvePartTwo = sum . map f
    where
        f (a, b, c) =
            let
                [p, q, t] = sort [a, b, c]
            in
                p*2 + q*2 + p*q*t

