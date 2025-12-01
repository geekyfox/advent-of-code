import Data.Char

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day16.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Stats = [(String, Int)]
type Problem = [(Int, Stats)]

parse :: String -> Problem
parse = map parseLine . lines

parseLine :: String -> (Int, Stats)
parseLine line = (index, stats)
    where
        tokens = split ' ' line
        index = parseNumber $ tokens !! 1
        stats = parseStats $ drop 2 tokens
        parseStats [] = []
        parseStats (x:y:xs) = (init x, parseNumber y) : parseStats xs
        parseNumber x
            | isDigit (last x) = read x
            | otherwise = read (init x)

solvePartOne :: Problem -> Int
solvePartOne = one . map fst . filter (matchPartOne . snd)

matchPartOne :: Stats -> Bool
matchPartOne = all (uncurry f)
    where
        f key value = case key of
            "children"    -> value == 3
            "cats"        -> value == 7
            "samoyeds"    -> value == 2
            "pomeranians" -> value == 3
            "akitas"      -> value == 0
            "vizslas"     -> value == 0
            "goldfish"    -> value == 5
            "trees"       -> value == 3
            "cars"        -> value == 2
            "perfumes"    -> value == 1
            _             -> error key

solvePartTwo :: Problem -> Int
solvePartTwo = one . map fst . filter (matchPartTwo . snd)

matchPartTwo :: Stats -> Bool
matchPartTwo = all (uncurry f)
    where
        f key value = case key of
            "children"    -> value == 3
            "cats"        -> value > 7
            "samoyeds"    -> value == 2
            "pomeranians" -> value < 3
            "akitas"      -> value == 0
            "vizslas"     -> value == 0
            "goldfish"    -> value < 5
            "trees"       -> value > 3
            "cars"        -> value == 2
            "perfumes"    -> value == 1
            _             -> error key

