import Data.Either
import Data.List

import LibAdvent;

main :: IO ()
main = do
    contents <- readFile "day25.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)

type Lock = [Int]
type Key = [Int]
type Puzzle = ([Lock], [Key])

parse :: String -> Puzzle
parse = partitionEithers . scan . lines
    where
        scan xs =
            let
                (a, b) = break null xs
                c = case head (head a) of
                    '#' -> Left (toHeights a)
                    '.' -> Right (toHeights $ reverse a)
                cs = if null b
                        then []
                        else scan (tail b)
            in
                c:cs
        toHeights = map toHeight . transpose
        toHeight = length . takeWhile ('#' ==) . tail

solvePartOne :: Puzzle -> Int
solvePartOne (locks, keys) = length pairs
    where
        pairs = [ (a, b) | a <- locks, b <- keys,
                           let c = zipWith (+) a b,
                           maximum c < 6 ]

