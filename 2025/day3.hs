import Data.Array (listArray, (!))
import LibDyn (eval, dyn)

main :: IO ()
main = do
    contents <- readFile "day3.txt"
    let xs = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs)

parse :: String -> [[Int]]
parse = map (map f) . lines
    where
        f x = read [x]

solvePartOne :: [[Int]] -> Int
solvePartOne = sum . map (optimizeBank 2)

optimizeBank :: Int -> [Int] -> Int
optimizeBank n digits = eval f (n, 0)
    where
        count = length digits
        lookup = listArray (0, count - 1) (reverse digits)
        f (0, _) = return 0
        f (n, ix) | n + ix > count = return 0
        f (n, ix) = do
            a <- dyn (n - 1, ix + 1)
            let c = a * 10 + (lookup ! ix)
            b <- dyn (n, ix + 1)
            return (b `max` c)

solvePartTwo :: [[Int]] -> Int
solvePartTwo = sum . map (optimizeBank 12)

