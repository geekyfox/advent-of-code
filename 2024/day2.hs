main :: IO ()
main = do
    contents <- readFile "day2.txt"
    let xs = map parse $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs)

parse :: String -> [Int]
parse = map read . words

solvePartOne :: [[Int]] -> Int
solvePartOne = length . filter isSafe

isSafe :: [Int] -> Bool
isSafe x = isSlowlyIncreasing x || isSlowlyIncreasing (reverse x)

isSlowlyIncreasing :: [Int] -> Bool
isSlowlyIncreasing x = and $ zipWith checkPair x (tail x)
    where
        checkPair a b = (a < b) && (b - a < 4)

solvePartTwo :: [[Int]] -> Int
solvePartTwo = length . filter isAlmostSafe

isAlmostSafe :: [Int] -> Bool
isAlmostSafe = any isSafe . dampen

dampen :: [a] -> [[a]]
dampen xs = xs : map skipNth [1..(length xs)]
    where
        skipNth n = take (n - 1) xs ++ drop n xs
