import Data.Array
import LibDyn

main :: IO ()
main = do
    contents <- readFile "day17.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

parse :: String -> [Int]
parse = map read . lines

solvePartOne :: [Int] -> Int
solvePartOne = last . solve

solvePartTwo :: [Int] -> Int
solvePartTwo = head . filter ((/=) 0) . solve

solve :: [Int] -> [Int]
solve buckets = fst $ getMany m [(count, 150, k) | k <- [1..count]]
    where
        count = length buckets
        cache = listArray (1, count) buckets
        m = wrap f
        f :: (Int, Int, Int) -> Meme (Int, Int, Int) Int
        f (i, j, k)
            | k < 0 = return 0
            | j < 0 = return 0
            | j == 0 = return 1
            | i <= 0 = return 0
        f (i, j, k) = do
            let x = cache ! i
            a <- dyn (i - 1, j, k)
            b <- dyn (i - 1, j - x, k - 1)
            return $ a + b

