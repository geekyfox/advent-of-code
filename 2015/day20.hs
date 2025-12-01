import Data.Char
import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day20.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

parse :: String -> Int
parse = read . head . lines

solvePartOne :: Int -> Int
solvePartOne n = head [ i | i <- [1..], giftsPartOne i >= n ]

giftsPartOne :: Int -> Int
giftsPartOne n = divisorSum n * 10

solvePartTwo :: Int -> Int
solvePartTwo n = head [ i | i <- [1..], giftsPartTwo i >= n ]

giftsPartTwo :: Int -> Int
giftsPartTwo n = 11 * sum [ n `div` i | i <- [1..50], n `mod` i == 0 ]

primes :: [Int]
primes = 2 : 3 : 5 : filter isPrime [7..]

isPrime :: Int -> Bool
isPrime n = f primes
    where
        f (x:xs)
            | (n `mod` x) == 0 = False
            | x*x > n = True
            | otherwise = f xs

divisorSum :: Int -> Int
divisorSum n = f n primes
    where
        f 1 _ = 1
        f n (p:ps)
            | n `mod` p == 0 = g n p ps
            | n < p*p = n + 1
            | otherwise = f n ps
        g n p ps =
            let (m, y) = h p 1 n
                z = f m ps
            in y * z
        h p acc n
            | n `mod` p == 0 = h p (acc * p + 1) (n `div` p)
            | otherwise = (n, acc)

