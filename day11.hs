import Data.List(sort)

main :: IO ()
main = do
    contents <- readFile "day11.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

parse :: String -> [Integer]
parse = map read . words

solvePartOne :: [Integer] -> Integer
solvePartOne = solve 25

solvePartTwo :: [Integer] -> Integer
solvePartTwo = solve 75

solve :: Int -> [Integer] -> Integer
solve n = unwrap . perform n evolve . map wrap
    where
        wrap x = (x, 1)
        unwrap = sum . map snd
        perform 0 f x = x
        perform n f x = perform (n-1) f (f x)

evolve :: [(Integer, Integer)] -> [(Integer, Integer)]
evolve = pack . sort . expand

pack :: [(Integer, Integer)] -> [(Integer, Integer)]
pack [] = []
pack ((x,n):(y,m):xs) | x == y = pack $ (x, m+n):xs
pack (x:xs) = x : pack xs

expand :: [(Integer, Integer)] -> [(Integer, Integer)]
expand [] = []
expand ((x,ct):xs)
        | x == 0 = (1, ct):rest
        | (Just (y, z)) <- chop x = (y, ct):(z, ct):rest
        | otherwise = (x * 2024, ct):rest
    where
        rest = expand xs

chop :: Integer -> Maybe (Integer, Integer)
chop x
    | even count = Just (left, right)
    | otherwise = Nothing
    where
        count = countDigits x
        left = takeDigits (count `div` 2) x
        right = dropDigits (count `div` 2) x

countDigits :: Integer -> Int
countDigits 0 = 0
countDigits n = impl n
    where
        impl 0 = 0
        impl n = 1 + impl (n `div` 10)

takeDigits :: Int -> Integer -> Integer
takeDigits n x | n <= 0 = 0
takeDigits n x = (x `mod` 10) + takeDigits (n - 1) (x `div` 10) * 10

dropDigits :: Int -> Integer -> Integer
dropDigits n x | n <= 0 = x
dropDigits n x = dropDigits (n - 1) (x `div` 10)
