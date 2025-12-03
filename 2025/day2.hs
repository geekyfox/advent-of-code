import LibAdvent (split, splitOnce, unique)
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "day2.txt"
    let xs = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs)

parse :: String -> [(Integer, Integer)]
parse = map (f . splitOnce '-') . split ','
    where
        f (x, y) = (read x, read y)

solvePartOne :: [(Integer, Integer)] -> Integer
solvePartOne pairs = sum $ unique ids
    where
        ids = [ id | (x, y) <- pairs
                   , (id, r) <- solvePair x y
                   , r == 2 ]

solvePartTwo :: [(Integer, Integer)] -> Integer
solvePartTwo pairs = sum $ unique ids
    where
        ids = [ id | (x, y) <- pairs
                   , (id, r) <- solvePair x y ]

solvePair :: Integer -> Integer -> [(Integer, Int)]
solvePair x y
        | sx == sy = concatMap (solvePair' x y sx) [2..sx]
        | otherwise = solvePair x (z - 1) ++ solvePair z y
    where
        sx = length (show x)
        sy = length (show y)
        z  = 10 ^ sx
    
solvePair' :: Integer -> Integer -> Int -> Int -> [(Integer, Int)]
solvePair' _ _ sz reps | (sz `mod` reps) /= 0 = []
solvePair' x y sz reps = [ (n, reps) | n <- nums, n >= x, n <= y ]
    where
        nums  = map (step *) [a..b]
        a     = read (take repsz $ show x)
        b     = read (take repsz $ show y)
        step  = (10 ^ sz - 1) `div` (10 ^ repsz - 1)
        repsz = sz `div` reps

