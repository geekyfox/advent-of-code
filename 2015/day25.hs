main :: IO ()
main = do
    contents <- readFile "day25.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)

parse :: String -> (Int, Int)
parse contents = (x, y)
    where
        x = read $ init $ tokens !! 2
        y = read $ init $ head tokens
        tokens = reverse $ words contents

solvePartOne :: (Int, Int) -> Integer
solvePartOne = code . gridToSequence

gridToSequence :: (Int, Int) -> Int
gridToSequence (r, c) = f r c
    where
        f r c = c + g (r + c - 2)
        g c = c * (c + 1) `div` 2

code :: Int -> Integer
code n = f 1 20151125
    where
        f i x | i == n = x
        f i x = f (i + 1) (g x)
        g x = (x * 252533) `mod` 33554393

