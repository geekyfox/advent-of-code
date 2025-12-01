main :: IO ()
main = do
    contents <- readFile "day1.txt"
    let xs = map parse $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs)

parse :: String -> Int
parse ('L':xs) = read xs * (-1)
parse ('R':xs) = read xs

solvePartOne :: [Int] -> Int
solvePartOne = length . filter fst . rotate

solvePartTwo :: [Int] -> Int
solvePartTwo = sum . map snd . rotate

rotate :: [Int] -> [(Bool, Int)]
rotate = rotateImpl 50

rotateImpl :: Int -> [Int] -> [(Bool, Int)]
rotateImpl _ [] = []
rotateImpl st (ch:chs) = (nd == 0, z):(rotateImpl nd chs)
    where
        (nd, z) = rotateOne st ch

rotateOne :: Int -> Int -> (Int, Int)
rotateOne st ch
    | (st + ch) > 0  = ((st + ch) `mod` 100, (st + ch) `div` 100)
    | (st + ch) == 0 = (0, 1)
    | st == 0 = rotateOne 100 ch
    | otherwise = (nd, z + 1)
        where
            (nd, z) = rotateOne (st + 100) ch

