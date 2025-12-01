main :: IO ()
main = do
    contents <- readFile "day1.txt"
    putStrLn $ "Part one : " ++ show (solvePartOne contents)
    putStrLn $ "Part two : " ++ show (solvePartTwo contents)

solvePartOne :: String -> Int
solvePartOne = sum . map charToMove

charToMove :: Char -> Int
charToMove '(' = 1
charToMove ')' = -1

solvePartTwo :: String -> Int
solvePartTwo = f 0 0
    where
        f index (-1) _ = index
        f index level (c:cs) = f (index + 1) (level + charToMove c) cs

