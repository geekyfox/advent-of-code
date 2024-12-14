main :: IO ()
main = do
    contents <- readFile "day7.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Equation = (Int, [Int])
type Puzzle = [Equation]

parse :: String -> Puzzle
parse = map parseOneLine . lines

parseOneLine :: String -> Equation
parseOneLine line = (a, as)
    where
        a = read $ reverse $ tail $ reverse b
        as = map read bs
        (b:bs) = words line

solvePartOne :: Puzzle -> Int
solvePartOne = solveUsing [(+), (*)]

solveUsing :: [Int -> Int -> Int] -> Puzzle -> Int
solveUsing funs = sum . map fst . filter (isValid funs)

isValid :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
isValid funs (n, xs) = impl xs
    where
        impl [x] = (n == x)
        impl (x:y:xs) = or [impl ((f x y):xs) | f <- funs]

solvePartTwo :: Puzzle -> Int
solvePartTwo = solveUsing [(+), (*), concatInts]

concatInts :: Int -> Int -> Int
concatInts x 0 = x * 10
concatInts x y = impl 1
    where
        impl z | z > y = x * z + y
        impl z = impl (z * 10)
