import Data.Maybe(mapMaybe);

main :: IO ()
main = do
    contents <- readFile "day13.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Equation = ((Int, Int), (Int, Int), (Int, Int))
type Puzzle = [Equation]

parse :: String -> Puzzle
parse = bundle . mapMaybe parseLine . lines
    where
        bundle (a:b:c:cs) = (a,b,c):bundle cs
        bundle [] = []

parseLine :: String -> Maybe (Int, Int)
parseLine xs =
    case split ' ' xs of
         ["Button", "A:", a, b] -> readPair a b
         ["Button", "B:", a, b] -> readPair a b
         ["Prize:", a, b] -> readPair a b
         [""] -> Nothing
         tokens -> error $ show tokens
    where
        readX = read . drop 2 . init
        readY = read . drop 2
        readPair a b = Just (readX a, readY b)

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce sep xs = f [] xs
    where
        f acc [] = (reverse acc, [])
        f acc (x:xs)
            | x == sep = (reverse acc, xs)
            | otherwise = f (x:acc) xs

split :: Eq a => a -> [a] -> [[a]]
split sep xs =
    case splitOnce sep xs of
         (x, []) -> [x]
         (x, xs') -> x : split sep xs'


solvePartOne :: Puzzle -> Int
solvePartOne = sum . map (toTokens . solveEquation)

solveEquation :: Equation -> Maybe (Int, Int)
solveEquation ((a, b), (c, d), (p, q)) = do
    -- ax + cy = p
    -- bx + dy = q
    --
    -- abx + bcy = bp
    -- abx + ady = aq
    --
    -- y(bc - ad) = bp - aq
    y <- (b*p - a*q) `xdiv` (b*c - a*d)
    -- ax + cy = p
    -- ax = p - cy
    x <- (p - c * y) `xdiv` a
    Just (x, y)

toTokens :: Maybe (Int, Int) -> Int
toTokens (Just (x, y)) = x * 3 + y
toTokens Nothing = 0

xdiv :: Int -> Int -> Maybe Int
xdiv a b
    | a `mod` b == 0 = Just (a `div` b)
    | otherwise = Nothing

solvePartTwo :: Puzzle -> Int
solvePartTwo = solvePartOne . map patch
    where
        shift = 10000000000000
        patch (ab, cd, (p, q)) = (ab, cd, (p + shift, q + shift))
