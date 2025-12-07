import LibAdvent (unique);

main :: IO ()
main = do
    contents <- readFile "day7.txt"
    let m = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne m)
    putStrLn $ "Part two : " ++ show (solvePartTwo m)

type Puzzle = [[Cell]]
data Cell = Source | Split | Empty deriving (Show, Eq)

parse :: String -> Puzzle
parse = map (map toCell) . lines
    where
        toCell 'S' = Source
        toCell '^' = Split
        toCell '.' = Empty

solvePartOne :: Puzzle -> Int
solvePartOne (x:xs) = trace (start x) xs

start :: [Cell] -> [Bool]
start = map f
    where
        f Source = True
        f Empty = False

trace :: [Bool] -> [[Cell]] -> Int
trace _ [] = 0
trace beams (cells:rows) = count + trace beams' rows
    where
        blob = zip3 beams cells [1..]
        count = length [ () | (b, c, _) <- blob, b, c == Split ]
        beams' = render [1..(length beams)] indices
        indices = unique $ concatMap analyze blob
        analyze (False, _, _)    = []
        analyze (True, Empty, n) = [n]
        analyze (True, Split, n) = [n - 1, n + 1]
        render [] _ = []
        render (x:xs) (y:ys) | x == y = True : render xs ys
        render (x:xs) ys = False : render xs ys

solvePartTwo :: Puzzle -> Integer
solvePartTwo (x:xs) = trace2 (start2 x) xs

start2 :: [Cell] -> [Integer]
start2 = map f
    where
        f Source = 1
        f Empty = 0

trace2 :: [Integer] -> [[Cell]] -> Integer
trace2 tls [] = sum tls
trace2 tls (cells:rows) = trace2 tls' rows
    where
        tls' = render blob
        blob = concatMap analyze $ zip3 cells tls [1..]
        analyze (Empty, tl, n) = [(n, tl)]
        analyze (Split, tl, n) = [(n-1, tl), (n, 0), (n+1, tl)]
        render ((a, m):(b, n):xs) | a == b = render $ (a, m + n):xs
        render ((a, m):xs) = m : render xs
        render [] = []

