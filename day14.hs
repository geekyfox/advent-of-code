import Data.Array;
import qualified Data.Map as M;

main :: IO ()
main = do
    contents <- readFile "day14.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    let (n, txt) = solvePartTwo puzzle
    putStrLn $ "Part two : " ++ show n
    putStrLn txt

parse :: String -> [((Int, Int), (Int, Int))]
parse = map parseLine . lines

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = ((c, d), (e, f))
    where
        (a, b) = splitOnce ' ' line
        (c, d) = parseHalf a
        (e, f) = parseHalf b

parseHalf :: String -> (Int, Int)
parseHalf half = (read a, read b)
    where
        (a, b) = splitOnce ',' $ snd $ splitOnce '=' half

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

trace :: Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
trace 0 = fst
trace n = trace (n - 1) . next

next :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
next ((x, y), (dx, dy)) = ((x', y'), (dx, dy))
    where
        x' = (x + dx + 101) `mod` 101
        y' = (y + dy + 103) `mod` 103

solvePartOne :: [((Int, Int), (Int, Int))] -> Int
solvePartOne puzzle = a * b * c * d
    where
        xys = map (trace 100) puzzle
        a = length [ (x,y) | (x, y) <- xys, x < 50, y < 51 ]
        b = length [ (x,y) | (x, y) <- xys, x < 50, y > 51 ]
        c = length [ (x,y) | (x, y) <- xys, x > 50, y < 51 ]
        d = length [ (x,y) | (x, y) <- xys, x > 50, y > 51 ]

render :: [(Int, Int)] -> String
render xys = "\n" ++ [arr!(x,y) | y <- [0..102], x <- [0..101]]
    where
        empty = listArray ((0,0), (101,102)) (repeat '.')
        arr = empty // patch
        patch = [(xy, '*') | xy <- xys] ++ [((101, y), '\n') | y <- [0..102]]

solvePartTwo :: [((Int, Int), (Int, Int))] -> (Int, String)
solvePartTwo = impl 0
    where
        impl steps puzzle
            | isTree puzzle = (steps, render $ map fst puzzle)
            | otherwise = impl (steps + 1) (map next puzzle)

isTree :: [((Int, Int), (Int, Int))] -> Bool
isTree puzzle = hasBigRow && hasBigCol
    where
        xys = map fst puzzle
        rowCounts = M.fromListWith (+) [ (y, 1) | (x, y) <- xys ]
        hasBigRow = any (> 30) $ M.elems rowCounts
        colCounts = M.fromListWith (+) [ (x, 1) | (x, y) <- xys ]
        hasBigCol = any (> 30) $ M.elems colCounts
