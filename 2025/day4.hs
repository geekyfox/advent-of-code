import LibAdvent;
import Data.Array;

main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let m = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne m)
    putStrLn $ "Part two : " ++ show (solvePartTwo m)

type Puzzle = Array (Int, Int) Bool

parse :: String -> Puzzle
parse = fmap ('@' ==) . toCharMatrix

solvePartOne :: Puzzle -> Int
solvePartOne = length . accessible
    
accessible :: Puzzle -> [(Int, Int)]
accessible m = [ ix | (ix, v) <- assocs m, v, near ix < 4 ]
    where
        ((ax, ay), (zx, zy)) = bounds m
        get x y = (x >= ax) && (x <= zx) && 
                  (y >= ay) && (y <= zy) && 
                  (m ! (x, y))
        near (x, y) = sum [ 1 | (dx, dy) <- shifts, get (x + dx) (y + dy) ]

shifts :: [(Int, Int)]
shifts = [ (dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

solvePartTwo :: Puzzle -> Int
solvePartTwo m
    | null accs = 0
    | otherwise = length accs + solvePartTwo (patch m accs)
    where
        accs = accessible m

patch :: Puzzle -> [(Int, Int)] -> Puzzle
patch m ixes = m // [ (ix, False) | ix <- ixes ]

