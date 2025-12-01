import qualified Data.Map as M

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day9.txt"
    let items = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne items)
    putStrLn $ "Part two : " ++ show (solvePartTwo items)

parse :: String -> M.Map (String, String) Int
parse text = M.fromList $ concatMap f x
    where
        x = map parseLine (lines text)
        f (a, b, n) = [((a, b), n), ((b, a), n)]

parseLine :: String -> (String, String, Int)
parseLine x = case split ' ' x of
    [a, "to", b, "=", c] -> (a, b, read c)

solvePartOne :: M.Map (String, String) Int -> Int
solvePartOne items = minimum $ map f paths
    where
        points = unique $ map fst $ M.keys items
        paths = permutations points
        f (x:y:rest) = (items !!! (x, y)) + (f $ y:rest)
        f [x] = 0

solvePartTwo :: M.Map (String, String) Int -> Int
solvePartTwo items = maximum $ map f paths
    where
        points = unique $ map fst $ M.keys items
        paths = permutations points
        f (x:y:rest) = (items !!! (x, y)) + (f $ y:rest)
        f [x] = 0

