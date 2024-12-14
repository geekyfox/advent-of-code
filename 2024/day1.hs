import Data.List(sort);
import qualified Data.Map as M;
import Data.Maybe;

main :: IO ()
main = do
    contents <- readFile "day1.txt"
    let (xs, ys) = unzip $ map parse $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs ys)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs ys)

parse :: String -> (Int, Int)
parse c = (read a, read b)
    where
        [a, b] = words c

solvePartOne :: [Int] -> [Int] -> Int
solvePartOne xs ys = sum $ zipWith measure xss yss
    where
        measure x y = abs (x - y)
        xss = sort xs
        yss = sort ys

solvePartTwo :: [Int] -> [Int] -> Int
solvePartTwo xs ys = sum $ map score xs
    where
        score x = x * count x
        count x = fromMaybe 0 $ M.lookup x counts
        counts = M.fromListWith (+) $ map (\y -> (y, 1)) ys
