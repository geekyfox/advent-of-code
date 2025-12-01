import Data.List(sort)

main :: IO ()
main = do
    contents <- readFile "day24.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

parse :: String -> [Int]
parse = reverse . sort . map read . lines

solvePartOne :: [Int] -> Int
solvePartOne = minimum . map product . packages 3

solvePartTwo :: [Int] -> Int
solvePartTwo = minimum . map product . packages 4

packages :: Int -> [Int] -> [[Int]]
packages n xs = f 1
    where
        f ct = case pick xs sm ct of
                    [] -> f (ct + 1)
                    ys -> ys
        sm = sum xs `div` n

pick :: [Int] -> Int -> Int -> [[Int]]
pick _ 0 0 = [[]]
pick _ sm _ | sm < 0 = []
pick _ _ ct | ct < 0 = []
pick [] _ _ = []
pick (x:xs) sm ct = as ++ bs
    where
        as = [ x:ys | ys <- pick xs (sm - x) (ct - 1) ]
        bs = pick xs sm ct

