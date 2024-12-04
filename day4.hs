import Data.Array;

main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let arr = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne arr)
    putStrLn $ "Part two : " ++ show (solvePartTwo arr)

parse :: String -> Array (Int, Int) Char
parse contents = array bounds assocs
    where
        rows = lines contents
        bounds = ((1, 1), (length rows, length $ head rows))
        assocs = [((r, c), x) | (r, xs) <- zip [1..] rows, (c,x) <- zip [1..] xs]

solvePartOne :: Array (Int, Int) Char -> Int
solvePartOne arr = sum [matches ix | ix <- indices arr]
    where
        ((rmin, cmin), (rmax, cmax)) = (bounds arr)
        paths = [(r,c) | r <- [-1..1], c <- [-1..1], abs r + abs c > 0]
        matches ix = length $ filter (check ix) paths
        check (r, c) (dr, dc) =
            arr!(r, c) == 'X' &&
            (r + dr * 3 >= rmin) && (r + dr * 3 <= rmax) &&
            (c + dc * 3 >= cmin) && (c + dc * 3 <= cmax) &&
            [arr!(r+dr*i, c+dc*i) | i <- [1..3]] == "MAS"

solvePartTwo :: Array (Int, Int) Char -> Int
solvePartTwo arr = length $ filter check $ indices arr
    where
        ((rmin, cmin), (rmax, cmax)) = (bounds arr)
        check (r, c)
            | (r <= rmin) || (r >= rmax) = False
            | (c <= rmin) || (c >= cmax) = False
            | otherwise = checkMain r c && checkCounter r c
        checkMain = checkDiagonal 1
        checkCounter = checkDiagonal (-1)
        checkDiagonal dc r c =
            (x, y, z) == ('M', 'A', 'S') ||
            (x, y, z) == ('S', 'A', 'M')
                where
                    x = arr!(r-1, c-dc)
                    y = arr!(r, c)
                    z = arr!(r+1, c+dc)
