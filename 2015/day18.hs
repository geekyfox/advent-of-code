import Data.Array

main :: IO ()
main = do
    contents <- readFile "day18.txt"
    let grid = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne grid)
    putStrLn $ "Part two : " ++ show (solvePartTwo grid)

type Grid = Array (Int, Int) Bool

parse :: String -> Grid
parse contents = array bounds assocs
    where
        rows = lines contents
        rowCount = length rows
        colCount = length (head rows)
        bounds = ((1, 1), (rowCount, colCount))
        assocs = [ ((i, j), f c) | (i, cs) <- zip [1..] rows
                                 , (j, c)  <- zip [1..] cs ]
        f '#' = True
        f '.' = False
        f x   = error $ show x

solvePartOne :: Grid -> Int
solvePartOne = unwrap . (!! 100) . iterate next

solvePartTwo :: Grid -> Int
solvePartTwo = unwrap . (!! 100) . iterate (patch . next) . patch

unwrap :: Grid -> Int
unwrap = length . filter id . elems

next :: Grid -> Grid
next grid = array (bounds grid) [ (ix, f v ix) | (ix, v) <- assocs grid ]
    where
        ((rmin, cmin), (rmax, cmax)) = bounds grid
        f v ix = case (v, g ix) of
                      (True, 2) -> True
                      (True, 3) -> True
                      (False, 3) -> True
                      _ -> False
        g (r, c) = length [ () | r' <- [(max rmin (r-1))..(min rmax (r+1))]
                               , c' <- [(max cmin (c-1))..(min cmax (c+1))]
                               , (r, c) /= (r', c')
                               , grid ! (r', c') ]

patch :: Grid -> Grid
patch grid = grid // extra
    where
        ((rmin, cmin), (rmax, cmax)) = bounds grid
        extra = [ ((r, c), True) | r <- [rmin, rmax], c <- [cmin, cmax] ]


