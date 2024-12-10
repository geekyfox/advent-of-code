import Data.Array;
import Data.Char(digitToInt);
import Data.List(sort);
import Data.Set(fromList, toList);
import Data.Tuple(swap);

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let arr = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne arr)
    putStrLn $ "Part two : " ++ show (solvePartTwo arr)

parse :: String -> Array (Int, Int) Int
parse contents = listArray bounds elems
    where
        rows = lines contents
        bounds = ((1, 1), (length rows, length $ head rows))
        elems = map digitToInt $ concat rows

solvePartOne :: Array (Int, Int) Int -> Int
solvePartOne = solve wrap combine unwrap
    where
        wrap r c = [(r,c)]
        combine = unique . concat
        unwrap = length
        unique = toList . fromList

solvePartTwo :: Array (Int, Int) Int -> Int
solvePartTwo = solve wrap combine unwrap
    where
        wrap _ _ = 1
        combine = sum
        unwrap = id

solve :: (Int -> Int -> a) -> ([a] -> a) -> (a -> Int)
         -> Array (Int, Int) Int -> Int
solve wrap combine unwrap puzzle = result
    where
        result = sum [ unwrap $ cache!ix | (ix,v) <- assocs puzzle, v == 0 ]
        cache = array (bounds puzzle) $ map cacheEntry nodes
        nodes = reverse $ sort $ map swap $ assocs puzzle
        cacheEntry (n, (r, c)) = ((r, c), compute n r c)
        compute 9 r c = wrap r c
        compute n r c = combine $
            [ cache!(r',c') | (dr, dc) <- [(1, 0), (0, 1), (-1, 0), (0, -1)],
                              let r' = r+dr, r' >= rmin, r' <= rmax,
                              let c' = c+dc, c' >= cmin, c' <= cmax,
                              puzzle!(r',c') == (n + 1) ]
        ((rmin, cmin), (rmax, cmax)) = bounds puzzle
