import Data.Array

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day6.txt"
    let puzzle = toCharMatrix contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Location = (Int, Int)
type Puzzle = Array (Int, Int) Char

solvePartOne :: Puzzle -> Int
solvePartOne arr = countUnique $ map fst $ trace arr Nothing

trace :: Puzzle -> Maybe Location -> [(Location, Side)]
trace arr block = impl start North
    where
        ((rmin, cmin), (rmax, cmax)) = bounds arr
        [start] = [ix | (ix, c) <- assocs arr, c == '^']
        impl loc side
            | isOutside loc' = [(loc, side)]
            | isBlocked loc' = impl loc (clockwise side)
            | otherwise = (loc, side) : impl loc' side
                where loc' = shift side loc
        isOutside (r, c) = (r < rmin) || (r > rmax) ||
                           (c < cmin) || (c > cmax)
        isBlocked loc = Just loc == block || arr!loc == '#'

solvePartTwo :: Puzzle -> Int
solvePartTwo arr = length $ filter check options
    where
        check loc = isLooping (trace arr $ Just loc)
        options = unique $ tail $ map fst $ trace arr Nothing

isLooping :: (Eq a) => [a] -> Bool
isLooping xs = impl xs (tail xs)
    where
        impl (x:xs) (y:_:ys)
            | x == y = True
            | otherwise = impl xs ys
        impl _ _ = False
