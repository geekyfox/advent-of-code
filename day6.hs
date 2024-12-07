import Data.Array;
import qualified Data.Set as S;
import Prelude hiding (Left, Right);

main :: IO ()
main = do
    contents <- readFile "day6.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Location = (Int, Int)
type Puzzle = Array Location Char

parse :: String -> Puzzle
parse contents = array bounds assocs
    where
        rows = lines contents
        bounds = ((1, 1), (length rows, length $ head rows))
        assocs = [((r, c), x) | (r, xs) <- zip [1..] rows, (c,x) <- zip [1..] xs]

data Side = Up | Down | Left | Right deriving (Eq, Ord)

advance :: Side -> Location -> Location
advance side (r, c) =
    case side of
         Up -> (r - 1, c)
         Down -> (r + 1, c)
         Left -> (r, c - 1)
         Right -> (r, c + 1)

turn :: Side -> Side
turn Up = Right
turn Right = Down
turn Down = Left
turn Left = Up

solvePartOne :: Puzzle -> Int
solvePartOne arr = countUnique $ map fst $ trace arr Nothing

trace :: Puzzle -> Maybe Location -> [(Location, Side)]
trace arr block = impl start Up
    where
        ((rmin, cmin), (rmax, cmax)) = bounds arr
        [start] = [ix | (ix, c) <- assocs arr, c == '^']
        impl loc side
            | isOutside loc' = [(loc, side)]
            | isBlocked loc' = impl loc (turn side)
            | otherwise = (loc, side) : impl loc' side
                where loc' = advance side loc
        isOutside (r, c) =
            (r < rmin) || (r > rmax) ||
            (c < cmin) || (c > cmax)
        isBlocked loc = Just loc == block || arr!loc == '#'

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

countUnique :: (Ord a) => [a] -> Int
countUnique = length . unique

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
