module LibAdvent where

import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

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

(!!!) :: (Show k, Ord k) => M.Map k v -> k -> v
m !!! k =
    case M.lookup k m of
         Nothing -> error $ "Lookup failed : " ++ show k
         Just v -> v

insertMany :: (Ord k) => M.Map k v -> [(k, v)] -> M.Map k v
insertMany = foldl (\m (k, v) -> M.insert k v m)

deleteMany :: (Ord k) => M.Map k v -> [k] -> M.Map k v
deleteMany = foldl (flip M.delete)

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

countUnique :: (Ord a) => [a] -> Int
countUnique = S.size . S.fromList

toCharMatrix :: String -> Array (Int, Int) Char
toCharMatrix contents = listArray bounds elems
    where
        rows = lines contents
        bounds = ((1, 1), (length rows, length $ head rows))
        elems = concat rows

data Side = East | North | West | South deriving (Eq, Ord, Show)

allSides :: [Side]
allSides = [East, North, West, South]

shift :: Side -> (Int, Int) -> (Int, Int)
shift North (r, c) = (r - 1, c)
shift South (r, c) = (r + 1, c)
shift West (r, c) = (r, c - 1)
shift East (r, c) = (r, c + 1)

clockwise :: Side -> Side
clockwise East = South
clockwise South = West
clockwise West = North
clockwise North = East

counterClockwise :: Side -> Side
counterClockwise East = North
counterClockwise North = West
counterClockwise West = South
counterClockwise South = East

oppositeSide :: Side -> Side
oppositeSide = clockwise . clockwise

charToSide :: Char -> Side
charToSide '^' = North
charToSide 'v' = South
charToSide '<' = West
charToSide '>' = East

adjacentCells :: (Int, Int) -> [(Int, Int)]
adjacentCells cell = [shift side cell | side <- allSides]

adjacentAvailableCells :: Array (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
adjacentAvailableCells maze cell = filter isAvailable (adjacentCells cell)
    where
        isAvailable (r, c)
            | (r < rmin) || (r > rmax) = False
            | (c < cmin) || (c > cmax) = False
            | otherwise = maze!(r, c)
        ((rmin, cmin), (rmax, cmax)) = bounds maze

fixMap :: (Show k, Ord k) => (k -> v -> M.Map k v -> [(k, v)]) 
                          -> M.Map k v -> M.Map k v
fixMap advance zeroMap = impl zeroMap zeroWave
    where
        zeroWave = M.keys zeroMap
        impl m [] = m
        impl m (w:ws) =
            let
                patches = advance w (m !!! w) m
                m' = insertMany m patches
                ws' = ws ++ map fst patches
            in
                impl m' ws'

parsePair :: (Read a, Read b) => Char -> String -> (a, b)
parsePair sep line = (read x, read y)
    where
        (x, y) = splitOnce sep line

one :: (Show a) => [a] -> a
one [x] = x
one xs = error $ "Expected exactly one element, got " ++ show xs

permutations :: [a] -> [[a]]
permutations = foldr (concatMap . interject) [[]]

interject :: a -> [a] -> [[a]]
interject x = scan []
    where
        scan acc [] = [reverse (x:acc)]
        scan acc (y:ys) = pack acc (y:ys) : scan (y:acc) ys
        pack xs ys = reverse (x:xs) ++ ys

