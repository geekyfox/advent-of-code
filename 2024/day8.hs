import Data.Maybe(catMaybes);
import qualified Data.Set as S;

main :: IO ()
main = do
    contents <- readFile "day8.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Puzzle = (Antennas, Int, Int)
type Antennas = [Antenna]
type Antenna = (Spot, Tag)
type Spot = (Int, Int)
type Tag = Char

parse :: String -> Puzzle
parse contents = (antennas, rmax, cmax)
    where
        antennas = [((r,c),t) | (r,ts) <- zip [1..] cs,
                                (c,t) <- zip [1..] ts,
                                t /= '.'  ]
        rmax = length cs
        cmax = length $ head cs
        cs = lines contents

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

countUnique :: (Ord a) => [a] -> Int
countUnique = length . unique

solvePartOne :: Puzzle -> Int
solvePartOne p = countUnique [ s | pair <- pairs p, s <- impl pair ]
    where
        impl (s1, s2) = take 1 $ drop 1 $ trace p s1 s2

pairs :: Puzzle -> [(Spot, Spot)]
pairs (antennas, _, _) = [
    (s1, s2) | (s1, t1) <- antennas,
               (s2, t2) <- antennas,
               t1 == t2, s1 /= s2 ]

isWithinBounds :: Puzzle -> Int -> Int -> Bool
isWithinBounds (_, rmax, cmax) r c =
    (r >= 1) && (r <= rmax) && (c >= 1) && (c <= cmax)

trace :: Puzzle -> Spot -> Spot -> [Spot]
trace p (r1, c1) (r2, c2) = takeWhile check gen
    where
        check (r,c) = isWithinBounds p r c
        gen = iterate next (r2, c2)
        next (r, c) = (r + dr, c + dc)
        dr = r2 - r1
        dc = c2 - c1

solvePartTwo :: Puzzle -> Int
solvePartTwo p = countUnique [ s | pair <- pairs p, s <- impl pair ]
    where
        impl (s1, s2) = trace p s1 s2
