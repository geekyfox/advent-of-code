import LibAdvent;
import Data.List;
import Data.Maybe;

main :: IO ()
main = do
    contents <- readFile "day6.txt"
    let m = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne m)
    putStrLn $ "Part two : " ++ show (solvePartTwo m)

type Column = (Char, [String])
type Puzzle = [Column]

parse :: String -> Puzzle
parse txt = slice (last xs) (init xs)
    where
        xs = lines txt

slice :: String -> [String] -> Puzzle
slice "" _ = []
slice os xs = (o, ys) : slice os' xs'
    where
        (o, os', n) = trim os
        ys  = map (take n) xs
        xs' = map (drop n) xs

trim :: String -> (Char, String, Int)
trim (c:cs) = f 1 cs
    where
        f n (' ':xs) = f (n + 1) xs
        f n xs = (c, xs, n)

solvePartOne :: Puzzle -> Integer
solvePartOne = sum . map (uncurry f)
    where
        f '+' = g (+)
        f '*' = g (*)
        g fn xs = foldr1 fn $ map read xs

solvePartTwo :: Puzzle -> Integer
solvePartTwo = solvePartOne . map f
    where
        f (op, ns) = (op, mapMaybe g $ transpose ns)
        g "" = Nothing
        g (' ':xs) = g xs
        g xs = Just xs

