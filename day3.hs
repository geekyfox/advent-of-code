import Data.Char(isDigit);
import Data.List(isPrefixOf);

main :: IO ()
main = do
    contents <- readFile "day3.txt"
    let xs = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne xs)
    putStrLn $ "Part two : " ++ show (solvePartTwo xs)

data Token = Mul Int Int | Do | Dont deriving Show

parse :: String -> [Token]
parse = f
    where
        f cs
          | "mul(" `isPrefixOf` cs = g "" (drop 4 cs)
          | "do()" `isPrefixOf` cs = Do : f (drop 4 cs)
          | "don't()" `isPrefixOf` cs = Dont : f (drop 7 cs)
        f (c:cs) = f cs
        f "" = []
        --
        g acc (c:cs) | isDigit c = g (acc ++ [c]) cs
        g acc (',':cs) | not (null acc) = h (read acc) "" cs
        g _ cs = f cs
        --
        h a acc (c:cs) | isDigit c = h a (acc ++ [c]) cs
        h a acc (')':cs) | not (null acc) = Mul a (read acc) : f cs
        h _ _ cs = f cs

solvePartOne :: [Token] -> Int
solvePartOne = sum . map f
    where
        f (Mul a b) = a * b
        f _ = 0

solvePartTwo :: [Token] -> Int
solvePartTwo = solvePartOne . accept

accept :: [Token] -> [Token]
accept (Dont:xs) = reject xs
accept (x:xs) = x : accept xs
accept [] = []

reject :: [Token] -> [Token]
reject (Do:xs) = accept xs
reject (x:xs) = reject xs
reject [] = []

