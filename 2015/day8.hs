import Data.Char
import Data.Bits
import qualified Data.Map as M

import LibAdvent
import LibDyn

main :: IO ()
main = do
    contents <- readFile "day8.txt"
    let items = lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne items)
    putStrLn $ "Part two : " ++ show (solvePartTwo items)

solvePartOne :: [String] -> Int
solvePartOne = sum . map f
    where
        f "" = 0
        f ('"':xs) = 1 + f xs
        f ('\\':'\\':xs) = 1 + f xs
        f ('\\':'"':xs) = 1 + f xs
        f ('\\':'x':_:_:xs) = 3 + f xs
        f (_:xs) = f xs

solvePartTwo :: [String] -> Int
solvePartTwo = sum . map f
    where
        f x = 2 + sum (map g x)
        g '\\' = 1
        g '"' = 1
        g _ = 0

