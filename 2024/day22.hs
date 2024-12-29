import Data.Array;
import Data.Bits

import LibAdvent;

main :: IO ()
main = do
    contents <- readFile "day22.txt"
    let puzzle = map read $ lines contents
    putStrLn $ "Part one : " ++ (show $ solvePartOne puzzle)
    putStrLn $ "Part two : " ++ (show $ solvePartTwo puzzle)

solvePartOne :: [Int] -> Int
solvePartOne = sum . map (last . numbers)

numbers :: Int -> [Int]
numbers = take 2001 . iterate nextNumber
    where
        nextNumber = stepC . stepB . stepA
        stepA x = mixPrune x (x * 64)
        stepB x = mixPrune x (x `div` 32)
        stepC x = mixPrune x (x * 2048)
        mixPrune x y = (x `xor` y) `mod` 16777216

solvePartTwo :: [Int] -> Int
solvePartTwo = maximum . map snd . elems . summarize . map bananas
    where
        summarize = accumArray f (0, 0) (0, mx) . concat 
        mx = 19 * 19 * 19 * 19
        f (prevSeed, total) (seed, value)
            | prevSeed == seed = (prevSeed, total)
            | otherwise = (seed, total + value)
        
bananas :: Int -> [(Int, (Int, Int))]
bananas seed = moves
    where
        prices = map (\x -> x `mod` 10) $ numbers seed
        moves =
            let
                (a:b:c:d:rest) = prices
            in
                chop a b c d rest
        chop _ _ _ _ [] = []
        chop a b c d (e:es) =
            let
                x = (b - a + 9)
                y = (c - b + 9) * 19
                z = (d - c + 9) * 19 * 19
                w = (e - d + 9) * 19 * 19 * 19
                k = x + y + z + w
                r = (k, (seed, e))
            in
                r:(chop b c d e es)

