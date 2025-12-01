import qualified Data.Map as M
import Data.Maybe

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day13.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Rule = ((String, String), Int)
type Problem = (M.Map (Int, Int) Int, Int)

parse :: String -> Problem
parse text = (twoWay, count)
    where
        rules = map parseLine $ lines text
        names = zip [1..] $ unique [ x | ((x, _), _) <- rules ]
        oneWay = M.fromList rules
        twoWay = M.fromList [
            ((x, y), v + w) | (x, a) <- names
                            , (y, b) <- names
                            , x /= y
                            , let v = oneWay !!! (a, b)
                            , let w = oneWay !!! (b, a) ]
        count = length names

parseLine :: String -> Rule
parseLine line = ((firstName, secondName), units)
    where
        tokens = split ' ' line
        firstName = head tokens
        secondName = init $ last tokens
        verb = tokens !! 2
        absUnits = read $ tokens !! 3
        units = case verb of
                     "gain" -> absUnits
                     "lose" -> absUnits * (-1)
                     _ -> error line

solvePartOne :: Problem -> Int
solvePartOne (twoWay, count) = maximum $ map evaluate $ permutations [1..count]
    where
        evaluate = sum . map evalPair . toPairs
        evalPair p = fromMaybe 0 $ M.lookup p twoWay
        toPairs xs = zip xs $ tail xs ++ [head xs]

solvePartTwo :: Problem -> Int
solvePartTwo (mapping, count) = solvePartOne (mapping, count + 1)

