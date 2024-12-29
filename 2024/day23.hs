import Data.List
import qualified Data.Map as M;
import qualified Data.Set as S;

import LibAdvent;

main :: IO ()
main = do
    contents <- readFile "day23.txt"
    let puzzle = map parseLine $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ solvePartTwo puzzle

parseLine :: String -> (String, String)
parseLine = splitOnce '-'

solvePartOne specs = countUnique trios
    where
        trios = [ (p, q, t) | (a, bs) <- M.toList nodes,
                              head a == 't',
                              b <- bs, c <- bs, b < c,
                              S.member (b, c) conns,
                              let [p, q, t] = sort [a, b, c] ]
        conns = S.fromList pairs
        nodes = M.fromListWith (++) [ (a, [b]) | (a, b) <- pairs ]
        pairs = specs ++ [ (b, a) | (a, b) <- specs ]

solvePartTwo :: [(String, String)] -> String
solvePartTwo specs = format (seek upperBound)
    where
        format = intercalate "," . sort
        seek size =
            case filter isClique $ makeAllSubsets size of
                [] -> seek (size - 1)
                (x:_) -> x
        upperBound = maximum [ length (a:bs) | (a, bs) <- M.assocs nodes ]
        isClique [] = True
        isClique (a:bs) = isClique bs && all (\b -> S.member (a, b) conns) bs
        makeAllSubsets want = concat $ [ 
            makeSubsets want has xs | (a, bs) <- M.assocs nodes,
                                      let xs = (a:bs),
                                      let has = length xs ]
        nodes = M.fromListWith (++) [ (a, [b]) | (a, b) <- pairs ]
        conns = S.fromList pairs
        makeSubsets want has xs
            | want == has = [xs]
            | want > has = []
            | want == 0 = [[]]
        makeSubsets want has (x:xs) = 
            [ x:ys | ys <- makeSubsets (want - 1) (has - 1) xs ] ++
            makeSubsets want (has - 1) xs
        pairs = specs ++ [ (b, a) | (a, b) <- specs ]

