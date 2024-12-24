import qualified Data.Map as M;
import qualified Data.Set as S;

import LibAdvent;
import LibDyn;

main :: IO ()
main = do
    contents <- readFile "day21.txt"
    let puzzle = lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

solvePartOne :: [String] -> Integer
solvePartOne = solve 2

solvePartTwo :: [String] -> Integer
solvePartTwo = solve 25

solve :: Int -> [String] -> Integer
solve depth codes = sum $ zipWith (*) prices numCodes
    where
        (prices, _) = resolve (wrap priceMove) $ pricePaths depth codes
        numCodes = map (read . init) codes

priceMove :: (Int, Char, Char) -> Meme (Int, Char, Char) Integer
priceMove (depth, f, t) =
    let
        paths = comboPaths !!! (f, t)
    in
        if depth == 0
            then return $ toInteger $ length $ head paths
            else fmap minimum $ pricePaths (depth - 1) paths

pricePaths :: Int -> [String] -> Result (Int, Char, Char) Integer [Integer]
pricePaths n [] = return []
pricePaths n (p:ps) = do
    v <- priceOnePath n p
    vs <- pricePaths n ps
    return $ (v:vs)

priceOnePath :: Int -> String -> Meme (Int, Char, Char) Integer
priceOnePath depth path = do
    let steps = [ (depth, f, t) | (f, t) <- zip ('A':path) path ]
    costs <- dynMany steps
    return (sum costs)

numericPaths :: M.Map (Char, Char) [String]
numericPaths = mkPaths [
    ('7', 1, 1), ('8', 1, 2), ('9', 1, 3),
    ('4', 2, 1), ('5', 2, 2), ('6', 2, 3),
    ('1', 3, 1), ('2', 3, 2), ('3', 3, 3),
                 ('0', 4, 2), ('A', 4, 3)  ]

dirPaths :: M.Map (Char, Char) [String]
dirPaths = mkPaths [
                 ('^', 1, 2), ('A', 1, 3),
    ('<', 2, 1), ('v', 2, 2), ('>', 2, 3)  ]

mkPaths :: [(Char, Int, Int)] -> M.Map (Char, Char) [String]
mkPaths input = M.fromList output
    where
        cells = S.fromList [ (r, c) | (_, r, c) <- input ]
        links a b
            | not (S.member a cells) = []
            | not (S.member b cells) = []
            | a == b = [""]
            | otherwise = hlinks a b ++ vlinks a b
        hlinks (ra, ca) (rb, cb) =
            case compare ca cb of
                 GT -> [ "<" ++ xs | xs <- links (ra, ca-1) (rb, cb) ]
                 LT -> [ ">" ++ xs | xs <- links (ra, ca+1) (rb, cb) ]
                 EQ -> []
        vlinks (ra, ca) (rb, cb) =
            case compare ra rb of
                 GT -> [ '^':xs | xs <- links (ra-1, ca) (rb, cb) ]
                 LT -> [ 'v':xs | xs <- links (ra+1, ca) (rb, cb) ]
                 EQ -> []
        paths a b = [ x ++ "A" | x <- links a b ]
        output = [ (k, v) | (ka, ra, ca) <- input,
                            (kb, rb, cb) <- input,
                            let k = (ka, kb),
                            let v = paths (ra, ca) (rb, cb) ]

comboPaths = M.union numericPaths dirPaths

