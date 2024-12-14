import qualified Data.Map as M;

main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

type Rule = (Int, Int)
type Rules = M.Map Rule Bool
type Update = [Int]
type Updates = [Update]
type Puzzle = (Rules, Updates)

parse :: String -> Puzzle
parse contents = (rules, updates)
    where
        rules = M.fromList [ e | r <- rs,
                                 let (x, y) = parseRule r,
                                 e <- [((x, y), True), ((y, x), False)] ]
        parseRule r = let (a, b) = splitOnce '|' r in (read a, read b)
        updates = map parseUpdate us
        parseUpdate = map read . split ','
        (rs, us) = splitOnce "" $ lines contents

splitOnce :: Eq (a) => a -> [a] -> ([a], [a])
splitOnce sep xs = f [] xs
    where
        f acc [] = (reverse acc, [])
        f acc (x:xs)
            | x == sep = (reverse acc, xs)
            | otherwise = f (x:acc) xs

split :: Eq (a) => a -> [a] -> [[a]]
split sep xs =
    case splitOnce sep xs of
         (x, []) -> [x]
         (x, xs') -> x : (split sep xs')

solvePartOne :: Puzzle -> Int
solvePartOne (rules, updates) = sum $ map f updates
    where
        f u | updateIsValid rules u = mid u
            | otherwise = 0

updateIsValid :: Rules -> Update -> Bool
updateIsValid rs u = null $ findErrors rs u

findErrors :: Rules -> Update -> [(Int, Int)]
findErrors rs [] = []
findErrors rs (x:xs) = foo ++ bar
    where
        foo = [(x, y) | y <- xs, not $ pairIsValid x y]
        bar = findErrors rs xs
        pairIsValid x y =
            case M.lookup (x, y) rs of
                 Just x -> x
                 Nothing -> error $ show ("undecided", x, y)

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

solvePartTwo :: Puzzle -> Int
solvePartTwo (rules, updates) = sum $ map f updates
    where
        f u | updateIsValid rules u = 0
            | otherwise = mid $ fix rules u

fix :: Rules -> Update -> Update
fix rs u = if null errors then u else fix rs (patch u)
    where
        patch (x:xs)
            | x == foo = patch xs
            | x == bar = x:foo:xs
            | otherwise = x:patch xs
        foo = fst $ head errors
        bar = snd $ last [e | e <- errors, fst e == foo] 
        errors = findErrors rs u
