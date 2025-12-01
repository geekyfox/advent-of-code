import Data.Array

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day6.txt"
    let tasks = map parseTask $ lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne tasks)
    putStrLn $ "Part two : " ++ show (solvePartTwo tasks)

data Op = On | Off | Toggle deriving Show
data Task = Task Op (Int, Int) (Int, Int) deriving Show
type Atom = (Int, Op)

parseTask :: String -> Task
parseTask x = Task op (rx, cx) (ry, cy)
    where
        (op, a, b) = case split ' ' x of
            ["toggle", p, "through", q] -> (Toggle, p, q)
            ["turn", "off", p, "through", q] -> (Off, p, q)
            ["turn", "on", p, "through", q] -> (On, p, q)
            xs -> error $ show xs
        (ra, ca) = parseCell a
        (rb, cb) = parseCell b
        rx = min ra rb
        ry = max ra rb
        cx = min ca cb
        cy = max ca cb

parseCell :: String -> (Int, Int)
parseCell x = (read a, read b)
    where
        [a, b] = split ',' x

atomize :: [Task] -> [Atom]
atomize = concatMap f
    where
        f (Task op (ra, ca) (rb, cb)) = [ (r * 1000 + c, op) |
            r <- [ra..rb], c <- [ca..cb] ]

solvePartOne :: [Task] -> Int
solvePartOne = solve f
    where
        f Toggle x = 1 - x
        f Off _ = 0
        f On _ = 1

solvePartTwo :: [Task] -> Int
solvePartTwo = solve f
    where
        f Toggle n = n + 2
        f Off n = max 0 (n - 1)
        f On n = n + 1

solve :: (Op -> Int -> Int) -> [Task] -> Int
solve f = sum . elems . accumArray (flip f) 0 (0, 999999) . atomize

