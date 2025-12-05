import LibAdvent;
import Data.Array;

main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let m = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne m)
    putStrLn $ "Part two : " ++ show (solvePartTwo m)

type Puzzle = ([(Int, Int)], [Int])

parse :: String -> Puzzle
parse = f [] . lines
    where
        f acc ("":xs) = (pack acc, map read xs)
        f acc (x:xs) =
            let (a, b) = splitOnce '-' x
                acc' = (read a, read b):acc
            in f acc' xs

pack :: [(Int, Int)] -> [(Int, Int)]
pack [] = []
pack ((a,b):xs) = impl xs []
    where
        impl [] acc = (a, b) : pack acc
        impl ((c,d):rest) acc
            | (b < c) || (d < a) = impl rest ((c,d):acc)
            | otherwise = pack $ [(a `min` c, b `max` d)] ++ acc ++ rest

solvePartOne :: Puzzle -> Int
solvePartOne (ranges, ids) = length (filter isFresh ids)
    where
        isFresh id = any (`contains` id) ranges
        contains (start, end) id = (start <= id) && (id <= end)

solvePartTwo :: Puzzle -> Int
solvePartTwo (ranges, _) = sum [ b - a + 1 | (a, b) <- ranges ]

