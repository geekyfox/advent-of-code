import Data.Array;

main :: IO ()
main = do
    contents <- readFile "day9.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

parse :: String -> [Int]
parse = map f . head . lines
    where
        f x = read [x]

solvePartOne :: [Int] -> Int
solvePartOne = checksumBlocks . rearrangeBlocks . mapToBlocks

mapToBlocks :: [Int] -> [Maybe Int]
mapToBlocks = concatMap f . mapToChunks
    where
        f (File i l _) = replicate l (Just i)
        f (Gap l _) = replicate l Nothing

rearrangeBlocks :: [Maybe Int] -> [Int]
rearrangeBlocks xs = f 1 sz
    where
        sz = length xs
        arr = listArray (1,sz) xs
        f i j
            | i > j = []
            | Just x <- arr!i = x:(f (i+1) j)
            | Just x <- arr!j = x:(f (i+1) (j-1))
            | otherwise = f i (j - 1)

checksumBlocks :: [Int] -> Int
checksumBlocks = sum . zipWith (*) [0..]

type Id = Int
type Length = Int
type Offset = Int
data Chunk = File Id Length Offset | Gap Length Offset deriving Show

mapToChunks :: [Int] -> [Chunk]
mapToChunks = f 0 0
    where
        f _ _ [] = []
        f id offset (x:xs) = (File id x offset):(g id (offset + x) xs)
        g _ _ [] = []
        g id offset (0:xs) = (f (id + 1) offset xs)
        g id offset (x:xs) = (Gap x offset):(f (id + 1) (offset + x) xs)

rearrangeChunks :: [Chunk] -> [(Id, Length, Offset)]
rearrangeChunks [] = []
rearrangeChunks ((File id len offset):xs) = (id, len, offset):(rearrangeChunks xs)
rearrangeChunks xs
    | (Gap _ _) <- x' = rearrangeChunks xs'
    | (Just xs'') <- patch = rearrangeChunks xs''
    | otherwise = (id, length, offset):(rearrangeChunks xs')
    where
        x' = last xs
        xs' = init xs
        patch = moveToFront id length xs'
        (File id length offset) = x'

moveToFront :: Id -> Length -> [Chunk] -> Maybe [Chunk]
moveToFront fi fl = f
    where
        f ((Gap gl go):xs)
            | fl < gl = Just $ (File fi fl go):(Gap (gl - fl) (go + fl)):xs
            | fl == gl = Just $ (File fi fl go):xs
        f (x:xs) = fmap ((:) x) $ f xs
        f [] = Nothing

checksumChunks :: [(Id, Length, Offset)] -> Int
checksumChunks = sum . map checksumChunk

checksumChunk :: (Id, Length, Offset) -> Int
checksumChunk (id, len, offset) = id * foo
    where
        foo = len * offset + bar
        bar = len * (len - 1) `div` 2

solvePartTwo :: [Int] -> Int
solvePartTwo = checksumChunks . rearrangeChunks . mapToChunks
