import LibAdvent;
import Data.List;
import qualified Data.Map as M;

main :: IO ()
main = do
    contents <- readFile "day8.txt"
    let m = parse contents
    let ps = connPairs m
    putStrLn $ "Part one : " ++ show (solvePartOne ps)
    putStrLn $ "Part two : " ++ show (solvePartTwo m ps)

type Puzzle = [Point]
type Point = (Int, Int, Int)

parse :: String -> Puzzle
parse = map (f . split ',') . lines
    where
        f [x, y, z] = (read x, read y, read z)

data State = State (M.Map Int Int) (M.Map Int [Int]) deriving Show

initState :: Int -> State
initState n = State ntc ctns
    where
        ntc = M.fromList [ (i, i) | i <- [1..n] ]
        ctns = M.fromList [ (i, [i]) | i <- [1..n] ]

connect :: State -> (Int, Int) -> State
connect (State ntc ctns) (i, j)
        | ci == cj  = State ntc   ctns
        | ci <  cj  = State ntc'  ctns'
        | otherwise = State ntc'' ctns''
    where
        ci = ntc M.! i
        cj = ntc M.! j
        xs = ctns M.! ci
        ys = ctns M.! cj
        ntc'   = insertMany ntc [ (y, ci) | y <- ys ]
        ctns'  = M.insert ci (xs ++ ys) $ M.delete cj ctns
        ntc''  = insertMany ntc [ (x, cj) | x <- xs ]
        ctns'' = M.insert cj (xs ++ ys) $ M.delete ci ctns

connectMany :: State -> [(Int, Int)] -> State
connectMany = foldl connect
        
connPairs :: Puzzle -> [(Int, Int)]
connPairs p = map snd $ sort blob
    where
        blob = [ (dsq, (i, j)) | (i, a) <- zip [1..] p
                               , let (x1, y1, z1) = a
                               , (j, b) <- zip [1..] p
                               , i < j
                               , let (x2, y2, z2) = b
                               , let dx = x1 - x2
                               , let dy = y1 - y2
                               , let dz = z1 - z2
                               , let dsq = dx * dx + dy * dy + dz * dz ]

solvePartOne :: [(Int, Int)] -> Int
solvePartOne ps = product maxSizes
    where
        (State _ ctns) = connectMany (initState 1000) $ take 1000 ps
        sizes = map length $ M.elems ctns
        maxSizes = take 3 $ reverse $ sort sizes


solvePartTwo :: Puzzle -> [(Int, Int)] -> Int
solvePartTwo puzzle = render . seek (initState 1000)
    where
        seek ns (c:cs) =
            let ns' = connect ns c
            in if isFinal ns' then c else seek ns' cs
        render (i, j) =
            let (x1, _, _) = puzzle !! (i - 1)
                (x2, _, _) = puzzle !! (j - 1)
            in x1 * x2
        isFinal (State _ ctns) = M.size ctns == 1

