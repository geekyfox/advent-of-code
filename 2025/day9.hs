import LibAdvent;
import Data.List;
import qualified Data.Map as M;
import Data.Array;

main :: IO ()
main = do
    contents <- readFile "day9.txt"
    let p = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne p)
    putStrLn $ "Part two : " ++ show (solvePartTwo p)

type Puzzle = [Point]
type Point = (Int, Int)

parse :: String -> Puzzle
parse = map (f . split ',') . lines
    where
        f [x, y] = (read x, read y)

solvePartOne :: Puzzle -> Int
solvePartOne ps = maximum $
    [ area | (x1, y1) <- ps, (x2, y2) <- ps
           , let dx = x1 - x2, let dy = y1 - y2
           , let area = ((abs dx) + 1) * ((abs dy) + 1) ]

solvePartTwo :: Puzzle -> Int
solvePartTwo puzzle = maximum areas
    where
        areas = [ area | ((ai, aj), (bi, bj)) <- solveCompact compact
                       , let ax = ix2x M.! ai
                       , let ay = ix2y M.! aj
                       , let bx = ix2x M.! bi
                       , let by = ix2y M.! bj
                       , let dx = abs (ax - bx) + 1
                       , let dy = abs (ay - by) + 1
                       , let area = dx * dy ]
        compact = [ (x2ix M.! x, y2ix M.! y) | (x, y) <- puzzle ]
        x2ix = M.fromList $ zip xs [1..]
        ix2x = M.fromList $ zip [1..] xs
        y2ix = M.fromList $ zip ys [1..]
        ix2y = M.fromList $ zip [1..] ys
        xs = unique $ map fst puzzle
        ys = unique $ map snd puzzle

solveCompact :: [Point] -> [(Point, Point)]
solveCompact pts = [ (a, b) | a <- pts, b <- pts, valid a b ]
    where
        valid a b = and [ m ! p /= ' ' | p <- fill a b ]
        patch = [ ((x, y), ' ') | x <- [1, mxx], y <- [1, mxy] ]
        mxx = maximum $ map fst pts
        mxy = maximum $ map snd pts
        pts' = (tail pts) ++ [(head pts)]
        m = paint $ array ((0,0), (mxx + 1, mxy + 1)) $
            [ (p, ' ') | p <- fill (0, 0) (mxx + 1, mxy + 1) ] ++
            [ (p, '.') | p <- fill (1, 1) (mxx, mxy) ] ++
            [ (p, '#') | (a, b) <- zip pts pts', p <- fill a b ]

paint :: Array Point Char -> Array Point Char
paint arr | null patch = arr
          | otherwise = paint arr'
    where
        ((ax, ay), (zx, zy)) = bounds arr
        xs = [(ax + 1)..(zx - 1)]
        ys = [(ay + 1)..(zy - 1)]
        patch = [ (x, y) | x <- xs, y <- ys, f x y ]
        f x y = arr ! (x, y) == '.' && (
            (arr ! (x - 1, y) == ' ') ||
            (arr ! (x, y - 1) == ' ') ||
            (arr ! (x + 1, y) == ' ') ||
            (arr ! (x, y + 1) == ' '))
        arr' = arr // [ (p, ' ') | p <- patch ]

fill :: Point -> Point -> [Point]
fill (x1, y1) (x2, y2) = [
     (x, y) | x <- [(min x1 x2)..(max x1 x2)]
            , y <- [(min y1 y2)..(max y1 y2)] ]

