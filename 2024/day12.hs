import Data.Array;
import qualified Data.Map as M;
import Data.Tuple(swap);

main :: IO ()
main = do
    contents <- readFile "day12.txt"
    let puzzle = parse contents
    let regions = detectRegions puzzle
    putStrLn $ "Part one : " ++ show (solvePartOne regions)
    putStrLn $ "Part two : " ++ show (solvePartTwo regions)

parse :: String -> Array (Int, Int) Char
parse contents = listArray bounds elems
    where
        rows = lines contents
        bounds = ((1, 1), (length rows, length $ head rows))
        elems = concat rows

solvePartOne :: Array (Int, Int) Int -> Int
solvePartOne regions = combine areas perimeters
    where
        combine a b = sum $ M.elems $ M.intersectionWith (*) a b
        areas = regionsToAreas regions
        perimeters = regionsToPerimeters regions

solvePartTwo :: Array (Int, Int) Int -> Int
solvePartTwo regions = combine areas angles
    where
        combine a b = sum $ M.elems $ M.intersectionWith (*) a b
        areas = regionsToAreas regions
        angles = regionsToAngles regions

detectRegions :: Array (Int, Int) Char -> Array (Int, Int) Int
detectRegions puzzle = fix zero
    where
        zero = array (bounds puzzle) $ zip (indices puzzle) [1..]
        fix reg = case patch reg of
                       [] -> reg
                       ps -> fix (reg // ps)
        patch reg = [ (x, n) | (x, y) <- links,
                               let m = reg!x, let n = reg!y,
                               n < m ]
        links = up ++ down ++ left ++ right
        up = [ (x, y) | (x, m) <- assocs puzzle,
                        let (r, c) = x, r > rmin,
                        let y = (r-1, c), puzzle!y == m ]
        left = [ (x, y) | (x, m) <- assocs puzzle,
                          let (r, c) = x, c > rmin,
                          let y = (r, c-1), puzzle!y == m ]
        down = map swap up
        right = map swap left
        ((rmin, cmin), (rmax, cmax)) = bounds puzzle

regionsToAreas :: Array (Int, Int) Int -> M.Map Int Int
regionsToAreas x = M.fromListWith (+) $ zip (elems x) (repeat 1)

regionsToPerimeters :: Array (Int, Int) Int -> M.Map Int Int
regionsToPerimeters x = M.fromListWith (+) $ map countBorders (assocs x)
    where
        ((rmin, cmin), (rmax, cmax)) = bounds x
        countBorders ((r, c), n) = (n, length [ () |
            (dr, dc) <- [(0,1), (1,0), (0,-1), (-1,0)],
            get (r + dr) (c + dc) /= n ])
        get r c
            | (r < rmin) || (r > rmax) || (c < cmin) || (c > cmax) = -1
            | otherwise = x!(r,c)

regionsToAngles :: Array (Int, Int) Int -> M.Map Int Int
regionsToAngles arr = M.fromListWith (+) $ map countAngles (assocs arr)
    where
        countAngles (rc, n) = (n, length [ () |
            side <- [TopLeft, TopRight, BottomLeft, BottomRight],
            isAngle arr rc side ])

data Side = TopLeft | TopRight | BottomLeft | BottomRight

isAngle :: Array (Int, Int) Int -> (Int, Int) -> Side -> Bool
isAngle arr (r, c) side =
    case (horIsSame, diagIsSame, vertIsSame) of
         (True, True, True) -> False
         (True, True, False) -> False
         (True, False, True) -> True
         (True, False, False) -> False
         (False, True, True) -> False
         (False, True, False) -> True
         (False, False, True) -> False
         (False, False, False) -> True
    where
        horIsSame  = isSame r (c + dc)
        vertIsSame = isSame (r + dr) c
        diagIsSame = isSame (r + dr) (c + dc)
        isSame r' c'
            | (r' < rmin) || (r' > rmax) = False
            | (c' < cmin) || (c' > cmax) = False
            | otherwise = arr!(r,c) == arr!(r', c')
        ((rmin, cmin), (rmax, cmax)) = bounds arr
        (dr, dc) = case side of
                        TopLeft -> (-1, -1)
                        TopRight -> (-1, 1)
                        BottomLeft -> (1, -1)
                        BottomRight -> (1, 1)
