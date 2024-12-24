import Data.Array;
import Data.Maybe;

import LibAdvent;

type Cell = (Int, Int)
type Puzzle = Array (Int, Int) Char

main :: IO ()
main = do
    contents <- readFile "day20.txt"
    let puzzle = toCharMatrix contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

locateTrack :: Puzzle -> [Cell]
locateTrack puzzle = start:scan start first
    where
        start = one [ ix | (ix, v) <- assocs puzzle, v == 'S' ]
        end = one [ ix | (ix, v) <- assocs puzzle, v == 'E' ]
        adjacent ix = [ ix2 | ix2 <- adjacentCells ix, puzzle!ix2 /= '#' ]
        first = one (adjacent start)
        scan _ curr | curr == end = [end]
        scan prev curr = curr:scan curr (next prev curr)
        next prev curr = one [ ix | ix <- adjacent curr, ix /= prev ]

solvePartOne :: Puzzle -> Int
solvePartOne = solve 2

solvePartTwo :: Puzzle -> Int
solvePartTwo = solve 20

solve :: Int -> Puzzle -> Int
solve maxCheat puzzle = length cheats
    where
        track = zip (locateTrack puzzle) [0..]
        scores = array (bounds puzzle) $ 
            [ ((r, c), Nothing) | r <- [rmin..rmax], c <-[cmin..cmax] ] ++ 
            [ (ix, Just s) | (ix, s) <- track ]
        cheats = [ () |
            ((startRow, startCol), startScore) <- track,
            (dr, dc) <- offsets,
            let endRow = startRow + dr, endRow >= rmin, endRow <= rmax,
            let endCol = startCol + dc, endCol >= cmin, endCol <= cmax,
            let ms = scores ! (endRow, endCol), isJust ms,
            let endScore = fromJust ms,
            let cheatScore = abs dr + abs dc,
            let fairScore = endScore - startScore,
            let improvement = fairScore - cheatScore,
            improvement >= 100 ]
        offsets = [ (dr, dc) | dr <- [(-maxCheat)..maxCheat],
                               dc <- [(-maxCheat)..maxCheat],
                               abs dr + abs dc >= 2,
                               abs dr + abs dc <= maxCheat ]
        ((rmin, cmin), (rmax, cmax)) = bounds puzzle

