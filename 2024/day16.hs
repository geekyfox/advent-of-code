import Data.Array
import qualified Data.Map as M
import Data.Maybe

import LibAdvent;

type Cell = (Int, Int)
type Score = Int
type Key = (Cell, Side)
type Scores = M.Map Key Score
type Wave = [Key]
type Puzzle = Array (Int, Int) Char
type State = (Scores, Wave)

main :: IO ()
main = do
    contents <- readFile "day16.txt"
    let puzzle = toCharMatrix contents
    let scores = fillScores puzzle

    let end = head [ ix | (ix, v) <- assocs puzzle, v == 'E' ]
    let partOne = minScore scores end
    putStrLn $ "Part one : " ++ show partOne

    let goodScores = M.fromList [ (ix, v) | (ix, v) <- M.assocs scores, v <= partOne ]
    let veryGoodScores = trimDeadEnds end scores
    let partTwo = countUnique [ fst ix | ix <- M.keys veryGoodScores ]
    putStrLn $ "Part two : " ++ show partTwo

fillScores :: Puzzle -> Scores
fillScores puzzle = impl zeroScore zeroWave
    where
        start = head [ ix | (ix, v) <- assocs puzzle, v == 'S' ]
        zeroScore = M.singleton (start, East) 0
        zeroWave = [(start, East)]
        impl s [] = s
        impl s (w:ws) =
            let ps = makePatches puzzle s w
            in impl (insertMany s ps) (ws ++ map fst ps)

makePatches :: Puzzle -> Scores -> Key -> [(Key, Score)]
makePatches puzzle scores key = filter (uncurry isGood) options
    where
        (cell, side) = key
        score = scores !!! key 
        options = [
            ((shift side cell, side), score + 1),
            ((cell, clockwise side), score + 1000),
            ((cell, counterClockwise side), score + 1000) ]
        isGood (c, _) _ | puzzle!c == '#' = False
        isGood k s = case M.lookup k scores of
                          Just x -> x > s
                          Nothing -> True

trimDeadEnds :: Cell -> Scores -> Scores
trimDeadEnds finish scores
        | null deadEnds = scores
        | otherwise = trimDeadEnds finish (deleteMany scores deadEnds)
    where
        deadEnds = map fst $ filter isDeadEnd $ M.assocs scores
        isDeadEnd ((cell, _), _) | cell == finish = False
        isDeadEnd ((cell, side), score) =
            case lookupCell scores (shift side cell) of
                 [] -> True
                 xs -> maximum xs <= score

lookupCell :: M.Map (Cell, Side) Int -> Cell -> [Int]
lookupCell scores cell = catMaybes [
    M.lookup (cell, side) scores | side <- allSides ]

minScore :: Scores -> Cell -> Int
minScore scores c = minimum $ lookupCell scores c

