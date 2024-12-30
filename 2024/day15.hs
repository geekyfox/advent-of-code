import qualified Data.Map as M
import Data.Maybe

import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day15.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solve puzzle)
    let puzzle = parse (explode contents)
    putStrLn $ "Part two : " ++ show (solve puzzle)

type Place = (Int, Int)
data Item = Wall | Box | Empty | Robot | LeftHalf | RightHalf
    deriving (Show, Eq)
type Map = M.Map Place Item
type Puzzle = (Map, Place, [Side])

parse :: String -> Puzzle
parse contents = (theMap, robot, moves)
    where
        robot = head [ ix | (ix, x) <- M.assocs theMap, x == Robot ]
        theMap = M.fromList [ ((r, c), charToItem x) | (r, c, x) <- mapChars ]
        moves = map charToSide $ concat movesLines
        allLines = lines contents
        mapLines = takeWhile (not . null) allLines
        mapChars = [ (r, c, x) | (r, xs) <- zip [1..] mapLines
                               , (c, x) <- zip [1..] xs ]
        movesLines = tail $ dropWhile (not . null) allLines

charToItem :: Char -> Item
charToItem '#' = Wall
charToItem 'O' = Box
charToItem '.' = Empty
charToItem '@' = Robot
charToItem '[' = LeftHalf
charToItem ']' = RightHalf
charToItem x = error $ "Don't know how to charToItem " ++ [x]

push :: Place -> Side -> Map -> Maybe Map
push from dir map =
    case (map !!! to, dir) of
         (Wall, _) -> Nothing
         (Empty, _) -> Just swap
         (Box, _) -> map2 >>= retry
         (_, West) -> map2 >>= retry
         (_, East) -> map2 >>= retry
         (LeftHalf, _) -> map2 >>= push (shift East to) dir >>= retry
         (RightHalf, _) -> map2 >>= push (shift West to) dir >>= retry
    where
        swap = insertMany map [(from, map !!! to), (to, map !!! from)]
        to = shift dir from
        map2 = push to dir map
        retry = push from dir

pushMany :: Map -> Place -> [Side] -> Map
pushMany map _ [] = map
pushMany map robot (dir:dirs) =
    case push robot dir map of
         Just map2 -> pushMany map2 (shift dir robot) dirs
         Nothing -> pushMany map robot dirs

solve :: Puzzle -> Int
solve (theMap, robot, moves) = sum indices
    where
        indices = [ index | ((r,c), x) <- M.assocs finalMap,
                            x == Box || x == LeftHalf,
                            let index = (r - 1) * 100 + (c - 1) ]
        finalMap = pushMany theMap robot moves

explode :: String -> String
explode contents = unlines $ map explodeLine mapLines ++ movesLines
    where
        allLines = lines contents
        mapLines = takeWhile (not . null) allLines
        movesLines = dropWhile (not . null) allLines
        explodeLine = concatMap explodeChar
        explodeChar '#' = "##"
        explodeChar 'O' = "[]"
        explodeChar '.' = ".."
        explodeChar '@' = "@."
