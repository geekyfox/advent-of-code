import qualified Data.Map as M;
import Prelude hiding (Left, Right);
import Data.Maybe;

main :: IO ()
main = do
    contents <- readFile "day15.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solve puzzle)
    let puzzle = parse (explode contents)
    putStrLn $ "Part two : " ++ show (solve puzzle)

type Place = (Int, Int)
data Item = Wall | Box | Empty | Robot | LeftHalf | RightHalf deriving (Show, Eq)
data Side = Up | Down | Left | Right deriving Show
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

charToSide :: Char -> Side
charToSide '^' = Up
charToSide 'v' = Down
charToSide '<' = Left
charToSide '>' = Right

shift :: Side -> Place -> Place
shift Up (r, c) = (r - 1, c)
shift Down (r, c) = (r + 1, c)
shift Left (r, c) = (r, c - 1)
shift Right (r, c) = (r, c + 1)

push :: Place -> Side -> Map -> Maybe Map
push p s m =
    case (get pp, s) of
         (Wall, _) -> Nothing
         (Empty, _) -> Just $ set m [(p, Empty), (pp, get p)]
         (Box, _) -> mm >>= retry
         (_, Left) -> mm >>= retry
         (_, Right) -> mm >>= retry
         (LeftHalf, _) -> mm >>= push (shift Right pp) s >>= retry
         (RightHalf, _) -> mm >>= push (shift Left pp) s >>= retry
    where
        get ix = fromJust $ M.lookup ix m
        set x [] = x
        set x ((ix, v):xs) = set (M.insert ix v x) xs
        pp = shift s p
        mm = push pp s m
        retry = push p s

solve :: Puzzle -> Int
solve (theMap, robot, moves) = 
    sum [ (r-1) * 100 + (c - 1) |
          ((r,c), x) <- M.assocs finalMap,
          x == Box || x == LeftHalf ]
    where
        finalMap = fst $ foldl next (theMap, robot) moves
        next (m, p) s = case push p s m of
                             Just mm -> (mm, shift s p)
                             Nothing -> (m, p)

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
