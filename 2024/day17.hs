import Data.Array
import Data.Maybe
import Data.Bits(xor)
import Data.List
import Numeric
import Data.Char
import qualified Data.Set as S
import Debug.Trace

main :: IO ()
main = do
    contents <- readFile "day17.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ solvePartOne puzzle
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

data State = State {
    regA   :: Int,
    regB   :: Int,
    regC   :: Int,
    regPC  :: Int,
    rom    :: Array Int Int,
    result :: [Int]
} deriving Show

parse :: String -> State
parse contents = State {
    regA   = lineToNum p,
    regB   = lineToNum q,
    regC   = lineToNum t,
    regPC  = 0,
    rom    = listArray (0, length ds - 1) ds,
    result = []
}
    where
        lineToNum = read . last . split ' '
        [p, q, t, "", r] = lines contents
        ds = map read $ split ',' $ last $ split ' ' r

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce sep xs = f [] xs
    where
        f acc [] = (reverse acc, [])
        f acc (x:xs)
            | x == sep = (reverse acc, xs)
            | otherwise = f (x:acc) xs

split :: Eq a => a -> [a] -> [[a]]
split sep xs =
    case splitOnce sep xs of
         (x, []) -> [x]
         (x, xs') -> x : split sep xs'

solvePartOne :: State -> String
solvePartOne = intercalate "," . map show . execute

execute :: State -> [Int]
execute s =
    case executeOne s of
         Nothing -> result s
         Just s' -> execute s'

readROM :: State -> Int -> Maybe Int
readROM st offset
        | (ix < ixmin) || (ix > ixmax) = Nothing
        | otherwise = Just $ r!ix
    where
        r = rom st
        (ixmin, ixmax) = bounds r
        ix = regPC st + offset

executeOne :: State -> Maybe State
executeOne state = do
        ins <- readROM state 0
        arg <- readROM state 1
        Just $ eval ins arg
    where
        st = state { regPC = regPC state + 2 }
        a = regA st
        b = regB st
        c = regC st
        combo n | (n >= 0) && (n <= 3) = n
        combo 4 = a
        combo 5 = b
        combo 6 = c
        eval 0 n = st { regA = a `div` (2 ^ combo n) }
        eval 1 n = st { regB = b `xor` n }
        eval 2 n = st { regB = combo n `mod` 8 }
        eval 3 n | a == 0 = st
        eval 3 n = st { regPC = n }
        eval 4 _ = st { regB = b `xor` c }
        eval 5 n = st { result = result st ++ [combo n `mod` 8] }
        eval 7 n = st { regC = a `div` (2 ^ combo n) }

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

solvePartTwo :: State -> Int
solvePartTwo st = seek 1 4 [0..3]
    where
        theRom = rom st
        output = elems theRom
        outlen = length output
        seek plen pow mods
            | plen + 3 >= outlen = bruteforce pow mods
            | otherwise = bisect plen pow mods
        bruteforce pow mods = head $
            [ n | (n, r) <- stream pow mods,
                  r == output ]
        stream pow mods =
            [ (n, res) | i <- [0..], j <- mods,
                         let n = i * pow + j,
                         let res = try n ]
        try n = execute State {
            regA = n, regB = 0, regC = 0,
            regPC = 0, rom = theRom, result = []
        }
        bisect plen pow mods =
            let prefix = take plen output
                sols = take 32 [ n | (n, r) <- stream pow mods,
                                     prefix `isPrefixOf` r ]
                mods' = unique [ n `mod` (pow * 2) | n <- sols ]
            in if length mods' > 8
                    then seek (plen + 1) (pow * 2) mods'
                    else seek plen (pow * 2) mods'
