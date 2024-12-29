import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

import LibAdvent
import LibDyn

main :: IO ()
main = do
    contents <- readFile "day24.txt"
    let puzzle = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne puzzle)
    putStrLn $ "Part two : " ++ show (solvePartTwo puzzle)

data Op = And | Or | Xor deriving (Show, Eq, Ord)
data Rule = Rule Op String String deriving (Show, Eq, Ord)
type Puzzle = ([(String, Bool)], [(String, Rule)])

parse :: String -> Puzzle
parse content = (inputs, wires)
    where
        (inputLines, wireLines) = splitOnce "" (lines content)
        inputs = map parseInput inputLines
        wires = map parseWire wireLines
        parseInput x = case split ' ' x of
            [a, "0"] -> (init a, False)
            [a, "1"] -> (init a, True)
        parseWire x = case split ' ' x of
            [a, "AND", b, "->", c] -> (c, Rule And a b)
            [a, "OR", b, "->", c] -> (c, Rule Or a b)
            [a, "XOR", b, "->", c] -> (c, Rule Xor a b)

solvePartOne :: Puzzle -> Integer
solvePartOne (inputs, wires) = fromBits $ evalMany f keys
    where
        rules = M.fromList $ [ (a, Left b) | (a, b) <- inputs ] ++
            [ (a, Right b) | (a, b) <- wires ]
        f x = case rules !!! x of
            Left v -> return v
            Right (Rule op y z) -> do
                a <- dyn y
                b <- dyn z
                return $ case op of
                    And -> a && b
                    Or -> a || b
                    Xor -> a /= b
        keys = sort [ a | (a, b) <- wires, head a == 'z' ]
        fromBits [] = 0
        fromBits (False:xs) = 2 * fromBits xs
        fromBits (True:xs) = 1 + 2 * fromBits xs

solvePartTwo :: Puzzle -> String
solvePartTwo (_, wires) = impl []
    where
        fmt swaps = intercalate "," $ sort $ concat [ [a,b] | (a,b) <- swaps ]
        impl swaps | length swaps == 4 = fmt swaps
        impl swaps =
            let
                table = prepare wires swaps
                optimalTable = optimize table
                swap = detectSwap optimalTable
            in
                impl (swap:swaps)

prepare :: [(String, Rule)] -> [(String, String)] -> [(String, String, Rule)]
prepare wires swaps = [ (k', k', r) | (k, r) <- wires, let k' = fix k ]
    where
        fix k = fixImpl k swaps
        fixImpl k [] = k
        fixImpl k ((a, b):bs)
            | k == a = b
            | k == b = a
            | otherwise = fixImpl k bs

optimize :: [(String, String, Rule)] -> [(String, String, Rule)]
optimize xs = case mapMaybe detectRename xs of
    ((from, to):_) -> optimize $ map (rename from to) xs
    [] -> sort xs

parseName :: String -> Maybe (Char, Int)
parseName [a, b, c]
    | isDigit b && isDigit c = Just (a, read [b, c])
    | otherwise = Nothing

rename :: String -> String -> (String, String, Rule) -> (String, String, Rule)
rename from to (x, y, Rule op z w) = (f x, y, Rule op (f z) (f w))
    where
        f x = if x == from then to else x

detectRename :: (String, String, Rule) -> Maybe (String, String)
detectRename (k, ok, Rule op x y) = do
    a <- parseName x
    b <- parseName y
    c <- canonicName op a b
    if isJust (parseName k) || (k == c)
        then Nothing
        else Just (k, c)

canonicName :: Op -> (Char, Int) -> (Char, Int) -> Maybe String
canonicName op (a, m) (b, n) | a > b = canonicName op (b, n) (a, m)
canonicName op (a, m) (b, n) = case (a, op, b) of
    ('x', Xor, 'y')
        | (m == 0) && (n == 0) -> Just "z00"
        | m == n -> Just $ mkName 'a' m
    ('x', And, 'y')
        | (m == 0) && (n == 0) -> Just "c00"
        | m == n -> Just $ mkName 'b' m
    ('a', Xor, 'c')
        | m == n + 1 -> Just $ mkName 'z' m
    ('a', And, 'c')
        | m == n + 1 -> Just $ mkName 'd' m
    ('b', Or, 'd')
        | m == n -> Just $ mkName 'c' m
    _ -> Nothing

mkName :: Char -> Int -> String
mkName x n | n < 10 = x:'0':show n
mkName x n = x:show n

detectSwap :: [(String, String, Rule)] -> (String, String)
detectSwap xs = result
    where
        outputs = filter isOutput xs
        isOutput (x, _, _) = head x == 'z'
        result = case mapMaybe detectAnomaly outputs of
            ((a, b):_) -> (originalName a, originalName b)
            [] -> error $ "I don't know what to do with " ++ show outputs
        originalName x | head x == 'z' = x
        originalName x | isNothing (parseName x) = x
        originalName x = case [ ok | (k, ok, _) <- xs, x == k ] of
            [v] -> v
            _ -> error $
                "I don't know how to find " ++ show x ++ " in " ++ show xs

detectAnomaly :: (String, String, Rule) -> Maybe (String, String)
detectAnomaly (k, ok, _) | k /= ok = Just (k, ok)
detectAnomaly (k, _, Rule Xor a b)
    | a /= expectA = Just (a, expectA)
    | b /= expectB = Just (b, expectB)
    where
        index = snd $ fromJust $ parseName k
        expectA = if a < b then expectP else expectQ
        expectB = if a < b then expectQ else expectP
        expectP = if index == 0 then "x00" else mkName 'a' index
        expectQ = if index == 0 then "y00" else mkName 'c' (index - 1)
detectAnomaly _ = Nothing

