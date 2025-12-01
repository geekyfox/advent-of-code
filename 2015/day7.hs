import Data.Char
import Data.Bits
import qualified Data.Map as M

import LibAdvent
import LibDyn

main :: IO ()
main = do
    contents <- readFile "day7.txt"
    let wires = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne wires)
    putStrLn $ "Part two : " ++ show (solvePartTwo wires)

parse :: String -> M.Map String Rule
parse = M.fromList . map parseLine . lines

data Rule = And String String
          | Or String String
          | Not String
          | RShift String String
          | LShift String String
          | Const String
          deriving Show

parseLine :: String -> (String, Rule)
parseLine x = case split ' ' x of
    [a, "AND", b, "->", c] -> (c, And a b)
    [a, "OR", b, "->", c] -> (c, Or a b)
    ["NOT", a, "->", b] -> (b, Not a)
    [a, "RSHIFT", b, "->", c] -> (c, RShift a b)
    [a, "LSHIFT", b, "->", c] -> (c, LShift a b)
    [a, "->", b] -> (b, Const a)
    tokens -> error $ show tokens

solvePartOne :: M.Map String Rule -> Int
solvePartOne wires = eval f "a"
    where
        f a | all isNumber a = pure (read a)
        f a = case wires !!! a of
            (And p q) -> do
                x <- dyn p
                y <- dyn q
                return (x .&. y)
            (Or p q) -> do
                x <- dyn p
                y <- dyn q
                return (x .|. y)
            (Not p) -> do
                x <- dyn p
                return (x `xor` 65535)
            (RShift p q) -> do
                x <- dyn p
                y <- dyn q
                return (x `shiftR` y)
            (LShift p q) -> do
                x <- dyn p
                y <- dyn q
                return $ (x `shiftL` y) .&. 65535
            (Const p) -> dyn p

solvePartTwo :: M.Map String Rule -> Int
solvePartTwo wires = solvePartOne patched
    where
        patched = M.insert "b" (Const $ show $ solvePartOne wires) wires

