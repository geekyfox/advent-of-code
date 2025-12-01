import Data.Array
import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day23.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

data Reg = RegA | RegB
    deriving Show

data OpCode = HLF Reg | TPL Reg | INC Reg
            | JMP Int | JIE Reg Int | JIO Reg Int
    deriving Show
type Problem = Array Int OpCode

data State = State {
    code :: Array Int OpCode,
    ax :: Int,
    bx :: Int,
    pc :: Int
}

parse :: String -> Problem
parse s = listArray (1, count) opcodes
    where
        opcodes = map parseLine $ lines s
        count = length opcodes

parseLine :: String -> OpCode
parseLine = f . split ' '
    where
        f ["tpl", r] = TPL (parseReg r)
        f ["hlf", r] = HLF (parseReg r)
        f ["inc", r] = INC (parseReg r)
        f ["jmp", off] = JMP (parseOffset off)
        f ["jie", r, off] = JIE (parseReg r) (parseOffset off)
        f ["jio", r, off] = JIO (parseReg r) (parseOffset off)
        f ts = error $ show ts

parseReg :: String -> Reg
parseReg "a" = RegA
parseReg "a," = RegA
parseReg "b" = RegB
parseReg "b," = RegB
parseReg x = error $ show ("parseReg", x)

parseOffset :: String -> Int
parseOffset ('+':xs) = read xs
parseOffset ('-':xs) = read xs * (-1)

solvePartOne :: Problem -> Int
solvePartOne p = run $ State { code = p, ax = 0, bx = 0, pc = 1 }

solvePartTwo :: Problem -> Int
solvePartTwo p = run $ State { code = p, ax = 1, bx = 0, pc = 1 }

run :: State -> Int
run st | (pc st < lo) || (pc st > hi) = bx st
    where
        (lo, hi) = bounds $ code st
run st = run $
    case code st ! pc st of
         JMP off -> jump st off
         JIO r off ->
            if get r == 1
                then jump st off
                else step st
         JIE r off ->
            if even (get r)
                then jump st off
                else step st
         INC r -> step $ update r (1 +)
         TPL r -> step $ update r (3 *)
         HLF r -> step $ update r (`div` 2)
    where
        jump s offset = s { pc = pc s + offset }
        step s = jump s 1
        get RegA = ax st
        get RegB = bx st
        update RegA f = st { ax = f (ax st) }
        update RegB f = st { bx = f (bx st) }

