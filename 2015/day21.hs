import Data.List(sort)
import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day21.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Stats = (Int, Int, Int)
data Winner = Us | Them deriving Eq

parse :: String -> Stats
parse contents = (h, d, a)
    where
        [h, d, a] = map f $ lines contents
        f = read . snd . splitOnce ':'

fight :: Stats -> Stats -> Winner
fight (ph, pd, pa) (bh, bd, ba)
        | bt <= pt = Us
        | otherwise = Them
    where
        bt = (bh - 1) `div` max 1 (pd - ba)
        pt = (ph - 1) `div` max 1 (bd - pa)

solvePartOne :: Stats -> Int
solvePartOne boss = minimum $
     [ cost | (cost, player) <- options
              , fight player boss == Us ]

solvePartTwo :: Stats -> Int
solvePartTwo boss = maximum $
     [ cost | (cost, player) <- options
              , fight player boss == Them ]

options :: [(Int, Stats)]
options = [ (c, (100, d, a)) | (wc, wd) <- weapons
                             , (ac, aa) <- armorCombos
                             , (rc, rd, ra) <- ringCombos
                             , let c = wc + ac + rc
                             , let d = wd + rd
                             , let a = aa + ra ]

weapons :: [(Int, Int)]
weapons = [(8, 4), (10, 5), (25, 6), (40, 7), (74, 8)]

armorCombos :: [(Int, Int)]
armorCombos = (0, 0) : armors

armors :: [(Int, Int)]
armors = [(13, 1), (31, 2), (53, 3), (75, 4), (102, 5)]

ringCombos :: [(Int, Int, Int)]
ringCombos = [(0, 0, 0)] ++ rings ++ [
    (c, d, a) | (c1, d1, a1) <- rings
              , (c2, d2, a2) <- rings
              , c1 /= c2
              , let c = c1 + c2
              , let d = d1 + d2
              , let a = a1 + a2 ]

rings :: [(Int, Int, Int)]
rings = [
    (25, 1, 0), (50, 2, 0), (100, 3, 0),
    (20, 0, 1), (40, 0, 2), (80, 0, 3) ]

