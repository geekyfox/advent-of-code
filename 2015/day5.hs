import Data.List

main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let words = lines contents
    putStrLn $ "Part one : " ++ show (solvePartOne words)
    putStrLn $ "Part two : " ++ show (solvePartTwo words)

solvePartOne :: [String] -> Int
solvePartOne = length . filter isNice
    where
        isNice x = (ruleA x) && (ruleB x) && (ruleC x)

ruleA :: String -> Bool
ruleA x = (length $ filter isVowel x) >= 3
    where
        isVowel x = or [ x == c | c <- "aeiou" ]

ruleB :: String -> Bool
ruleB xs = or $ zipWith (==) xs (tail xs)

ruleC :: String -> Bool
ruleC xs = and $ zipWith f xs (tail xs)
    where
        f 'a' 'b' = False
        f 'c' 'd' = False
        f 'p' 'q' = False
        f 'x' 'y' = False
        f _ _ = True

solvePartTwo :: [String] -> Int
solvePartTwo = length . filter isNice
    where
        isNice x = (ruleD x) && (ruleE x)

ruleD :: String -> Bool
ruleD (a:b:bs) =
    if [a, b] `isInfixOf` bs
        then True
        else ruleD (b:bs)
ruleD _ = False

ruleE :: String -> Bool
ruleE xs = or $ zipWith (==) xs (drop 2 xs)

