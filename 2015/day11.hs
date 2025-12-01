import Data.Char

main :: IO ()
main = do
    contents <- readFile "day11.txt"
    let problem = parse contents
    let partOne = solve problem
    putStrLn $ "Part one : " ++ partOne
    let partTwo = solve partOne
    putStrLn $ "Part two : " ++ partTwo

parse :: String -> String
parse = head . lines

solve :: String -> String
solve = f . nextWord
    where
        f x | valid x = x
        f x = f (nextWord x)

nextWord :: String -> String
nextWord (c:cs)
    | isConfusing c = nextChar c : replicate (length cs) 'a'
nextWord [c] = [nextChar c]
nextWord (x:xs)
        | head ys < head xs = nextChar x : ys
        | otherwise = x:ys
    where
        ys = nextWord xs

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c
        | isConfusing c' = nextChar c'
        | otherwise = c'
    where
        c' = chr $ ord c + 1

isConfusing :: Char -> Bool
isConfusing c = (c == 'l') || (c == 'i') || (c == 'o')

valid :: String -> Bool
valid xs = hasIncreasingTriple xs && hasTwoPairs xs

hasIncreasingTriple :: String -> Bool
hasIncreasingTriple = f . map ord
    where
        f (x:y:z:rest)
            | z - y /= 1 = f (z:rest)
            | y - x /= 1 = f (y:z:rest)
            | otherwise = True
        f _ = False

hasTwoPairs :: String -> Bool
hasTwoPairs (x:y:rest)
    | x == y = hasPair rest
    | otherwise = hasTwoPairs (y:rest)
hasTwoPairs _ = False

hasPair :: String -> Bool
hasPair (x:y:rest)
    | x == y = True
    | otherwise = hasPair (y:rest)
hasPair _ = False

