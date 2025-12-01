import Data.Char
import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day19.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

type Atom = String
type Molecule = [Atom]
type Rule = (Atom, Molecule)
type Problem = ([Rule], Molecule)

parse :: String -> Problem
parse contents = (rules, molecule)
    where
        entries = reverse $ lines contents
        molecule = atomize $ head entries
        rules = map parseRule $ drop 2 entries

parseRule :: String -> Rule
parseRule text = case split ' ' text of
                      [x, "=>", y] -> (x, atomize y)
                      _ -> error text

atomize :: String -> Molecule
atomize = scan ""
    where
        scan "" (x:xs) = scan [x] xs
        scan acc (x:xs)
            | isUpper x = acc : scan [x] xs
            | otherwise = scan (acc ++ [x]) xs
        scan acc "" = [acc]

solvePartOne :: Problem -> Int
solvePartOne (rules, molecule) = countUnique $ concatMap f rules
    where
        f = patch molecule

patch :: Molecule -> Rule -> [Molecule]
patch [] _ = []
patch (x:xs) (src, dst) = takes ++ skips
    where
        takes
            | x == src = [dst ++ xs]
            | otherwise = []
        skips = [ x:ys | ys <- patch xs (src, dst) ]

solvePartTwo :: Problem -> Int
solvePartTwo (rules, m) =
    case filter (not . ruleIsValid) rules of
         [] -> complexity m - complexity (atomize "e")
         xs -> error $ show xs

ruleIsValid :: Rule -> Bool
ruleIsValid (_, xs) = complexity xs == 2

complexity :: Molecule -> Int
complexity = sum . map f
    where
        f "Rn" = 0
        f "Ar" = 0
        f "Y"  = -1
        f _    = 1

