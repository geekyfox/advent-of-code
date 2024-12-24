import qualified Data.Map as M
import Data.List(isPrefixOf)
import Debug.Trace

import LibAdvent
import LibDyn

main :: IO ()
main = do
    contents <- readFile "day19.txt"
    let (towels, designs) = parse contents
    let ways = computeWays towels designs
    --
    let partOne = length [ w | w <- ways, w > 0 ]
    putStrLn $ "Part one : " ++ show partOne
    --
    let partTwo = sum ways
    putStrLn $ "Part two : " ++ show partTwo

parse :: String -> ([String], [String])
parse content = (parseFirst first, rest)
    where
        (first:"":rest) = lines content
        parseFirst = map strip . split ','
        strip (' ':xs) = xs
        strip xs = xs

computeWays :: [String] -> [String] -> [Integer]
computeWays towels = fst . getMany (wrap count)
    where
        count "" = return 1
        count d = do
            let cs = chop d
            vs <- dynMany cs
            return $ sum vs
        chop d = [ drop n d | (n, p) <- prefixes, p `isPrefixOf` d ]
        prefixes = [ (length t, t) | t <- towels ]

