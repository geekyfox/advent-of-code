import Data.Aeson
import Data.Aeson.KeyMap (KeyMap, elems)
import Data.ByteString.Builder (toLazyByteString, stringUtf8)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text, pack)
import Data.Vector (toList)

main :: IO ()
main = do
    contents <- readFile "day12.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

parse :: String -> Value
parse = fromJust  . decode . toLazyByteString . stringUtf8

solvePartOne :: Value -> Int
solvePartOne = solve (const False)

solvePartTwo :: Value -> Int
solvePartTwo = solve hasRed

solve :: (KeyMap Value -> Bool) -> Value -> Int
solve shouldSkip = scan
    where scan v = case v of
                        Object obj | shouldSkip obj -> 0
                        Object obj -> sum $ map scan $ elems obj
                        Array arr  -> sum $ map scan $ toList arr
                        String _   -> 0
                        Number n   -> fromJust $ toBoundedInteger n
                        _ -> error $ take 50 $ show v

hasRed :: KeyMap Value -> Bool
hasRed = any isRed . elems

isRed :: Value -> Bool
isRed (String s) = s == red
isRed _ = False

red :: Text
red = pack "red"

