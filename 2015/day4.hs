import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Crypto.Hash.MD5 as MD5

main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let prefix = head (lines contents)
    putStrLn $ "Part one : " ++ show (solvePartOne prefix)
    putStrLn $ "Part two : " ++ show (solvePartTwo prefix)

solvePartOne :: String -> Int
solvePartOne = solve "00000"

solvePartTwo :: String -> Int
solvePartTwo = solve "000000"

solve :: String -> String -> Int
solve digestPrefix payloadPrefix = head matches
    where
        matches = [ n | n <- [1..],
                        let payload = payloadPrefix ++ (show n),
                        let digest = md5Hash payload,
                        digestPrefix `isPrefixOf` digest ]

md5Hash :: String -> String
md5Hash = unpack . MD5.hash . pack
    where
        pack = C.toStrict . C.pack
        unpack = C.unpack . toLazyByteString . byteStringHex

