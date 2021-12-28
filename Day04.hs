import Distribution.Utils.MD5
import Data.ByteString.Char8 (pack)

solve :: Int -> String -> Int
solve len key = head $ filter breaks [0..]
    where breaks n = take len (showMD5 $ md5 (pack $ key ++ show n)) == replicate len '0'

solve1 = solve 5
solve2 = solve 6

main = do
    print $ solve1 "bgvyzdsv"
    print $ solve2 "bgvyzdsv"    