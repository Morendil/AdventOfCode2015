import Data.Char (digitToInt)
import Data.List

expand :: String -> String
expand = concatMap (\g -> show (length g) ++ show (head g)) . group . map digitToInt

solve1 :: String -> Int
solve1 = solve 40

solve2 :: String -> Int
solve2 = solve 50

solve :: Int -> String -> Int
solve n = length . last . take (n+1) . iterate expand

main = do
    let initial = "3113322113"
    print $ solve1 initial
    print $ solve2 initial