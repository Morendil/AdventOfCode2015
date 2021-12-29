import Data.Char

strip (x:rest) | x == '-' || isNumber x = x:strip rest
strip (x:rest) = ' ':strip rest
strip "" = ""

reindeer :: String -> [Int]
reindeer = map read . words . strip

advance ::  Int -> [Int] -> Int
advance n [speed,duration,rest] = speed * ((times*duration) + min duration remainder)
    where (times, remainder) = n `divMod` (duration+rest)
advance n _ = error "Bad reindeer"

solve1 :: Int -> [[Int]] -> Int
solve1 n = maximum . map (advance n)

solve2 contestants = maximum $ map score contestants
    where best = map (`solve1` contestants) [1..2503]
          score contestant = length $ filter (\n -> advance n contestant == best !! (n-1)) [1..2503]

main = do
    contestants <- map reindeer . lines <$> readFile "day14.txt"
    print $ solve1 2503 contestants
    print $ solve2 contestants