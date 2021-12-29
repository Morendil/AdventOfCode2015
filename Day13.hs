import Data.List
import Data.Maybe
import Data.List.Extra (chunksOf)

solve1 :: [Int] -> Int
solve1 = maximum . allEffects

solve2 :: [Int] -> Int
solve2 = maximum . allEffects'

allEffects :: [Int] -> [Int]
allEffects effects = map impact $ permutations [0..n]
    where impact path = oneWay path + oneWay (reverse path)
          oneWay path = sum $ zipWith (curry (fromJust . (`lookup` table))) path (tail $ cycle path)
          table = zip ([(x,y) | x <- [0..n], y <- [0..n], x /= y]) effects
          n = floor $ sqrt $ fromIntegral $ length effects

allEffects' :: [Int] -> [Int]
allEffects' effects = map impact $ permutations [0..n]
    where impact path = oneWay path + oneWay (reverse path)
          oneWay path = sum $ zipWith (curry (fromJust . (`lookup` table))) path (tail path)
          table = zip ([(x,y) | x <- [0..n], y <- [0..n], x /= y]) effects
          n = floor $ sqrt $ fromIntegral $ length effects

effect :: String -> Int
effect s = sign * size
    where size = read $ parts !! 3
          sign = if parts !! 2 == "gain" then 1 else -1
          parts = words s

main = do
    effects <- map effect . lines <$> readFile "day13.txt"
    print $ solve1 effects
    print $ solve2 effects