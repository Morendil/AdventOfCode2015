import Data.List
import Data.Ord

solve1 :: [Int] -> Int
solve1 = length . filter ((==150).sum) . powerset

solve2 :: [Int] -> Int
solve2 containers = length $ filter ((==best).length) sorted
    where combos = filter ((==150).sum) $ powerset containers
          sorted = sortOn length combos
          best = length $ head sorted

powerset :: [Int] -> [[Int]]
powerset = map concat . mapM (\item -> [[],[item]])

main = do
    containers <- map read . lines <$> readFile "day17.txt"
    print $ solve1 containers
    print $ solve2 containers