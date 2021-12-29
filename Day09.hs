import Data.List
import Data.Maybe

allPairsWith :: (a -> a -> b) -> [a] -> [b]
allPairsWith fn l = [fn x y | (x:ys) <- tails l, y <- ys]

solve1 :: [Int] -> Int
solve1 = minimum . allDists

solve2 :: [Int] -> Int
solve2 = maximum . allDists

allDists :: [Int] -> [Int]
allDists dists = map pathLength $ permutations [0..7]
    where pathLength path = sum $ zipWith (curry (fromJust . (`lookup` table) . sortPair)) path (tail path)
          table = zip (allPairsWith (,) [0..7]) dists
          sortPair (a,b) = if a > b then (b,a) else (a,b)
 
main = do
    distances <- map (read . last . words) . lines <$> readFile "day09.txt"
    print $ solve1 distances
    print $ solve2 distances