import Data.List
import Data.Ord

main = do
    weights <- map (read :: String -> Int) . lines <$> readFile "day24.txt"
    let target1 = sum weights `div` 3
    print $ minimum $ map product $ [[a1, a2, a3, a4, a5, a6] |
       let a1 = 1,
       a2 <- weights,
       a2 /= a1,
       a3 <- weights,
       a3 /= a1,
       a3 /= a2,
       a4 <- weights,
       a4 /= a1,
       a4 /= a2,
       a4 /= a3,
       a5 <- weights,
       a5 /= a1,
       a5 /= a2,
       a5 /= a3,
       a5 /= a4,
       a6 <- weights,
       a6 /= a1,
       a6 /= a2,
       a6 /= a3,
       a6 /= a4,
       a6 /= a5,
       let arr = [a1, a2, a3, a4, a5, a6],
       sum arr == target1]
    let target2 = sum weights `div` 4
    print $ minimum $ map product $ [[a1, a2, a3, a4] |
       a1 <- weights,
       a2 <- weights,
       a2 /= a1,
       a3 <- weights,
       a3 /= a1,
       a3 /= a2,
       a4 <- weights,
       a4 /= a1,
       a4 /= a2,
       a4 /= a3,
       let arr = [a1, a2, a3, a4],
       sum arr == target2]
