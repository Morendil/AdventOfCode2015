import Data.Char
import Data.List

strip (x:rest) | x == '-' || isNumber x = x:strip rest
strip (x:rest) = ' ':strip rest
strip "" = ""

ingredient :: String -> [Int]
ingredient = map read . words . strip

solve1 :: [[Int]] -> Int
solve1 ingredients = maximum $ map fst $ scoreAndCalories ingredients

solve2 :: [[Int]] -> Int
solve2 ingredients = maximum $ map fst $ filter ((==500).snd) $ scoreAndCalories ingredients

scoreAndCalories :: [[Int]] -> [(Int, Int)]
scoreAndCalories ingredients = map (\combo -> (score withoutCalories combo, sum $ zipWith (*) calories combo)) combos
    where nIngredients = length ingredients
          withoutCalories = map (take 4) ingredients
          calories = map last ingredients
          combos = combinations nIngredients

combinations :: Int -> [[Int]]
combinations n = go n 100
    where go 1 rest = [[rest]]
          go n total = concatMap (\m -> map (m:) (go (n-1) (total-m))) [0..total]

score :: [[Int]] -> [Int] -> Int
score ingredients weights = product $ map (max 0) $ foldr1 (zipWith (+)) $ zipWith (\weight -> map (*weight)) weights ingredients

main = do
    ingredients <- map ingredient . lines <$> readFile "day15_test.txt"
    print $ solve1 ingredients
    print $ solve2 ingredients
