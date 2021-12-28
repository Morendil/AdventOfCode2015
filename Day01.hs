import Data.List

opens l = length $ filter (=='(') l
closes l = length $ filter (==')') l
onFloor l = opens l - closes l

solve1 = onFloor
solve2 parens = length (takeWhile (>=0) $ map onFloor (inits parens))

main = do
    parens <- readFile "day01.txt"
    print $ solve1 parens
    print $ solve2 parens