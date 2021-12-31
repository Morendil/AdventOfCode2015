import Math.NumberTheory.ArithmeticFunctions ( divisorsList )

solve1 :: Int -> Int
solve1 target = head $ filter ((>=target).(*10).sum.divisorsList) [1..]

main = do
    print $ solve1 33100000