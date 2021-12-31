import Math.NumberTheory.ArithmeticFunctions ( divisorsList )

solve1 :: Int -> Int
solve1 target = head $ filter ((>=target).(*10).sum.divisorsList) [1..]

solve2 :: Int -> Int
solve2 target = head $ filter ((>=target).(*11).sum.modifiedDivisorsList) [1..]

modifiedDivisorsList n = filter (\d -> d*50 >= n) $ divisorsList n

main = do
    print $ solve1 33100000
    print $ solve2 33100000