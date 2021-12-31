sigma = (map sigma' [0..] !!)
    where sigma' 1 = 1
          sigma' 2 = 3
          sigma' n = sum $ zipWith (*) signs (map (term n) $ recur n)
          signs = cycle [1,1,-1,-1]
          term n 0 = n
          term n m = sigma m
          recur n = takeWhile (>=0) (map ((n-).penta) $ ranks 1)
          penta n = (3*(n^2)-n)`div`2
          ranks m = m:(-m):ranks (m+1)

solve1 :: Int -> Int
solve1 target = head $ filter ((>=target).(*10).sigma) [1..]

main = do
    print $ solve1 33100000