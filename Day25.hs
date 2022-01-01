import Math.NumberTheory.Powers.Modular

solve :: (Int, Int) -> Integer
solve (row, col) = (20151125 * powMod 252533 (zig (row,col)-1) 33554393) `mod` 33554393

tri n = n * (n+1) `div` 2
zig (row, col) = tri (row+col-2)+col

main = do
    print $ solve (2947, 3029)