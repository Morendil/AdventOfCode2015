step :: [String] -> [String]
step grid = [[rule (x,y) | x <- [0..width-1]] | y <- [0..height-1]]
  where rule pos = case at grid pos of
            '#' -> if near pos `elem` [2,3] then '#' else '.'
            '.' -> if near pos == 3 then '#' else '.'
            _ -> error "Bad grid"
        near pos = length $ filter (== '#') $ map (at grid) $ neighbours pos
        neighbours pos = filter (inGrid width height) (from pos)
        height = length grid
        width = length (head grid)

add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
offsets = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]
from pos = map (add pos) offsets
inGrid w h (x,y) = x >=0 && y >= 0 && x < w && y < h

at :: [[Char]] -> (Int, Int) -> Char
at tiles (x,y) = (tiles !! y) !! x

adjust :: [String] -> [String]
adjust grid = [adjusted begin] ++ middle grid ++ [adjusted end]
    where adjusted s = "#" ++ middle s ++ "#"
          middle l = take (length l-2) (tail l)
          begin = head grid
          end = last grid

solve1 :: [String] -> Int
solve1 = length . filter (=='#') . concat . last . take 101 . iterate step

solve2 :: [String] -> Int
solve2 = length . filter (=='#') . concat . last . take 101 . iterate (adjust.step.adjust)

main = do
    grid <- lines <$> readFile "day18.txt"
    print $ solve1 grid
    print $ solve2 grid