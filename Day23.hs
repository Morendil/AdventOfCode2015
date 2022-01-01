import Data.List.HT
import Data.Tuple.Extra (first, second)

type State = ((Int, Int), Int)

step :: [[String]] -> State -> State
step instructions state = execute (instructions !! snd state) state

switch :: Char -> (Int -> Int) -> (Int, Int) -> (Int, Int)
switch 'a' = first
switch 'b' = second
switch _ = error "Bad register"

pick :: Char -> (Int, Int) -> Int
pick 'a' = fst
pick 'b' = snd
pick _ = error "Bad register"

readi :: String -> Int
readi ('+':rest) = read rest
readi s = read s

execute :: [String] -> State -> State
execute ["hlf", r:_] (regs, pc) = (switch r (`div` 2) regs, pc+1)
execute ["tpl", r:_] (regs, pc) = (switch r (*3) regs, pc+1)
execute ["inc", r:_] (regs, pc) = (switch r succ regs, pc+1)
execute ["jmp", arg] (regs, pc) = (regs, pc+readi arg)
execute ["jie", r:_, arg] (regs, pc) = if even (pick r regs) then (regs, pc + readi arg) else (regs, pc+1)
execute ["jio", r:_, arg] (regs, pc) = if pick r regs == 1 then (regs, pc + readi arg) else (regs, pc+1)
execute i _ = error $ "Bad instruction " ++ show i

solve1 :: [[String]] -> Int 
solve1 instructions = solve instructions ((0,0),0)

solve2 :: [[String]] -> Int 
solve2 instructions = solve instructions ((1,0),0)

solve :: [[String]] -> State -> Int 
solve instructions = snd . fst . last . takeUntil ((>=length instructions).snd) . iterate (step instructions)

main = do
    instructions <- map words . lines <$> readFile "day23.txt"
    print $ solve1 instructions
    print $ solve2 instructions