import qualified Data.Map as M
import Data.Tuple.Extra
import Data.Maybe

type Pos = (Int, Int)
type Presents = M.Map (Int, Int) Int
type State = (Pos, Presents)
type StateTwo = ((Int, (Pos, Pos)), Presents)

deliver :: State -> Char -> State
deliver (pos, prev) dir = (pos', M.alter (Just . succ . fromMaybe 0) pos' prev)
    where pos' = which what pos
          (which, what) = interpret dir

deliverTwo :: StateTwo -> Char -> StateTwo
deliverTwo ((turn,pos), prev) dir = ((turn', pos'), M.alter (Just . succ . fromMaybe 0) (readWhichPos pos') prev)
    where pos' = changeWhichPos (which what) pos
          turn' = turn+1
          (which, what) = interpret dir
          changeWhichPos = if even turn then first else second
          readWhichPos = if even turn then fst else snd

interpret '>' = (first, succ)
interpret '<' = (first, pred)
interpret 'v' = (second, succ)
interpret '^' = (second, pred)          
interpret _ = error "Direction"

solve1 :: String -> Int 
solve1 = length . M.elems . snd . foldl deliver initial
    where initial = ((0,0), M.insert (0,0) 1 M.empty)

solve2 :: String -> Int 
solve2 = length . M.elems . snd . foldl deliverTwo initial
    where initial = ((0,((0,0),(0,0))), M.insert (0,0) 1 M.empty)

main = do
    moves <- readFile "day03.txt"
    print $ solve1 moves
    print $ solve2 moves
