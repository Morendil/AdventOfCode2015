import Data.Text (unpack, pack, splitOn)
import Data.Array.IO
import Data.Foldable (foldlM)

type Pos = (Int, Int)
type Instruction = (Action, (Pos, Pos))
data Action = On | Off | Toggle
    deriving (Eq, Show)

instruction :: String -> Instruction
instruction s = (match action, (asPair p1, asPair p2)) -- "toggle 461,550 through 564,900"
    where parts = words s
          action = unwords (take (length parts-3) parts)
          [p1,_,p2] = drop (length parts-3) parts
          asPair p = read $ "("++p++")" :: (Int, Int)
          match "turn on" = On
          match "turn off" = Off
          match "toggle" = Toggle
          match _ = error "Bad action"

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

apply :: IOArray (Int,Int) Int -> Instruction -> IO (IOArray (Int,Int) Int)
apply arr (action,((xmin,ymin),(xmax,ymax))) = do
    sequence_ [perform action arr (x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]
    return arr
        where perform :: Action -> IOArray (Int,Int) Int -> (Int, Int) -> IO ()
              perform action arr coords = do
                val <- readArray arr coords
                writeArray arr coords (pick action val)
              pick On val = val + 1
              pick Off val = max 0 (val - 1)
              pick Toggle val = val + 2

main = do
    instructions <- map instruction . lines <$> readFile "day06.txt"
    lights <- newArray ((0,0),(999,999)) 0 :: IO (IOArray (Int,Int) Int)
    final <- foldlM apply lights instructions
    elems <- getElems final
    print $ sum elems