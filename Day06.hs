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

apply :: IOArray (Int,Int) Bool -> Instruction -> IO (IOArray (Int,Int) Bool)
apply arr (On,((xmin,ymin),(xmax,ymax))) = do
    sequence_ [writeArray arr (x,y) True | x <- [xmin..xmax], y <- [ymin..ymax]]
    return arr
apply arr (Off,((xmin,ymin),(xmax,ymax))) = do
    sequence_ [writeArray arr (x,y) False | x <- [xmin..xmax], y <- [ymin..ymax]]
    return arr
apply arr (Toggle,((xmin,ymin),(xmax,ymax))) = do
    sequence_ [toggle arr (x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]
    return arr
        where toggle :: IOArray (Int,Int) Bool -> (Int, Int) -> IO ()
              toggle arr coords = do
                val <- readArray arr coords
                writeArray arr coords (not val)

main = do
    instructions <- map instruction . lines <$> readFile "day06.txt"
    lights <- newArray ((0,0),(999,999)) False :: IO (IOArray (Int,Int) Bool)
    final <- foldlM apply lights instructions
    elems <- getElems final
    print $ length $ filter id elems