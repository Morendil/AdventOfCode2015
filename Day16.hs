import Data.Char
import Data.Maybe

type Sue = (Int, [(String, Int)])
type Clue = (String, Int -> Bool)

strip (x:rest) | x == '-' || isNumber x = x:strip rest
strip (x:rest) = ' ':strip rest
strip "" = ""

-- Sue 1: cars: 9, akitas: 3, goldfish: 0
sue :: String -> Sue
sue s = (number, pairs)
    where parts = words s
          number = read $ strip $ parts !! 1
          pair n = (chop $ parts !! (2+(n*2)), read $ strip $ parts !! (3+(n*2)))
          chop s = take (length s - 1) s
          pairs = map pair [0..2]

couldMatch :: Sue -> (String, Int -> Bool) -> Bool
couldMatch (_, traits) (trait, fn) = isNothing found || fn (fromJust found)
    where found = lookup trait traits

clues1 :: [Clue]
clues1 = [("children",(==3)),("cats",(==7)),("samoyeds",(==2)),("pomeranians",(==3)),("akitas",(==0)),("vizslas",(==0)),("goldfish",(==5)),("trees",(==3)),("cars",(==2)),("perfumes",(==1))]

clues2 :: [Clue]
clues2 = [("children",(==3)),("cats",(>7)),("samoyeds",(==2)),("pomeranians",(<3)),("akitas",(==0)),("vizslas",(==0)),("goldfish",(<5)),("trees",(>3)),("cars",(==2)),("perfumes",(==1))]

solve :: [Sue] -> [Clue] -> Int
solve sues clues = if length remain > 1 then error "Ambiguous" else fst $ head remain
    where eliminate clue = filter (`couldMatch` clue)
          remain = foldr eliminate sues clues

main = do
    sues <- map sue . lines <$> readFile "day16.txt"
    print $ solve sues clues1
    print $ solve sues clues2