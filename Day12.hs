import Text.ParserCombinators.ReadP
import Data.Char

data Item = Number Int | Label String | Array [Item] | Object [(String, Item)]
    deriving (Eq, Show)

sumAll :: Item -> Int
sumAll (Number n) = n
sumAll (Label _) = 0
sumAll (Array items) = sum $ map sumAll items
sumAll (Object pairs) | any ((==Label"red").snd) pairs = 0
sumAll (Object pairs) = sum $ map (sumAll.snd) pairs

item :: ReadP Item
item = choice [number, label, array, object]

number :: ReadP Item
number = Number . read <$> many1 (satisfy (\c -> isNumber c || c == '-'))

label :: ReadP Item
label = Label <$> between (string "\"") (string "\"") (many1 (satisfy isAlpha))

array :: ReadP Item
array = Array <$> between (string "[") (string "]") (sepBy1 item (string ","))

object = Object <$> between (string "{") (string "}") (sepBy1 pair (string ","))
    where pair = (,) <$> between (string "\"") (string "\"") (many1 (satisfy isAlpha)) <*> (string ":" *> item)

parse :: ReadP a -> String ->  a
parse parser input =
    case reverse $ readP_to_S parser input of
        ((result, _):_) -> result
        _ -> error "No parse"

solve1 :: String -> Int
solve1 = sum . map read . words . strip

solve2 :: String -> Int
solve2 = sumAll . parse item

strip :: String -> String
strip (x:rest) | x == '-' || isNumber x = x:strip rest
strip (x:rest) = ' ':strip rest
strip "" = ""

main = do
    json <- readFile "day12.txt"
    print $ solve1 json
    print $ solve2 json