import Text.ParserCombinators.ReadP
    ( ReadP, choice, many1, readP_to_S, satisfy, string )
import Data.Word ( Word16 )
import Data.Bits ( Bits(shift, complement, (.|.), (.&.)) )
import Data.Char ( isNumber, isAlpha )
import Data.Maybe ( fromJust )
import qualified Data.Map as M
import Control.Monad.Trans.State ( State, get, put, runState )
import Control.Monad.Extra

data Input = Constant Word16 | Not String | Or String String | DAnd Word16 String | And String String | LShift String Word16 | RShift String Word16 | Pass String
    deriving (Eq, Show)
type Instruction = (String, Input)
type Booklet = M.Map String Input
type Values = M.Map String Word16

eval :: Booklet -> String -> State Values Word16
eval booklet wire = do
    let cached = M.lookup wire <$> get
        compute = do
            result <- apply booklet (fromJust $ M.lookup wire booklet)
            values <- get
            put $ M.insert wire result values
            return result
    maybeM compute return cached

apply :: Booklet -> Input -> State Values Word16
apply booklet (Constant val) = return val
apply booklet (Not wire) = complement <$> eval booklet wire
apply booklet (Or w1 w2) = (.|.) <$> eval booklet w1 <*> eval booklet w2
apply booklet (And w1 w2) = (.&.) <$> eval booklet w1 <*> eval booklet w2
apply booklet (DAnd v wire) = (v .&.) <$> eval booklet wire
apply booklet (Pass wire) = eval booklet wire
apply booklet (LShift wire v) = shift <$> eval booklet wire <*> pure (fromIntegral v)
apply booklet (RShift wire v) = shift <$> eval booklet wire <*> pure (negate $ fromIntegral v)

instruction :: ReadP Instruction
instruction = do
    inp <- input
    string " -> "
    output <- many1 (satisfy isAlpha)
    return (output, inp)

input :: ReadP Input
input = choice [
        Constant <$> number,
        Pass <$> identifier,
        Not <$> (string "NOT " *> identifier),
        Or <$> identifier <*> (string " OR " *> identifier),
        And <$> identifier <*> (string " AND " *> identifier),
        DAnd <$> number <*> (string " AND " *> identifier),
        LShift <$> identifier <*> (string " LSHIFT " *> number),
        RShift <$> identifier <*> (string " RSHIFT " *> number)]
    where identifier = many1 (satisfy isAlpha)
          number :: ReadP Word16
          number = read <$> many1 (satisfy isNumber)

parse :: ReadP a -> String ->  a
parse parser input =
    case reverse $ readP_to_S parser input of
        ((result, _):_) -> result
        _ -> error "No parse"

solve1 :: Booklet -> Int
solve1 booklet = fromIntegral $ fst result
    where result = runState (eval booklet "a") M.empty

main = do
    booklet <- M.fromList . map (parse instruction) . lines <$> readFile "day07.txt"
    let result1 = solve1 booklet
    print $ solve1 booklet
    print $ solve1 $ M.insert "b" (Constant $ fromIntegral result1) booklet