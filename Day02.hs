import Data.Text (unpack, pack, splitOn)
import Data.List

allPairsWith :: (a -> a -> b) -> [a] -> [b]
allPairsWith fn l = [fn x y | (x:ys) <- tails l, y <- ys]

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

parcel :: String -> [Int]
parcel = map read . split "x"

paperArea :: [Int] -> Int
paperArea sides = base + extra
    where extra = minimum areas
          base = 2 * sum areas
          areas = allPairsWith (*) sides

ribbonLength :: [Int] -> Int
ribbonLength parcel = lengthAround + volume
    where volume = product parcel
          lengthAround = 2 * minimum (allPairsWith (+) parcel)

solve1 = sum . map paperArea
solve2 = sum . map ribbonLength

main = do
    parcels <- map parcel . lines <$> readFile "day02.txt"
    print $ solve1 parcels
    print $ solve2 parcels