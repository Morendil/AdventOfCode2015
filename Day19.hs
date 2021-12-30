import Data.Text (Text, pack, unpack, splitOn, breakOnAll, drop, concat)
import Data.List

split :: String -> String -> [String]
split sep = map unpack . splitOn (pack sep) . pack

replacement :: String -> (String, String)
replacement s= (one, two)
    where [one, _, two] = words s

substitute :: String -> (String, String) -> [String]
substitute molecule (from, to) = map perform $ breakOnAll (pack from) (pack molecule)
    where perform :: (Text, Text) -> String
          perform (prefix, suffix) = unpack $ Data.Text.concat [prefix, pack to, Data.Text.drop (length from) suffix]

solve1 :: String -> [(String, String)] -> Int
solve1 molecule replacements = length $ nub $ concatMap (substitute molecule) replacements

main = do
    [machine, molecule] <- split "\n\n" <$> readFile "day19.txt"
    let replacements = map replacement $ lines machine
    print $ solve1 molecule replacements
