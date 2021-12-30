import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Ord
import Data.Tuple (swap)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

replacement :: Text -> (Text, Text)
replacement s= (one, two)
    where [one, _, two] = T.words s

substitute :: Text -> (Text, Text) -> [Text]
substitute molecule (from, to) = map perform $ T.breakOnAll from molecule
    where perform :: (Text, Text) -> Text
          perform (prefix, suffix) = T.concat [prefix, to, T.drop (T.length from) suffix]

solve1 :: Text -> [(Text, Text)] -> Int
solve1 molecule replacements = length $ nub $ concatMap (substitute molecule) replacements

-- greedy reduction - works only if the input is non-adversarial !
reduce :: [(Text, Text)] -> (Int, Text) -> (Int, Text)
reduce replacements s = foldl countSteps s replacements
    where countSteps (n, s) (from, to) = (n+nSteps, T.replace from to s)
            where nSteps = length $ T.breakOnAll from s

solve2 :: Text -> [(Text, Text)] -> Int
solve2 molecule replacements = fst $ converge (reduce (map swap replacements)) (0, molecule)

main = do
    [machine, molecule] <- T.splitOn (T.pack "\n\n") . T.pack <$> readFile "day19.txt"
    let replacements = map replacement $ T.lines machine
    print $ solve1 molecule replacements
    print $ solve2 molecule replacements
