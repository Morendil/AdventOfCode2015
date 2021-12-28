import Data.List

isNice :: String -> Bool
isNice string = hasThreeVowels && repeats && noTaboo
    where hasThreeVowels = length (filter isVowel string) >= 3
          repeats = any (uncurry (==)) (zip string (tail string))
          noTaboo = (not.any (`isInfixOf` string)) ["ab", "cd", "pq", "xy"]

isNiceTwo :: String -> Bool
isNiceTwo string = nonOverlappingPairs && repeatsOneApart string
    where nonOverlappingPairs = any twoNonOverlapping pairIndices
          twoNonOverlapping pi = any (>=2) $ zipWith (-) (tail pi) pi
          pairIndices = map (`elemIndices` pairs) pairs
          pairs = zip string (tail string)

repeatsOneApart (x:y:z:rest) | x == z = True
repeatsOneApart (x:y:z:rest) = repeatsOneApart (y:z:rest)
repeatsOneApart _ = False

isVowel = flip elem "aeiou"

solve1 = length . filter isNice
solve2 = length . filter isNiceTwo

main = do
    strings <- lines <$> readFile "day05.txt"
    print $ solve1 strings
    print $ solve2 strings