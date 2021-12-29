solve1 :: [String] -> Int
solve1 strings = sum (map length strings) - sum (map decodedLength strings)

decodedLength ('"':x:rest) = decodedLength (x:rest)
decodedLength ('\\':'\\':rest) = 1 + decodedLength rest
decodedLength ('\\':'"':rest) = 1 + decodedLength rest
decodedLength ('\\':'x':d1:d2:rest) = 1 + decodedLength rest
decodedLength "\"" = 0
decodedLength (x:rest) = 1 + decodedLength rest
decodedLength "" = 0

main = do
    strings <- lines <$> readFile "day08.txt"
    print $ solve1 strings