import Data.Maybe (catMaybes, isNothing)
type Player = (Int, Int, Int)
data Equipment = E (Int, Int, Int)
    deriving (Eq, Show)

wins :: Player -> Player -> Bool
wins (hp1,dmg1,arm1) (hp2,dmg2,arm2) = dpt1 == 0 || (dpt2 > 0 && (hp1 `div` dpt1) >= (hp2 `div` dpt2))
    where dpt1 = max 0 (dmg2-arm1)
          dpt2 = max 0 (dmg1-arm2)


outfits :: [[Equipment]]
outfits = [map E $ catMaybes [weapon,armor,ring1,ring2] | weapon <- one weapons, armor <- some armor, ring1 <- some rings, ring2 <- some rings, different ring1 ring2]
    where one l = map Just l
          some l = Nothing:map Just l
          different e1 e2 = e1 /= e2 || isNothing e1
          weapons = [(8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0)]
          armor = [(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,5)]
          rings = [(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3)]

cost :: Equipment -> Int
cost (E (c,_,_)) = c

equip :: Player -> Equipment -> Player
equip (hp,dmg,arm) (E (_,dmg',arm')) = (hp,dmg+dmg',arm+arm')

equipAll :: Player -> [Equipment] -> Player
equipAll p = foldl equip p

solve1 :: Player -> Int
solve1 boss = minimum $ map (sum . map cost) $ filter (\outfit -> wins (equipAll naked outfit) boss) outfits
    where naked = (100,0,0)

solve2 :: Player -> Int
solve2 boss = maximum $ map (sum . map cost) $ filter (\outfit -> not $ wins (equipAll naked outfit) boss) outfits
    where naked = (100,0,0)

main = do
    let boss = (100,8,2)
    print $ solve1 boss
    print $ solve2 boss