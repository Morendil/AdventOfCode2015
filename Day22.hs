import Algorithm.Search
import Data.Maybe (mapMaybe, fromJust)

data State = State {playerHp :: Int, mana :: Int, bossHp :: Int, bossDamage :: Int, spells :: [Spell], spellCost :: Int}
    deriving (Eq, Show, Ord)
data Spell = Spell {cost :: Int, timer :: Int, effect :: Effect }
    deriving (Eq, Show, Ord)
data Effect = Missile | Drain | Shield | Poison | Recharge
    deriving (Eq, Show, Ord)

-- neighbours of a state: each non-active spell, or no spell, with effects from ongoing spells, pruning losing states
-- cost of a move: mana cost
-- heuristic which does not overestimate the remaining distance: remaining HP of boss, divided average HP for poison
-- goal: boss at 0 HP

allSpells = [Spell {cost=53, timer=0, effect=Missile},
             Spell {cost=73, timer=0, effect=Drain},
             Spell {cost=113, timer=6, effect=Shield},
             Spell {cost=173, timer=6, effect=Poison},
             Spell {cost=229, timer=5, effect=Recharge}]

simulate2 :: State -> Spell -> Maybe State
simulate2 state spell = if playerHp state <= 1 then Nothing else simulate state {playerHp = playerHp state -1 } spell

simulate :: State -> Spell -> Maybe State
simulate state spell = if duplicateSpell || mana endState < 0 || playerHp endState <= 0 then Nothing else Just endState
    where playerTurn = ageAndRetireSpells . deductSpellCost . applyCurrentSpells
          deductSpellCost s = s {mana = mana s - cost spell, spellCost = cost spell}
          applyCurrentSpells s = foldl apply s (map effect $ spells s)
          ageAndRetireSpells s = s {spells = filter ((>0).timer) $ map (\spell -> spell {timer=timer spell-1}) (spells s)}
          instantOrDelayed s = if timer spell == 0 then apply s (effect spell) else s {spells=spell:spells s}
          bossTurn = applyBossDamage . ageAndRetireSpells . applyCurrentSpells
          effectiveArmor s = if Shield `elem` map effect (spells s) then 7 else 0
          applyBossDamage s = if bossHp s <= 0 then s else s {playerHp = playerHp s - max 1 (bossDamage s - effectiveArmor s)}
          -- Needed to avoid spell duplication
          intermediate = playerTurn state
          duplicateSpell = effect spell `elem` map effect (spells intermediate)
          endState = bossTurn $ instantOrDelayed intermediate

apply :: State -> Effect -> State
apply state Missile = state {bossHp = bossHp state - 4}
apply state Drain = state {bossHp = bossHp state - 2, playerHp = playerHp state + 2}
apply state Shield = state -- deal with this in boss turn as it's not permanent
apply state Poison = state {bossHp = bossHp state - 3}
apply state Recharge = state {mana = mana state + 101}

neighbours :: State -> [State]
neighbours state = mapMaybe (simulate state) allSpells

neighbours2 :: State -> [State]
neighbours2 state = mapMaybe (simulate2 state) allSpells

moveCost :: State -> State -> Int
moveCost _ = spellCost

heuristic :: State -> Int
heuristic state = bossHp state `div` 4

goal :: State -> Bool
goal state = bossHp state <= 0

solve1 :: State -> Int
solve1 = fst . fromJust . aStar neighbours moveCost heuristic goal

solve2 :: State -> Int
solve2 = fst . fromJust . aStar neighbours2 moveCost heuristic goal

main = do
    let initial = State {playerHp = 50, mana = 500, bossHp = 71, bossDamage = 10, spells = [], spellCost = 0}
    -- let initial = State {playerHp = 10, mana = 250, bossHp = 14, bossDamage = 8, spells = [], spellCost = 0}
    print $ solve1 initial
    print $ solve2 initial