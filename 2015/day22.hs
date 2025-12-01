import Data.List(sort)
import Data.Maybe(fromJust, isNothing, catMaybes, listToMaybe)
import LibAdvent

main :: IO ()
main = do
    contents <- readFile "day22.txt"
    let problem = parse contents
    putStrLn $ "Part one : " ++ show (solvePartOne problem)
    putStrLn $ "Part two : " ++ show (solvePartTwo problem)

data Spell = Missile | Drain | Shield | Poison | Recharge deriving Show

data State = State {
    playerHP :: Int,
    playerMana :: Int,
    totalMana :: Int,
    budget :: Int,
    bossHP :: Int,
    bossDamage :: Int,
    shieldActive :: Int,
    poisonActive :: Int,
    rechargeActive :: Int,
    isHard :: Bool
} deriving Show

data Outcome = Win Int | Loss

parse :: String -> State
parse contents = st
    where
        [h, d] = map f $ lines contents
        f = read . snd . splitOnce ':'
        st = State {
            playerHP = 50,
            playerMana = 500,
            totalMana = 0,
            budget = 999999,
            bossHP = h,
            bossDamage = d,
            shieldActive = 0,
            poisonActive = 0,
            rechargeActive = 0,
            isHard = False
        }


solvePartOne :: State -> Int
solvePartOne st =
    case playerTurn st of
         Win m -> solvePartOne $ st { budget = m }
         Loss -> budget st

solvePartTwo :: State -> Int
solvePartTwo st = solvePartOne $ st { isHard = True }

spellCost :: Spell -> Int
spellCost Missile = 53
spellCost Drain = 73
spellCost Shield = 113
spellCost Poison = 173
spellCost Recharge = 229

chain :: [State -> Either Outcome State]
         -> (State -> Outcome) -> State -> Outcome
chain (f:fs) g st =
    case f st of
         Left out -> out
         Right st' -> chain fs g st'
chain [] g st = g st

playerTurn :: State -> Outcome
playerTurn = chain [applyHard, applyEffects] castSpells

applyHard :: State -> Either Outcome State
applyHard st =
    case (isHard st, playerHP st) of
         (False, _) -> Right st
         (True, 1) -> Left Loss
         (True, n) -> Right $ st { playerHP = n - 1 }

applyEffects :: State -> Either Outcome State
applyEffects = applyPoison . applyShield . applyRecharge

applyPoison :: State -> Either Outcome State
applyPoison st =
    case (poisonActive st, bossHP st) of
         (0, _) -> Right st
         (_, hp) | hp <= 3 -> Left $ Win $ totalMana st
         (n, hp) -> Right $ st { poisonActive = n - 1, bossHP = hp - 3 }

applyShield :: State -> State
applyShield st =
    case shieldActive st of
         0 -> st
         n -> st { shieldActive = n - 1 }

applyRecharge :: State -> State
applyRecharge st =
    case (rechargeActive st, playerMana st) of
         (0, _) -> st
         (n, m) -> st { rechargeActive = n - 1, playerMana = m + 101 }

castSpells :: State -> Outcome
castSpells st = pickWin $ map (`castSpell` st)
    [ Missile, Drain, Shield, Poison, Recharge ]

pickWin :: [Outcome] -> Outcome
pickWin ((Win n):_) = Win n
pickWin (_:xs) = pickWin xs
pickWin [] = Loss

castSpell :: Spell -> State -> Outcome
castSpell spell = chain [applySpell spell, checkBossAlive] bossTurn

checkBossAlive :: State -> Either Outcome State
checkBossAlive st
    | bossHP st <= 0 = Left $ Win (totalMana st)
    | otherwise = Right st

applySpell :: Spell -> State -> Either Outcome State
applySpell Missile st = withdrawMana 53 $
    st { bossHP = bossHP st - 4 }
applySpell Drain st = withdrawMana 73 $
    st { bossHP = bossHP st - 2, playerHP = playerHP st + 2 }
applySpell Shield st
    | shieldActive st > 0 = Left Loss
    | otherwise = withdrawMana 113 $ st { shieldActive = 6 }
applySpell Poison st
    | poisonActive st > 0 = Left Loss
    | otherwise = withdrawMana 173 $ st { poisonActive = 6 }
applySpell Recharge st
    | rechargeActive st > 0 = Left Loss
    | otherwise = withdrawMana 229 $ st { rechargeActive = 5 }

withdrawMana :: Int -> State -> Either Outcome State
withdrawMana m st
    | x < 0 = Left Loss
    | y >= budget st = Left Loss
    | otherwise = Right $ st { playerMana = x, totalMana = y }
    where
        x = playerMana st - m
        y = totalMana st + m

bossTurn :: State -> Outcome
bossTurn = chain [applyEffects, applyDamage] playerTurn

applyDamage :: State -> Either Outcome State
applyDamage st =
    let
        armor = if shieldActive st > 0 then 7 else 0
        damage = max 1 $ bossDamage st - armor
        hpAfter = playerHP st - damage
    in
        if hpAfter <= 0
            then Left Loss
            else Right $ st { playerHP = hpAfter }

