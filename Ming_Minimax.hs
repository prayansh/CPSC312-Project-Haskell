module Ming_Minimax where

import TypeDef
import UltimateTicTacToe
import TreeDict

type Mem = Dict State (Action, Double)

-- Minimax Player
-- | Searches all game outcomes and assigns values to them, then chooses best
minimax :: Game -> State -> Mem -> Int -> ((Action, Double), Mem)
minimax game state mem limit =
    case getval state mem of
        Just val -> (val, mem)
        Nothing ->
            let (act_val, mem1) = argmax (valueact game state) avail mem limit
                in (act_val, (insertval state act_val mem1))
    where
        State ub actB nextP = state
        avail = (get_valid_actions ub actB)

-- argmax f lst = (e, f e) for e <- lsts where f e is maximal
-- Note that this does not require the elements of lst to be comparable
-- like  max[(f e,e) <- e in lst] but where only the first elements of pairs
-- are compared in the max
argmax :: Ord v => (e -> Mem -> Int -> (v, Mem)) -> [e] -> Mem -> Int -> ((e, v), Mem)
argmax f [e] mem limit = ((e, v), mem1)
    where (v, mem1) = f e mem limit
argmax f (h:t) mem limit
    | fh > ft = ((h, fh), mem2)
    | otherwise = ((bt, ft), mem2)
    where
        ((bt, ft), mem1) = argmax f t mem limit
        (fh, mem2) = f h mem1 limit

-- valueact game state action returns the value of doing action in state for game
valueact :: Game -> State -> Action -> Mem -> Int -> (Double, Mem)
valueact game state action = value game action (game action state)

-- value game result returns value based on result
value :: Game -> Action -> Result -> Mem -> Int -> (Double, Mem)
value _ _ (EndOfGame Nothing _) mem _ = (0.0, mem)
value _ _ (EndOfGame _ _) mem _ = (1.0, mem)
value game (ChooseBoard b) (ContinueGame state) mem limit
    | limit == 0 = (getScore (ChooseBoard b) state, mem)
    | otherwise =
        let ((_, val), mem2) = minimax game state mem (limit - 1)
            in (val, mem2)
value game (PlaceAt r c) (ContinueGame state) mem limit
    | limit == 0 = (getScore (PlaceAt r c) state, mem)
    | otherwise =
        let ((_, val), mem2) = minimax game state mem (limit - 1)
            in (-val, mem2)

-- abstract minimax player
ming_mm_player :: Game -> State -> Action
ming_mm_player game state = fst (fst (minimax game state emptyDict (getLimit state)))

getLimit :: State -> Int
getLimit (State ub actB nextP)
    | numZeroes <= 10 = 50
    | numZeroes <= 20 = 30
    | numZeroes <= 30 = 10
    | numZeroes <= 40 = 5
    | numZeroes <= 81 = 3
    where numZeroes = countZeroes ub

countZeroes :: UltimateBoard -> Int
countZeroes [] = 0
countZeroes ub = count (concat (concat (concat ub)))

count :: [Maybe Symbol] -> Int
count [] = 0
count (h:t)
    | h == Nothing = 1 + countRest
    | otherwise = countRest
    where countRest = count t

getScore :: Action -> State -> Double
getScore (PlaceAt _ _) (State _ (-1) _) = 0.3
getScore (PlaceAt _ _) (State _ _ _) = 0.5
getScore (ChooseBoard 4) (State _ _ _) = 0.8
getScore (ChooseBoard 0) (State _ _ _) = 0.7
getScore (ChooseBoard 2) (State _ _ _) = 0.7
getScore (ChooseBoard 6) (State _ _ _) = 0.7
getScore (ChooseBoard 8) (State _ _ _) = 0.7
getScore (ChooseBoard 1) (State _ _ _) = 0.6
getScore (ChooseBoard 3) (State _ _ _) = 0.6
getScore (ChooseBoard 5) (State _ _ _) = 0.6
getScore (ChooseBoard 7) (State _ _ _) = 0.6
getScore _ _ = 0.0
