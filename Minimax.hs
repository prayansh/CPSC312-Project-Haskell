module Minimax where

import TypeDef
import UltimateTicTacToe
import TreeDict

type Mem = Dict State (Action, Double)

-- Minimax Player
-- | Searches all game outcomes and assigns values to them, then chooses best
minimax :: Game -> State -> Mem -> ((Action, Double), Mem)
minimax game state mem =
    case getval state mem of
        Just val -> (val, mem)
        Nothing ->
            let (act_val, mem1) = argmax (valueact game state) avail mem
            in (act_val, (insertval state act_val mem1))
    where
        State ub actB nextP = state
        avail = (get_valid_actions ub actB)

-- argmax f lst = (e, f e) for e <- lsts where f e is maximal
-- Note that this does not require the elements of lst to be comparable
-- like  max[(f e,e) <- e in lst] but where only the first elements of pairs
-- are compared in the max
argmax :: Ord v => (e -> Mem -> (v, Mem)) -> [e] -> Mem -> ((e, v), Mem)
argmax f [e] mem = ((e, v), mem1)
    where (v, mem1) = f e mem
argmax f (h:t) mem
    | fh > ft = ((h, fh), mem2)
    | otherwise = ((bt, ft), mem2)
    where
        ((bt, ft), mem1) = argmax f t mem
        (fh, mem2) = f h mem1

-- valueact game state action returns the value of doing action in state for game
valueact :: Game -> State -> Action -> Mem -> (Double, Mem)
valueact game state action = value game (game action state)

-- value game result returns value based on result
value :: Game -> Result -> Mem -> (Double, Mem)
value _ (EndOfGame Nothing _) mem = (0.0, mem)
value _ (EndOfGame _ _) mem = (1.0, mem)
value game (ContinueGame state) mem =
    let ((_, val), mem2) = minimax game state mem
        in (-val, mem2)

-- abstract minimax player
mm_player :: Game -> State -> Action
mm_player game state = fst (fst (minimax game state emptyDict))
