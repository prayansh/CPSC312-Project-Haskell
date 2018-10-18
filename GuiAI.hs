module GuiAI where

import TypeDef
import UltimateTicTacToe
import qualified Data.Map as Map
import GuiHeuristic

type Mem = Map.Map State (Action, Double)
type PurePlayer = Game -> State -> Action

default_parameter_1 :: Double
default_parameter_1 = 9

default_parameter_2 :: Double
default_parameter_2 = 9

default_depth :: Int
default_depth = 2

-- 
player_with_options :: (Double, Double) -> Int -> PurePlayer
player_with_options heuristic_options depth game state = fst (fst (dl_minimax (gui_heuristic heuristic_options) depth game state Map.empty))

player :: PurePlayer
player = player_with_options (default_parameter_1,default_parameter_2) default_depth

----------------------------------------------------------------------------------------
-- Modified version of minimax algorithm (based on Minimax_mem.hs from class) 

dl_minimax :: Heuristic -> Int -> Game -> State -> Mem -> ((Action, Double), Mem)
dl_minimax heuristic depth game state mem =
    case Map.lookup state mem of
        Just val -> (val, mem)
        Nothing ->
            let (act_val, mem1) = argmax (valueact heuristic depth game state ) avail mem
            in (act_val, (Map.insert state act_val mem1))
    where
        State ub actB nextP = state
        avail = (get_valid_actions ub actB)

argmax :: Ord v => (e -> Mem -> (v, Mem)) -> [e] -> Mem -> ((e, v), Mem)
argmax f [e] mem = ((e, v), mem1)
    where (v, mem1) = f e mem
argmax f (h:t) mem
    | fh > ft = ((h, fh), mem2)
    | otherwise = ((bt, ft), mem2)
    where
        ((bt, ft), mem1) = argmax f t mem
        (fh, mem2) = f h mem1

valueact :: Heuristic -> Int -> Game -> State -> Action -> Mem -> (Double, Mem)
valueact heuristic depth game state action = value heuristic depth game (game action state)

value :: Heuristic -> Int -> Game -> Result -> Mem -> (Double, Mem)
value _ _ _ (EndOfGame Nothing _) mem = (0.0, mem)
value _ _ _ (EndOfGame _ _) mem = (1.0, mem)
value heuristic 0 game (ContinueGame state) mem = ((heuristic state) ,mem)
value heuristic depth game (ContinueGame state) mem =
    let ((_, val), mem2) = dl_minimax heuristic (depth-1) game state mem
        in (-val, mem2)
