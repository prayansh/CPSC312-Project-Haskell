module HeuristicPlayer where

import TypeDef
import UltimateTicTacToe

-- Heuristic minimax player
hmm_player :: Game -> State -> Action
hmm_player game state = fst (heuristic game state 3)

heuristic :: Game -> State -> Int -> (Action, Double)
-- ChooseBoard actions
heuristic game (State ub (-1) currSym) ttl =
  argmax (valueChooseAct game (State ub (-1) currSym) ttl) avail
  where
    avail = (get_valid_actions ub (-1))
-- PlaceAt actions
heuristic game state ttl = argmax (valuePlaceAct game state ttl) avail
  where
    State ub actB _ = state
    avail = (get_valid_actions ub actB)

valuePlaceAct :: Game -> State -> Int -> Action -> Double
valuePlaceAct game st ttl act =
  heuristicForPlace game (game act st) ttl

valueChooseAct :: Game -> State -> Int -> Action -> Double
valueChooseAct game st ttl act =
  heuristicForChoose game (game act st) ttl

heuristicForPlace :: Game -> Result -> Int -> Double
heuristicForPlace game (ContinueGame (State ub actB symbol)) 0 =
  heuristicU ub symbol
heuristicForPlace game (ContinueGame st) ttl =
  - (snd (heuristic game st (ttl - 1)))
  where
    (State ub _ symbol) = st
heuristicForPlace game (EndOfGame winner (State ub actB symbol)) ttl =
  heuristicU ub symbol

heuristicForChoose :: Game -> Result -> Int -> Double
heuristicForChoose game (ContinueGame st) 0 =
  heuristicB ((concat ub) !! actB) symbol
  where
    (State ub actB symbol) = st
heuristicForChoose game (ContinueGame st) ttl =
  (snd (heuristic game st (ttl - 1)))
  where
    (State ub actB symbol) = st
-- heuristicForChoose game (EndOfGame winner (State ub actB symbol)) _ =
--   heuristicB ((concat ub) !! actB) symbol

argmax :: Ord v => (e -> v) -> [e] -> (e, v)
argmax f [e] = (e, f e)
argmax f (h:t)
  | fh > ft = (h, fh)
  | otherwise = (bt, ft)
  where
    (bt, ft) = argmax f t
    fh = f h

-- |  Recursive Weight Heuristic
bWeights = [3, 2, 3, 2, 4, 2, 3, 2, 3]

sumWeights = sum bWeights

heuristicU :: UltimateBoard -> Symbol -> Double
heuristicU ub p
  | pWinner = sumWeights * sumWeights
  | oWinner = -sumWeights * sumWeights
  | is_u_board_draw ub = 0
  | otherwise = sum [heuristicB b p | b <- concat ub]
  where
    winner = get_winner_u_board ub
    pWinner = maybe False (== p) winner
    oWinner = maybe False (== (nextSymbol p)) winner

heuristicB :: Board -> Symbol -> Double
heuristicB b p
  | pWinner = sumWeights
  | oWinner = -sumWeights
  | is_board_draw b = 0
  | otherwise =
    sum
      [ weight
      | (weight, cell) <- zip bWeights (concat b)
      , maybe False (== p) cell
      ]
  where
    winner = get_winner_board b
    pWinner = maybe False (== p) winner
    oWinner = maybe False (== (nextSymbol p)) winner

----------------------------- Debugging Code
heuristicList :: Game -> State -> Int -> [(Action, Double)]
-- ChooseBoard actions
heuristicList game (State ub (-1) currSym) ttl =
  argmaxList (valueChooseAct game (State ub (-1) currSym) ttl) avail
  where
    avail = (get_valid_actions ub (-1))

-- PlaceAt actions
heuristicList game state ttl = argmaxList (valuePlaceAct game state ttl) avail
  where
    State ub actB _ = state
    avail = (get_valid_actions ub actB)

argmaxList :: Ord v => (e -> v) -> [e] -> [(e, v)]
argmaxList f [e] = [(e, f e)]
argmaxList f (h:t) = (h, f h) : argmaxList f t

