module HeuristicPlayer where

import TypeDef
import UltimateTicTacToe

-- Heuristic minimax player
hmm_player :: Game -> State -> Action
hmm_player game state = fst (heuristic game state 3)

heuristic :: Game -> State -> Int -> (Action, Double)
-- ChooseBoard actions
heuristic game (State ub (-1) currSym) ttl =
  argmax (valueChooseAct game (State ub (-1) currSym) currSym ttl) avail
  where
    avail = (get_valid_actions ub (-1))
-- PlaceAt actions
heuristic game state ttl = argmax (valuePlaceAct game state currSym ttl) avail
  where
    State ub actB currSym = state
    avail = (get_valid_actions ub actB)

valuePlaceAct :: Game -> State -> Symbol -> Int -> Action -> Double
valuePlaceAct game st currSym ttl act =
  heuristicForPlace game (game act st) currSym ttl

valueChooseAct :: Game -> State -> Symbol -> Int -> Action -> Double
valueChooseAct game st currSym ttl act =
  heuristicForChoose game (game act st) currSym ttl

heuristicForPlace :: Game -> Result -> Symbol -> Int -> Double
heuristicForPlace game (ContinueGame (State ub actB _)) symbol 0 =
  heuristicU ub symbol
heuristicForPlace game (ContinueGame st) symbol ttl =
  (heuristicU ub symbol) - (snd (heuristic game st (ttl - 1)))
  where
    (State ub _ _) = st
heuristicForPlace game (EndOfGame winner (State ub actB _)) symbol ttl =
  heuristicU ub symbol

heuristicForChoose :: Game -> Result -> Symbol -> Int -> Double
heuristicForChoose game (ContinueGame st) symbol 0 =
  heuristicB ((concat ub) !! actB) symbol
  where
    (State ub actB _) = st
heuristicForChoose game (ContinueGame st) symbol ttl =
  heuristicB ((concat ub) !! actB) symbol + (snd (heuristic game st (ttl - 1)))
  where
    (State ub actB _) = st
heuristicForChoose game (EndOfGame winner (State ub actB _)) symbol _ = 0 -- TODO wtf

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
