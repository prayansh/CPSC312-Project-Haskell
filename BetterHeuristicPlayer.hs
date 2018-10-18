module BetterHeuristicPlayer where

import TypeDef
import UltimateTicTacToe

-- Better Heuristic Player
winSeqH_player :: Game -> State -> Action
winSeqH_player game state = fst (heuristic game state 3)

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
valuePlaceAct game st ttl act = heuristicForPlace game (game act st) ttl

valueChooseAct :: Game -> State -> Int -> Action -> Double
valueChooseAct game st ttl act = heuristicForChoose game (game act st) ttl

heuristicForPlace :: Game -> Result -> Int -> Double
heuristicForPlace game (ContinueGame (State ub actB symbol)) 0 =
  heuristicU ub symbol
heuristicForPlace game (ContinueGame st) ttl =
  (snd (heuristic game st (ttl - 1)))
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

-- |  Winning Possibilities Heuristic
heuristicU :: UltimateBoard -> Symbol -> Double
heuristicU ub p
  | is_u_board_draw ub = 0.0
  | pWinner = 10.0 ** 4 + playableCellsCount
  | oWinner = -(10.0 ** 4 + playableCellsCount)
  | otherwise =
    sum [heuristicB b p | b <- concat ub, not (is_board_draw b)] +
    (heuristicB (u_board_to_board ub) p) * 50
  where
    winner = get_winner_u_board ub
    pWinner = maybe False (== p) winner
    oWinner = maybe False (== (nextSymbol p)) winner
    playableCellsCount =
      sum ([3 | c <- (concat (concat (concat ub))), c == Nothing])

-- heuristicB :: Board -> Symbol -> Double
-- heuristicB b p
--   | is_board_won b || is_board_draw b = 0
--   | otherwise = playerVal - opponentVal
--   where
--     winCombs = gen_wins_rows b
--     playerVal =
--       sum
--         [ if length y > 1
--           then 8
--           else 1
--         | y <- [[x | x <- seq, x == Just p] | seq <- winCombs]
--         ]
--     opponentVal =
--       sum
--         [ if length y > 1
--           then 8
--           else 1
--         | y <- [[x | x <- seq, x == Just (nextSymbol p)] | seq <- winCombs]
--         ]
heuristicB :: Board -> Symbol -> Double
heuristicB b p
  | is_board_draw b = 0
  | otherwise = playerVal - opponentVal
  where
    winCombs = gen_wins_rows b
    filteredSeqP =
      [ onlyPSeq
      | onlyPSeq <- [[c | c <- seq, c /= Nothing] | seq <- winCombs]
      , not (elem (Just (nextSymbol p)) onlyPSeq)
      ]
    filteredSeqO =
      [ onlyOSeq
      | onlyOSeq <- [[c | c <- seq, c /= Nothing] | seq <- winCombs]
      , not (elem (Just p) onlyOSeq)
      ]
    playerVal =
      sum
        [ if length y > 1
          then 8
          else 1
        | y <- filteredSeqP
        ]
    opponentVal =
      sum
        [ if length y > 1
          then 8
          else 1
        | y <- filteredSeqO
        ]
---------------------------- Debugging
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
