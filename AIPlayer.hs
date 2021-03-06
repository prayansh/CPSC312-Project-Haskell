module AIPlayer where

import Data.Char
import Data.Maybe
import HeuristicPlayer
import BetterHeuristicPlayer
import System.Random
import Text.Read (readMaybe)
import TypeDef
import UltimateTicTacToe
import qualified GuiAI as Gui
import Minimax
import Ming_Minimax

-------------------------- Super simple AI player
-- | Chooses first option from valid actions
unintelligent_player :: Player
unintelligent_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  unintelligent_player (State ub actB nextP) True
unintelligent_player (State ub actB nextP) True = do
  let action = head (get_valid_actions ub actB)
  return action

--------------------------- Simple Player
-- | Chooses first valid option from a list of actions
choose_actions =
  [ ChooseBoard 0
  , ChooseBoard 2
  , ChooseBoard 5
  , ChooseBoard 6
  , ChooseBoard 3
  , ChooseBoard 4
  , ChooseBoard 8
  , ChooseBoard 7
  , ChooseBoard 1
  ]

placeAt_actions =
  [ PlaceAt 0 0
  , PlaceAt 0 2
  , PlaceAt 2 2
  , PlaceAt 2 0
  , PlaceAt 1 1
  , PlaceAt 1 0
  , PlaceAt 2 1
  , PlaceAt 1 2
  , PlaceAt 0 1
  ]

all_actions = choose_actions ++ placeAt_actions

simple_player :: Player
simple_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  simple_player (State ub actB nextP) True
simple_player (State ub actB nextP) True = do
  let valid_actions = (get_valid_actions ub actB)
  let action = get_next_best valid_actions all_actions
  return action

-- | get next best action from list
get_next_best :: [Action] -> [Action] -> Action
get_next_best valid (x:xs)
  | elem x valid = x
  | otherwise = get_next_best valid xs
get_next_best _ _ = Invalid

----------------------------------- Random Player
--  Chooses a random action from valid_actions list
random_player :: Player
random_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  random_player (State ub actB nextP) True
random_player (State ub actB nextP) _ = do
  let valid_actions = (get_valid_actions ub actB)
  index <- randomRIO (0, ((length valid_actions) - 1) :: Int)
  let action = valid_actions !! index
  return action

-- player that uses the heuristic function
my_hmm_player :: Player
my_hmm_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  my_hmm_player (State ub actB nextP) True
my_hmm_player state True = do
  return (hmm_player ultimateTicTacToe state)

-- player that uses the Better heuristic function
smart_player :: Player
smart_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  smart_player (State ub actB nextP) True
smart_player state True = do
  return (winSeqH_player ultimateTicTacToe state)

-- ultimate tic tac toe minimax player
my_mm_player :: Player
my_mm_player state True = do
    return (ming_mm_player ultimateTicTacToe state)
my_mm_player (State ub actB nextP) False = do
    putStrLn ("Press enter for the AI to place "++(show nextP))
    getLine
    return (ming_mm_player ultimateTicTacToe (State ub actB nextP))


-- Takes a non io, pure player and converts it into a io player
create_player :: (Game -> State -> Action) -> Player
create_player pure_player state True = do
    return (pure_player ultimateTicTacToe state)
create_player pure_player (State ub actB nextP) False = do
    putStrLn ("Press enter for the AI to place "++(show nextP))
    getLine
    return (pure_player ultimateTicTacToe (State ub actB nextP))

gui_player :: Player
gui_player = create_player Gui.player

gui_player_with_options :: (Double, Double) -> Int -> Player
gui_player_with_options hp depth = create_player (Gui.player_with_options hp depth)
