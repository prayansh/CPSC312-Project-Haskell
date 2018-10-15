module AIPlayer where

import Data.Char
import Data.Maybe
import System.Random
import Text.Read (readMaybe)
import TypeDef
import UltimateTicTacToe

-- Super simple AI player
-- | Chooses first option from valid actions
unintelligent_player :: Player
unintelligent_player (State ub actB nextP) True = do
  let action = head (get_valid_actions ub actB)
  return action
unintelligent_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  let action = head (get_valid_actions ub actB)
  return action

-- Simple Player
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
simple_player (State ub actB nextP) True = do
  let valid_actions = (get_valid_actions ub actB)
  let action = get_next_best valid_actions all_actions
  return action
simple_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  let valid_actions = (get_valid_actions ub actB)
  let action = get_next_best valid_actions all_actions
  return action

-- | get next best action from list
get_next_best :: [Action] -> [Action] -> Action
get_next_best valid (x:xs)
  | elem x valid = x
  | otherwise = get_next_best valid xs
get_next_best _ _ = Invalid

-- | Not implemented, Chooses a random action from valid_actions list
random_player :: Player
random_player (State ub actB nextP) False = do
  putStrLn ("Press enter for the AI to place " ++ (show nextP))
  getLine
  let valid_actions = (get_valid_actions ub actB)
  index <- randomRIO (0, ((length valid_actions) - 1) :: Int)
  let action = valid_actions !! index
  return action
random_player (State ub actB nextP) _ = do
  let valid_actions = (get_valid_actions ub actB)
  index <- randomRIO (0, ((length valid_actions) - 1) :: Int)
  let action = valid_actions !! index
  return action

-- | Not implemented, Chooses the first action from a shuffled valid_actions list
shuffle_player :: Player
shuffle_player (State ub actB nextP) _ = do
  return Invalid
