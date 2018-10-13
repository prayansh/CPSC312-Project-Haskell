module AIPlayer where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import UltimateTicTacToe

-- Super simple AI player
unintelligent_player :: Player
unintelligent_player (State ub actB nextP) = do
    putStrLn ("Press enter for the AI to place "++(show nextP))
    getLine
    let action = head (get_valid_actions ub actB)
    return action

choose_actions =  [ ChooseBoard 0, ChooseBoard 6, ChooseBoard 5,
                    ChooseBoard 2, ChooseBoard 3, ChooseBoard 4,
                    ChooseBoard 8, ChooseBoard 7, ChooseBoard 1]
placeAt_actions = [ PlaceAt 2 0, PlaceAt 0 2, PlaceAt 0 0,
                    PlaceAt 2 2, PlaceAt 1 1, PlaceAt 1 0,
                    PlaceAt 2 1, PlaceAt 1 2, PlaceAt 0 1]
all_actions = choose_actions ++ placeAt_actions
simple_player :: Player
simple_player (State ub actB nextP) = do
    putStrLn ("Press enter for the AI to place "++(show nextP))
    getLine
    let valid_actions = (get_valid_actions ub actB)
    let action = get_next_best valid_actions all_actions
    return action

get_next_best :: [Action] -> [Action] -> Action
get_next_best valid (x:xs)
    | elem x valid = x
    | otherwise = get_next_best valid xs

get_next_best _ _ = Invalid

random_player :: Player
random_player (State ub actB nextP) = do
    return Invalid

shuffle_player :: Player
shuffle_player (State ub actB nextP) = do
    return Invalid