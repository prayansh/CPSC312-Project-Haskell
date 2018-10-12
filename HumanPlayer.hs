module HumanPlayer where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import GameHelper

---- Player Implementations
-- Human Player implementation
human_player :: Player
human_player (State ub actB nextP) line
    | elem action (get_valid_actions ub actB) = action
    | otherwise = Invalid
    where
        action = gen_action actB line

-- convert user input to Action
gen_action :: Int -> String -> Action
gen_action (-1) [bs] = (ChooseBoard b)
    where b = ((digitToInt bs) - 10)
gen_action _ input
    | rowCol == Nothing = Invalid
    | otherwise = PlaceAt (fst (fromJust rowCol)) (snd (fromJust rowCol))
    where
        rowCol = (readMaybe input :: Maybe (Int, Int))
