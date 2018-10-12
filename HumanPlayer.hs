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
        action = gui_parse_action actB line


gui_parse_action (-1) [bs] = (ChooseBoard b)
    where b = ((digitToInt bs) - 10)
gui_parse_action _ ['(',rs,',',cs,')'] = (PlaceAt r c)
    where 
        r = (digitToInt rs)
        c = (digitToInt cs)
gui_parse_action _ _ = Invalid

-- -- convert user input to Action
-- gen_action :: Int -> String -> Action
-- gen_action (-1) input = (ChooseBoard (fromJust (readMaybe input :: Maybe Int)))


-- gen_action _ input = (PlaceAt row col)
--     where
--         (row, col) = fromJust(readMaybe input :: Maybe (Int, Int))
