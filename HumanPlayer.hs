module HumanPlayer where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import GameHelper
import UltimateTicTacToe

---- Player Implementations
-- Human Player implementation
human_player :: Player
human_player (State ub actB nextP) _  = do
    if actB == (-1)
        then putStrLn "Choose board to play next. Example: A"
        else putStrLn ("Choose a coordinate (row,column) in board [" ++
                            [(bNameFromInt actB)] ++ "] for where to place "++(show nextP)++". Example: (1,2)")
    line <- getLine
    let valid_actions = get_valid_actions ub actB
    let action = gen_action actB line
    if elem action valid_actions
        then return action
        else return Invalid

-- convert user input to Action
gen_action :: Int -> String -> Action
gen_action (-1) [bs] = (ChooseBoard b)
    where b = bNameToInt bs
gen_action _ input
    | rowCol == Nothing = Invalid
    | otherwise = PlaceAt (row - 1) (col - 1)
    where
        rowCol = (readMaybe input :: Maybe (Int, Int))
        row = maybe (-1) fst rowCol
        col = maybe (-1) snd rowCol
