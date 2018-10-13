module AIPlayer where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import UltimateTicTacToe

-- Super simple AI player
simple_player :: Player
simple_player (State ub actB nextP) = do
    putStrLn "Press enter for the AI to place o"
    getLine
    let action = head (get_valid_actions ub actB)
    return action
