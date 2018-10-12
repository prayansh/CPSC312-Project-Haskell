module AIPlayer where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import GameHelper

-- Super simple AI player
ai_player :: Player
ai_player (State ub actB nextP) _ = head (get_valid_actions ub actB)