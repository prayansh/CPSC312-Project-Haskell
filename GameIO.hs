module GameIO where

import Text.Read (readMaybe)
import Data.Maybe
import TypeDef
import GameVisualization
import UltimateTicTacToe

data Result = EndOfGame CellState State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> String -> Action

data Action = PlaceAt Int Int 
              | ChooseBoard Int
              | Invalid
         deriving (Eq, Show)

data State = State UltimateBoard Int CellState
        deriving (Eq, Show)

gameFun :: Game
gameFun act (State ub actB nextP) = (ContinueGame (State ub actB nextP))

playerFun :: Player
playerFun (State ub actB nextP) s = (ChooseBoard actB)

playerFun2 :: Player
playerFun2 (State ub actB nextP) s = (PlaceAt 1 1)

start :: IO()
start = play gameFun (ContinueGame emptyState) playerFun
    where emptyState = (State emptyUBoard (-1) 1)

---- Play functions
play :: Game -> Result -> Player -> IO ()
play game (EndOfGame winner _st) player = do
    putStrLn ("Winner " ++ (show winner))

-- Human Player playing
play game (ContinueGame (State ub actB 1)) player = do
    -- printBoard
    if actB == (-1)
        then do putStrLn "Choose board"
        else do putStrLn "Choose coordinates with this format (1,1)"
    line <- getLine
    let action = player (State ub actB 1) line
    play game (game action (State ub actB 1)) player

-- AI Player playing
play game (ContinueGame (State ub actB 2)) player = do
    let action = player (State ub actB 2) ""
    -- Add computer has played this
    play game (game action (State ub actB 2)) player

---- Player Implementations
-- Human Player implementation
human_player :: Player
human_player (State ub actB nextP) line
    | elem action (get_valid_actions ub actB) = action
    | otherwise = (Invalid)
    where
        action = gen_action actB line

-- convert user input to Action
gen_action :: Int -> String -> Action
gen_action (-1) input = (ChooseBoard (fromJust (readMaybe input :: Maybe Int)))
gen_action _ input = (PlaceAt row col)
    where
        (row, col) = fromJust(readMaybe input :: Maybe (Int, Int))

-- Super simple AI player
ai_player :: Player
ai_player (State ub actB nextP) _ = head (get_valid_actions ub actB)

get_valid_actions :: UltimateBoard -> Int -> [Action]
get_valid_actions _ _ = [(ChooseBoard 4)]