module GameIO where

import GameVisualization
import UltimateTicTacToe

data Result = EndOfGame CellVal State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> String -> Action

data Action = PlaceAt Int Int 
              |ChooseBoard Int 
              |Invalid                 

data State = State UltimateBoard Int CellVal 

gameFun :: Game
gameFun act (State ub actB nextP) = (ContinueGame (State ub actB nextP))

playerFun :: Player
playerFun (State ub actB nextP) s = (ChooseBoard actB)

playerFun2 :: Player
playerFun2 (State ub actB nextP) s = (PlaceAt 1 1)

start :: IO()
start = play gameFun (ContinueGame emptyState) playerFun  
    where emptyState = (State emptyUBoard -1 1)

play :: Game -> Result -> Player -> IO ()
play game (EndOfGame winner _st) player = do
    putStrLn "Winner " ++ (show winner)

play game (ContinueGame (State ub actB 1)) player = do
    -- printBoard
    if actB == -1
        then do putStrLn "Choose board"
        else do putStrLn "Choose coordinates with this format (1,1)"
    line <- getLine 
    let action = player (State ub actB 1) line
    play game (game action (State ub actB 1)) player

play game (ContinueGame (State ub actB 2)) player = do
    let action = player (State ub actB 2) line
    -- Add computer has played this 
    play game (game action (State ub actB 2)) player



human_player :: Player
human_player (State ub actB nextP) line 
    |elem action (getValidActions ub actB) = action
    |otherwise = (Invalid)
    where
        if actB == -1
            then action = (ChooseBoard (readMaybe line :: Maybe Int))
            else action = (PlaceAt (readMaybe line :: Maybe Int Int))





cpu_player
-- personPlay ::  Game -> Result -> Player -> IO ()


-- play game start opponent tournament_state =