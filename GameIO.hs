module GameIO where

import Text.Read (readMaybe)
import Data.Maybe
import Data.Char
import TypeDef
import GameVisualization
import HumanPlayer
import AIPlayer
import UltimateTicTacToe
import GameHelper

---- Game Start
start :: IO()
start = play ultimateTicTacToe (ContinueGame emptyState) [human_player, ai_player]
    where emptyState = (State emptyUBoardCell (-1) X)

---- Play functions
play :: Game -> Result -> [Player] -> IO ()
play _ (EndOfGame winner (State ub actB _)) _ = do
    draw_ultimate_board ub (-1) (u_board_to_board ub)
    putStrLn ("Winner " ++ (show winner))

-- Human Player playing
play game (ContinueGame (State ub actB X)) [p1,p2] = do
    draw_ultimate_board ub actB (u_board_to_board ub)
    if actB == (-1)
        then do putStrLn "Choose board to play next. Example: A"
        else do putStrLn ("Choose a coordinate (row,column) in board [" ++
                            [(bNameFromInt actB)] ++ "] for where to place x. Example: (1,2)")
    line <- getLine
    let action = p1 (State ub actB X) line
    putStrLn ("You: " ++ (show action))
    play game (game action (State ub actB X)) [p1,p2]

-- AI Player playing
play game (ContinueGame (State ub actB O)) [p1,p2] = do
    draw_ultimate_board ub actB (u_board_to_board ub)
    putStrLn "Press enter for the AI to place o"
    getLine
    let action = p2 (State ub actB O) ""
    -- Add computer has played this
    putStrLn ("Computer: " ++ (show action))
    play game (game action (State ub actB O)) [p1,p2]