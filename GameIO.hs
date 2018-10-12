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



gameFun :: Game
gameFun act (State ub actB nextP) = (ContinueGame (State ub actB nextP))

ultimateTicTacToe :: Game
ultimateTicTacToe Invalid s = (ContinueGame s)
ultimateTicTacToe (ChooseBoard b) (State ub actB nextP) = (ContinueGame (State ub b nextP))
ultimateTicTacToe (PlaceAt row col) (State ub actB p) 
    | nextUBoardWinner==0  = (ContinueGame (State nextUBoard nextActiveBoard (next p)) ) 
    | otherwise = (EndOfGame nextUBoardWinner (State nextUBoard (-1) p))
    where 
        nextUBoard = (put (State ub actB p) row col)
        nextUBoardWinner = board_winner (uboard_winners nextUBoard)
        nextActiveBoard = if (elem (to_board_index row col) (get_valid_boards nextUBoard))
            then (to_board_index row col) 
            else (-1)


put :: State -> Int -> Int -> UltimateBoard
put (State ub actB nextP) row col = put_u_board ub actB row col nextP

to_board_index :: Int -> Int -> Int
to_board_index row col = (3 * row) + col

to_board_name :: Int -> Char
to_board_name bi = toUpper (intToDigit (bi+10))

next :: CellState -> CellState
next 1 = 2
next 2 = 1


start :: IO()
start = play ultimateTicTacToe (ContinueGame emptyState) [human_player, ai_player]
    where emptyState = (State emptyUBoard (-1) 1)

---- Play functions
play :: Game -> Result -> [Player] -> IO ()
play _ (EndOfGame winner (State ub actB _)) _ = do
    draw_ultimate_board ub (-1)
    putStrLn ("Winner " ++ (show winner))

-- Human Player playing
play game (ContinueGame (State ub actB 1)) [p1,p2] = do
    draw_ultimate_board ub actB
    if actB == (-1)
        then do putStrLn "Choose board to play next. Example: A"
        else do putStrLn ("Choose a coordinate (row,column) in board ["++[(to_board_name actB)]++"] for where to place x. Example: (1,2)")
    line <- getLine
    let action = p1 (State ub actB 1) line
    putStrLn ("You played: " ++ (show action))
    play game (game action (State ub actB 1)) [p1,p2]

-- AI Player playing
play game (ContinueGame (State ub actB 2)) [p1,p2] = do
    draw_ultimate_board ub actB
    putStrLn "Press enter for the AI to place o"
    getLine
    let action = p2 (State ub actB 2) ""
    -- Add computer has played this
    putStrLn ("Computer Played: " ++ (show action))
    play game (game action (State ub actB 2)) [p1,p2]

















