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
start :: [Player] -> Bool -> IO()
start [p1,p2] True = playFast ultimateTicTacToe (ContinueGame emptyState) [p1,p2]
    where emptyState = (State emptyUBoardCell (-1) X)

start [p1,p2] _ = play ultimateTicTacToe (ContinueGame emptyState) [p1,p2]
    where emptyState = (State emptyUBoardCell (-1) X)

start _ _ = do
    putStrLn "Must specify two players"

startDefault = start [human_player, unintelligent_player] False

---- Play functions
play :: Game -> Result -> [Player] -> IO ()

play _ (EndOfGame winner (State ub actB _)) _ = do
    draw_ultimate_board ub (-1) (u_board_to_board ub)
    putStrLn ("Winner " ++ (show winner))

play game (ContinueGame (State ub actB playerSymbol)) [currentP, otherP] = do
    draw_ultimate_board ub actB (u_board_to_board ub)
    putStrLn ("Now playing: "++ (show playerSymbol))
    action <- currentP (State ub actB playerSymbol) False
    putStrLn ((show playerSymbol) ++ ": " ++ (show action))
    let playerList = if isPlaceAt action then [otherP, currentP] else [currentP, otherP]
    play game (game action (State ub actB playerSymbol)) playerList

-- | Reduce output for faster game processing (ideal for AI vs AI)
playFast :: Game -> Result -> [Player] -> IO ()

playFast _ (EndOfGame winner (State ub actB _)) _ = do
    draw_ultimate_board ub (-1) (u_board_to_board ub)
    putStrLn ("Winner " ++ (show winner))

playFast game (ContinueGame (State ub actB playerSymbol)) [currentP, otherP] = do
    action <- currentP (State ub actB playerSymbol) True
    putStrLn ((show playerSymbol) ++ ": " ++ (show action))
    let playerList = if isPlaceAt action then [otherP, currentP] else [currentP, otherP]
    playFast game (game action (State ub actB playerSymbol)) playerList