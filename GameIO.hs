module GameIO where

import AIPlayer
import Data.Char
import Data.Maybe
import GameHelper
import GameVisualization
import HumanPlayer
import Text.Read (readMaybe)
import TypeDef
import UltimateTicTacToe

---- Game Start
-- | Start the game with a list of two players and a boolean for fast-mode
start :: [Player] -> Bool -> IO ()
start [p1, p2] True =
  playFast ultimateTicTacToe (ContinueGame emptyState) [p1, p2]
  where
    emptyState = State emptyUBoardCell (-1) X
start [p1, p2] _ = play ultimateTicTacToe (ContinueGame emptyState) [p1, p2]
  where
    emptyState = State emptyUBoardCell (-1) X
start _ _ = do
  putStrLn "Must specify two players"

-- | Start a default game of Human vs AI
startDefault = start [human_player, unintelligent_player] False

---- Play functions
-- | Play the game given the current result, and a list of players
play :: Game -> Result -> [Player] -> IO ()
play _ (EndOfGame winner (State ub actB _)) _ = do
  draw_ultimate_board ub (-1) (u_board_to_board ub)
  putStrLn ("Winner " ++ (show winner))
play game (ContinueGame (State ub actB playerSymbol)) [currentP, otherP] = do
  draw_ultimate_board ub actB (u_board_to_board ub)
  putStrLn ("Now playing: " ++ (show playerSymbol))
  action <- currentP (State ub actB playerSymbol) False
  putStrLn ((show playerSymbol) ++ ": " ++ (show action))
  let playerList =
        if isPlaceAt action
          then [otherP, currentP]
          else [currentP, otherP]
  play game (game action (State ub actB playerSymbol)) playerList

-- | Play a game with reduced output, for faster game speed (ideal for AI vs AI)
playFast :: Game -> Result -> [Player] -> IO ()
playFast _ (EndOfGame winner (State ub actB _)) _ = do
  draw_ultimate_board ub (-1) (u_board_to_board ub)
  putStrLn ("Winner " ++ (show winner))
playFast game (ContinueGame (State ub actB playerSymbol)) [currentP, otherP] = do
  action <- currentP (State ub actB playerSymbol) True
  putStrLn ((show playerSymbol) ++ ": " ++ (show action))
  let playerList =
        if isPlaceAt action
          then [otherP, currentP]
          else [currentP, otherP]
  playFast game (game action (State ub actB playerSymbol)) playerList

playSuperFast :: Game -> Result -> [Player] -> IO (Maybe Symbol)
playSuperFast _ (EndOfGame winner (State ub actB _)) _ = do
  putStrLn ("Winner " ++ (show winner))
  return winner
--   draw_ultimate_board ub (-1) (u_board_to_board ub)
playSuperFast game (ContinueGame (State ub actB playerSymbol)) [currentP, otherP] = do
  action <- currentP (State ub actB playerSymbol) True
  let playerList =
        if isPlaceAt action
          then [otherP, currentP]
          else [currentP, otherP]
  playSuperFast game (game action (State ub actB playerSymbol)) playerList

tournament 0 (p1, p2) (s1, s2, d) = do
  putStrLn
    ("Score: p1: " ++
     (show s1) ++ ", p2: " ++ (show s2) ++ ", draws: " ++ (show d))
tournament number (p1, p2) (s1, s2, d) = do
  let emptyState = State emptyUBoardCell (-1) X
  winner <- playSuperFast ultimateTicTacToe (ContinueGame emptyState) [p1, p2]
  let newScore =
        if (odd number)
          then case winner of
                 Nothing -> (s1, s2, d + 1)
                 Just X -> (s1 + 1, s2, d)
                 Just O -> (s1, s2 + 1, d)
          else case winner of
                 Nothing -> (s1, s2, d + 1)
                 Just X -> (s1, s2+1, d)
                 Just O -> (s1+1, s2, d)
  tournament (number - 1) (p2, p1) newScore

startTournament number (p1, p2) = tournament number (p1, p2) (0, 0, 0)
