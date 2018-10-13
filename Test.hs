module Test where

import TypeDef
import GameVisualization
import HumanPlayer
import AIPlayer
import UltimateTicTacToe
import GameHelper
import GameIO

ubc = uBoardToCell --alias

testStart1 :: IO()
testStart1 = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
    where startState = (State (ubc tBoardAlmostX) 0 X)

testStart2 :: IO()
testStart2 = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
    where startState = (State (ubc tBoardAlmostO) 0 O)

testStart3 :: IO()
testStart3 = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
    where startState = (State (ubc tUBoardAlmostX) 2 X)

testStart4 :: IO()
testStart4 = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
    where startState = (State (ubc tUBoardAlmostO) 2 O)

testStart5 :: IO()
testStart5 = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
    where startState = (State (ubc tUBoardAlmostDraw) 1 X)

--testPlayDraw :: IO()
--testPlayDraw = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
--    where startState = (State (ubc tUBoardDraw) 2 O)
--
--testPlayXWin :: IO()
--testPlayXWin = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
--    where startState = (State (ubc testUBoardWinX) 2 O)

testEndScenario = play ultimateTicTacToe (EndOfGame (Just X) (State emptyUBoardCell (-1) X)) []
