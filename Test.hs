module Test where

import AIPlayer
import GameHelper
import GameIO
import GameVisualization
import HumanPlayer
import TypeDef
import UltimateTicTacToe

ubc = uBoardToCell --alias

testStart1 :: IO()
testStart1 = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
    where startState = (State (ubc tBoardAlmostX) 0 X)

testStart2 :: IO()
testStart2 = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
    where startState = (State (ubc tBoardAlmostO) 0 O)

testStart3 :: IO()
testStart3 = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
    where startState = (State (ubc tUBoardAlmostX) 2 X)

testStart4 :: IO()
testStart4 = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
    where startState = (State (ubc tUBoardAlmostO) 2 O)

testStart5 :: IO()
testStart5 = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
    where startState = (State (ubc tUBoardAlmostDraw) 1 X)

testMiniMax :: IO()
testMiniMax = playFast ultimateTicTacToe (ContinueGame startState) [simple_player, betterH_player]
    where startState = (State (ubc tBoardMinimax) (-1) O)

--testPlayDraw :: IO()
--testPlayDraw = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
--    where startState = (State (ubc tUBoardDraw) 2 O)
--
--testPlayXWin :: IO()
--testPlayXWin = play ultimateTicTacToe (ContinueGame startState) [human_player, simple_player]
--    where startState = (State (ubc testUBoardWinX) 2 O)
testEndScenario :: IO ()
testEndScenario =
  play ultimateTicTacToe (EndOfGame (Just X) (State emptyUBoardCell (-1) X)) []
