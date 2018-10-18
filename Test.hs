module Test where

import AIPlayer
import GameHelper
import GameIO
import GameVisualization
import HumanPlayer
import TypeDef
import UltimateTicTacToe

ubc = uBoardToCell --alias

testStart1 :: IO ()
testStart1 =
  play
    ultimateTicTacToe
    (ContinueGame startState)
    [human_player, unintelligent_player]
  where
    startState = (State (ubc tBoardAlmostX) 0 X)

testStart2 :: IO ()
testStart2 =
  play
    ultimateTicTacToe
    (ContinueGame startState)
    [human_player, unintelligent_player]
  where
    startState = (State (ubc tBoardAlmostO) 0 O)

testStart3 :: IO ()
testStart3 =
  play ultimateTicTacToe (ContinueGame startState) [human_player, human_player]
  where
    startState = (State (ubc tUBoardAlmostX) 2 X)

testStart4 :: IO ()
testStart4 =
  play
    ultimateTicTacToe
    (ContinueGame startState)
    [human_player, unintelligent_player]
  where
    startState = (State (ubc tUBoardAlmostO) 2 O)

testStart5 :: IO ()
testStart5 =
  play
    ultimateTicTacToe
    (ContinueGame startState)
    [human_player, unintelligent_player]
  where
    startState = (State (ubc tUBoardAlmostDraw) 1 X)

testStartGuiPLayer :: IO ()
testStartGuiPLayer = playFast ultimateTicTacToe (ContinueGame startState) [unintelligent_player, (gui_player_with_options (9,9) 2)]
     where startState = (State (ubc tUBoardAlmostO) (-1) X)

--testPlayDraw :: IO()
--testPlayDraw = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
--    where startState = (State (ubc tUBoardDraw) 2 O)
--
--testPlayXWin :: IO()
--testPlayXWin = play ultimateTicTacToe (ContinueGame startState) [human_player, ai_player]
--    where startState = (State (ubc testUBoardWinX) 2 O)
testEndScenario :: IO ()
testEndScenario =
  play ultimateTicTacToe (EndOfGame (Just X) (State emptyUBoardCell (-1) X)) []
