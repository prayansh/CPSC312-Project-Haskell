module TestHeuristic where
import TypeDef
import UltimateTicTacToe
import HeuristicPlayer

-- alias
ubc = uBoardToCell
s ub actB p = State (ubc ub) actB p
h0 s = heuristic ultimateTicTacToe s 0
h1 s = heuristic ultimateTicTacToe s 1
h2 s = heuristic ultimateTicTacToe s 2
h3 s = heuristic ultimateTicTacToe s 3
h4 s = heuristic ultimateTicTacToe s 4


