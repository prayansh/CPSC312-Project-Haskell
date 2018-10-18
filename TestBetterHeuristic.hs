module TestBetterHeuristic where

import BetterHeuristicPlayer
import TypeDef
import UltimateTicTacToe

-- alias
ubc = uBoardToCell

s ub actB p = State (ubc ub) actB p

h0 s = heuristic ultimateTicTacToe s 0

h1 s = heuristic ultimateTicTacToe s 1

h2 s = heuristic ultimateTicTacToe s 2

h3 s = heuristic ultimateTicTacToe s 3

h4 s = heuristic ultimateTicTacToe s 4

hl0 s = heuristicList ultimateTicTacToe s 0

hl1 s = heuristicList ultimateTicTacToe s 1

hl2 s = heuristicList ultimateTicTacToe s 2

hl3 s = heuristicList ultimateTicTacToe s 3

hl4 s = heuristicList ultimateTicTacToe s 4
