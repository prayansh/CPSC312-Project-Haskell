module TypeDef where

data Symbol = X | O
    deriving (Eq, Show, Read)
type Cell = Maybe Symbol -- Nothing for blank
type Board = [[Cell]]
type UltimateBoard = [[Board]]

-- | Result is either EndOfGame with winning symbol
-- |           or ContinueGame with new state
data Result = EndOfGame (Maybe Symbol) State     -- end of game, winning symbol, last state
            | ContinueGame State                 -- continue with state
         deriving (Eq, Show)

-- | Action is either PlaceAt with row, col
-- |               or ChooseBoard with index [0..8]
-- |               or Invalid
data Action = PlaceAt Int Int
              | ChooseBoard Int
              | Invalid
         deriving (Eq)
isPlaceAt (PlaceAt _ _) = True
isPlaceAt _     = False

isChooseBoard (ChooseBoard _) = True
isChooseBoard  _     = False

instance Show Action where
    show (PlaceAt row col) = "Place At (row: " ++ show (row+1) ++ ", col: " ++ show (col+1) ++ ")"
    show (ChooseBoard bNo) = "Choose Board " ++ show bNo
    show Invalid = "Invalid action"

-- | State represents the current board, and current active board, current(next) player
data State = State UltimateBoard Int Symbol
        deriving (Eq, Show)

-- | Game function given an Action, and a State, returns a Result
type Game = Action -> State -> Result

-- | Player function given a State, and a fast-mode boolean, returns an Action
type Player = State -> Bool -> IO Action


------------------------------------------------------------------------------------------
-- | Converts a Symbol into the number representation
symbolToInt :: Symbol -> Integer
symbolToInt X = 1
symbolToInt O = 2

-- | Converts a number into the Symbol representation
intToSymbol :: Integer -> Symbol
intToSymbol 1 = X
intToSymbol 2 = O

-- | Converts a Symbol into the char representation
symbolToChar :: Symbol -> Char
symbolToChar X = 'X'
symbolToChar O = 'O'

-- | Converts Cell to number representation
cellToInt :: Cell -> Integer
cellToInt Nothing  = 0
cellToInt (Just s) = symbolToInt s

-- | Converts Cell to number representation
intToCell :: Integer -> Cell
intToCell 0 = Nothing
intToCell s = Just $ intToSymbol s

-- | Shows the value of a cell
cellToChar :: Cell -> Char
cellToChar Nothing  = ' '
cellToChar (Just s) = symbolToChar s

-- | Convert number rep of UltimateBoard to Cell representation
uBoardToCell :: [[[[Integer]]]] -> UltimateBoard
uBoardToCell ub = [[boardToCell c | c <- r]| r<-ub]

-- | Convert number rep of Board to Cell representation
boardToCell :: [[Integer]] -> Board
boardToCell b = [[intToCell c | c <- r]| r<-b]

-- | Returns the other symbol
nextSymbol :: Symbol -> Symbol
nextSymbol X = O
nextSymbol O = X
------------------------------------------------------------------------------------------
emptyUBoard = [
       [ [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,0]] ]
    ]

testUBoard = [
       [ [[1,0,2],[1,0,2],[0,0,0]], [[0,0,2],[0,0,0],[0,0,0]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

testUBoard2 = [
       [ [[1,0,2],[1,0,2],[0,0,0]], [[0,0,2],[0,0,0],[0,0,0]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

testUBoardWinX = [
       [ [[1,1,1],[1,1,1],[1,1,1]], [[0,0,2],[0,0,0],[0,0,0]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,1,1],[1,1,1],[1,1,1]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[1,1,1],[1,1,1],[1,1,1]] ]
    ]

testUBoardWinO = [
       [ [[1,0,2],[1,0,2],[0,0,0]], [[2,2,2],[2,2,2],[2,2,2]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[2,2,2],[2,2,2],[2,2,2]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[2,2,2],[2,2,2],[2,2,2]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

tBoardAlmostX = [
       [ [[1,0,2],[1,0,2],[0,0,0]], [[0,0,2],[0,0,0],[0,0,0]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

tBoardAlmostO = [
       [ [[1,1,2],[1,2,2],[0,1,0]], [[0,0,2],[0,0,0],[0,0,0]], [[0,0,1],[0,0,0],[0,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

tUBoardAlmostX = [
            [ [[1,0,2],[1,0,2],[1,0,0]], [[2,1,2],[0,1,0],[0,1,0]], [[0,2,1],[2,0,1],[2,1,0]] ],
            [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
            [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
         ]

tUBoardAlmostO = [
       [ [[1,0,2],[1,0,2],[2,0,2]], [[2,1,2],[2,0,0],[2,1,0]], [[0,2,1],[2,0,1],[2,1,0]] ],
       [ [[0,0,0],[0,0,0],[0,0,0]], [[1,0,0],[0,2,0],[0,0,0]], [[0,0,0],[0,0,0],[0,0,2]] ],
       [ [[0,0,1],[0,0,0],[0,0,0]], [[0,0,1],[0,0,2],[0,0,0]], [[0,0,0],[0,2,0],[0,0,0]] ]
    ]

tUBoardDraw = [
       [ board6, board6, board6],
       [ board6, board6, board6],
       [ board6, board6, board6]
    ]

tUBoardAlmostDraw = [
       [ [[1,2,1],[2,2,1],[1,1,2]], [[1,2,1],[2,2,1],[0,1,2]], [[1,2,1],[2,2,1],[1,1,2]]],
       [ [[1,2,1],[2,2,1],[1,1,2]], [[1,2,1],[2,2,1],[1,1,2]], [[1,2,1],[2,2,1],[1,1,2]]],
       [ [[1,2,1],[2,2,1],[1,1,2]], [[1,2,1],[2,2,1],[1,1,2]], [[1,2,1],[2,2,1],[1,1,2]]]
    ]

tBoard = [
       [ [[1,0,2],[0,2,0],[2,0,2]], [[2,1,2],[1,1,1],[2,0,2]], [[1,1,1],[0,0,0],[0,0,0]]],
       [ [[2,1,2],[1,1,1],[0,0,2]], [[1,1,2],[1,1,1],[2,0,2]], [[2,0,2],[0,2,0],[2,0,2]]],
       [ [[1,1,2],[1,2,0],[2,0,2]], [[1,1,2],[1,1,0],[2,2,2]], [[1,1,1],[0,0,0],[0,0,0]]]
    ]

tBoardMinimax = [
        [[[1,2,1],[2,2,1],[1,0,2]],[[1,2,1],[2,2,1],[1,1,2]],[[1,2,1],[2,2,1],[1,1,2]]],
        [[[1,2,1],[2,2,1],[1,1,2]],[[1,2,1],[2,2,1],[1,0,2]],[[1,2,1],[2,2,1],[1,1,2]]],
        [[[1,2,1],[2,2,1],[1,1,2]],[[1,2,1],[2,2,1],[1,1,2]],[[1,2,1],[2,2,1],[1,0,2]]]
    ]

emptyUBoardCell = (uBoardToCell emptyUBoard)

board1 = [[0,0,0],[0,0,0],[0,0,0]]
board2 = [[0,1,0],[0,1,0],[0,1,0]]
board3 = [[1,1,1],[0,0,0],[0,0,0]]
board4 = [[1,0,0],[0,1,0],[0,0,1]]
board5 = [[2,0,0],[0,2,0],[0,0,2]]
board6 = [[1,2,1],[2,2,1],[1,1,2]] -- Draw