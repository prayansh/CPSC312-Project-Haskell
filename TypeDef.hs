module TypeDef where

data Symbol = X | O
    deriving (Eq, Show, Read)
type Cell = Maybe Symbol -- Nothing for blank
type Board = [[Cell]]
type UltimateBoard = [[Board]]

-- | Result is either EndOfGame with winning symbol
-- |           or ContinueGame with new state
data Result = EndOfGame (Maybe Symbol) State     -- end of game, winning symbol, starting state
            | ContinueGame State            -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> String -> Action

-- | Action is either PlaceAt with row, col
-- |               or ChooseBoard with index [0..8]
-- |               or Invalid
data Action = PlaceAt Int Int
              | ChooseBoard Int
              | Invalid
         deriving (Eq, Show)

-- | State represents the current board, and current active board, current(next) player
data State = State UltimateBoard Int Symbol
        deriving (Eq, Show)

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

emptyUBoardCell = (uBoardToCell emptyUBoard)

board1 = [[0,0,0],[0,0,0],[0,0,0]]
board2 = [[0,1,0],[0,1,0],[0,1,0]]
board3 = [[1,1,1],[0,0,0],[0,0,0]]
board4 = [[1,0,0],[0,1,0],[0,0,1]]
board5 = [[2,0,0],[0,2,0],[0,0,2]]