module GameVisualization where

type CellState = Integer
type Board = [[CellState]]
type UltimateBoard = [[Board]]

type Drawing = [String]
type DrawingLine = String

get_ultimate_board_drawing :: UltimateBoard -> Int -> Drawing
get_ultimate_board_drawing [r1,r2,r3] ai = (get_ub_row_drawing r1 "ABC" ai)++
                           (get_ub_row_drawing r2 "DEF" (ai-3))++
                           (get_ub_row_drawing r3 "GHI" (ai-6))

get_ub_row_drawing :: [Board] -> [Char] -> Int -> Drawing
get_ub_row_drawing [x,y,z] [a,b,c] ai = [(\i -> ((get_board_drawing x (ai==0) a)!!i ++" "++
                                            (get_board_drawing y (ai==1) b)!!i ++" "++
                                            (get_board_drawing z (ai==2) c)!!i)) i | i <- [0..9]]

get_board_drawing :: Board -> Bool -> Char -> Drawing
get_board_drawing [r1,r2,r3] act name = [
    (border act),                        -- 0  .....................
    (labels act name),                   -- 1  : [A] 1   2   3     :
    (separator act),                     -- 2  :   |---|---|---|   :
    (row act '1' (row_chars r1)),        -- 3  : 1 |   |   |   |   :
    (separator act),                     -- 4  :   |---|---|---|   :
    (row act '2' (row_chars r2)),        -- 5  : 2 |   |   |   |   :
    (separator act),                     -- 6  :   |---|---|---|   :
    (row act '3' (row_chars r3)),        -- 7  : 3 |   |   |   |   :
    (separator act),                     -- 8  :   |---|---|---|   :
    (border act)]                        -- 9  .....................

row_chars :: [CellState] -> [Char]
row_chars r = [tc x | x <- r] where
    tc 0 = ' '
    tc 1 = 'x'
    tc 2 = 'o'

border :: Bool -> DrawingLine
border act = if act then "....................."
                      else "                     "

labels :: Bool -> Char -> DrawingLine
labels act name = add_vertical_border act (" ["++[name]++"] 1   2   3     ")

separator :: Bool -> DrawingLine
separator act = add_vertical_border act "   |---|---|---|   "

row :: Bool -> Char -> [Char] -> DrawingLine
row act i [a,b,c] = add_vertical_border act (" "++[i]++" | "++[a]++" | "++[b]++" | "++[c]++" |   ")

add_vertical_border :: Bool -> DrawingLine -> DrawingLine
add_vertical_border act str = if act then ":"++str++":" else " "++str++" "



draw_ultimate_board :: UltimateBoard -> Int -> IO ()
draw_ultimate_board ub ai = draw (get_ultimate_board_drawing ub ai)

draw :: Drawing -> IO ()
draw d = do
    putStr "\ESC[2J" -- clears terminal
    mapM_ putStrLn d



-- --------------------------------------------------------------------------------------------

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

board1 = [[0,0,0],[0,0,0],[0,0,0]]
board2 = [[0,1,0],[0,1,0],[0,1,0]]
board3 = [[1,1,1],[0,0,0],[0,0,0]]
board4 = [[1,0,0],[0,1,0],[0,0,1]]

test = draw_ultimate_board testUBoard 4

-- put cell in position (row,col) at b_no in u_board
put_u_board :: UltimateBoard -> Int -> Int -> Int -> CellState -> UltimateBoard
put_u_board u_board b_no row col cell =
      [if (quot b_no 3) == i
       then (put_u_board_h x (rem b_no 3) row col cell)
       else x
       | (i, x) <- indexer u_board]
put_u_board_h boards b_no row col cell = [if b_no == i
                                          then (put_board x row col cell)
                                          else x
                                          |(i, x) <- indexer boards]

-- put cell in position (row,col) in board
put_board :: Board -> Int -> Int -> CellState -> Board
put_board board row col cell = [if row == i
                                then (put_h x col cell)
                                else x | (i, x) <- indexer board]

-- replaces the cell in row_c at col with cell value
put_h :: [CellState] -> Int -> CellState
put_h row_c col cell = [if col == i
                        then cell
                        else x
                        | (i, x) <- indexer row_c]

-- function to make small board have all 1's or 2's depending on who won
fill_win_u :: UltimateBoard -> Int -> CellState -> UltimateBoard
fill_win_u u_board b_no val = split3 $ [ if (i == b_no)
                                        then (fill_win b val)
                                        else b
                                        | (i, b) <- (indexer (concat u_board))]

fill_win :: Board -> CellState -> Board
fill_win board val = [[val,val,val]| r<-board]

-- function to check if valid move
is_valid_move u_board b_no row col = ((((concat u_board)!!b_no)!!row)!!col) == 0

-- function to check if board is won

-- function to check who won u_board
get_winner_u_board u_board = get_winner_board (split3 (map get_winner_board (concat u_board)))

-- function to check who won small_board
get_winner_board board = head $ [x | x<-(map get_winner $ make_win_rows board), x/=0]++[0]

-- get_winner returns the winner given a row
get_winner [1,1,1] = 1
get_winner [2,2,2] = 2
get_winner _       = 0

-- make_win_rows flattens the board into a list of rows that can be won
make_win_rows board = horiz ++ vert ++ diag
   where horiz = [r | r <- board]
         col1  = [head r | r <- board]
         col2  = [head (drop 1 r) | r <- board]
         col3  = [head (drop 2 r) | r <- board]
         vert  = [col1, col2, col3]
         diag  = [[head col1, head (drop 1 col2), head (drop 2 col3)]
                 ,[head (drop 2 col1), head (drop 1 col2), head col3]]

-- Helper Functions

-- returns an indexed tuple for each element in the lst
indexer :: [a] -> [(Int, a)]
indexer lst = zip [0..] lst

-- convert lst to seperate at 3
split3 :: [a] -> [[a]]
split3 [] = []
split3 (x:y:z:lst) = [x,y,z]:(split3 lst)
