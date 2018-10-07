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

board1 = [[0,0,0],[0,0,0],[0,0,0]]

test = draw_ultimate_board testUBoard 4

-- returns an indexed tuple for each element in the lst
indexer lst = zip [0..] lst

-- put cell in position (row,col) at b_no in u_board
put_u_board u_board b_no row col cell =
      [if (quot b_no 3) == i then (put_u_board_h x (rem b_no 3) row col cell) else x | (i, x) <- indexer u_board]
put_u_board_h boards b_no row col cell = [if b_no == i then (put_board x row col cell) else x|(i, x) <- indexer boards]

put_board :: [[CellState]]-> Int-> Int -> CellState -> [[CellState]]
-- put cell in position (row,col) in board
put_board board row col cell = [if row == i then (put_h x col cell)
                              else x | (i, x) <- indexer board]  
put_h row_c col cell = [if col == i then cell else x | (i, x) <- indexer row_c] 
