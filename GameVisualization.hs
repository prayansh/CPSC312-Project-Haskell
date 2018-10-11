module GameVisualization where

import UltimateTicTacToe

type DrawingLine = String
data Drawing = Drawing [DrawingLine]
instance Show Drawing where
  show (Drawing d) = (foldl (\str acc -> str++['\n']++acc ) "" d)

-- Drawing Generators-------------------------------------------------------------------

get_ultimate_board_drawing :: UltimateBoard -> Int -> Drawing 
get_ultimate_board_drawing [r1,r2,r3] ai = Drawing(
                           (get_ub_row_drawing r1 "ABC" ai)++
                           (get_ub_row_drawing r2 "DEF" (ai-3))++
                           (get_ub_row_drawing r3 "GHI" (ai-6)))

get_ub_row_drawing :: [Board] -> [Char] -> Int -> [DrawingLine]
get_ub_row_drawing [x,y,z] [a,b,c] ai = [(\i -> (
                                            (get_board_drawing x (ai==0) a (board_winner x))!!i ++" "++
                                            (get_board_drawing y (ai==1) b (board_winner y))!!i ++" "++
                                            (get_board_drawing z (ai==2) c (board_winner z))!!i)) i | i <- [0..9]]

get_board_drawing :: Board -> Bool -> Char -> CellState -> [DrawingLine]
get_board_drawing [r1,r2,r3] act name 0 = [
    (border act),                        -- 0  .....................
    (labels act name),                   -- 1  : [A] 1   2   3     :
    (separator act),                     -- 2  :   |---|---|---|   :
    (row act '1' (row_chars r1)),        -- 3  : 1 | x |   | x |   :      
    (separator act),                     -- 4  :   |---|---|---|   :
    (row act '2' (row_chars r2)),        -- 5  : 2 | o |   |   |   :
    (separator act),                     -- 6  :   |---|---|---|   :
    (row act '3' (row_chars r3)),        -- 7  : 3 |   | o |   |   :
    (separator act),                     -- 8  :   |---|---|---|   :
    (border act)]                        -- 9  .....................

get_board_drawing _ act name winner = [
    (border act),                        -- 0  .....................
    (won_labels act name),               -- 1  : [A]               :
    (separator act),                     -- 2  :   |---|---|---|   :
    (won_row act ' ' ),                  -- 3  :   |           |   :      
    (empty_separator act),               -- 4  :   |           |   :
    (won_row act (to_char winner) ),     -- 5  :   |     x     |   :
    (empty_separator act),               -- 6  :   |           |   :
    (won_row act ' ' ),                  -- 7  :   |           |   :
    (separator act),                     -- 8  :   |---|---|---|   :
    (border act)]                        -- 9  .....................

-- Helpers -------------- --------------------------------------------------------------

row_chars :: [CellState] -> [Char]
row_chars r = [to_char x | x <- r]

to_char :: CellState -> Char
to_char 0 = ' '
to_char 1 = 'x'
to_char 2 = 'o'

board_winner:: Board -> CellState
board_winner [[a,b,c],
              [d,e,f],
              [h,i,j]] = maximum cWinners -- assumes there will be one winner only 
              where 
                combinations = [[a,b,c],[d,e,f],[h,i,j],[a,d,h],[b,e,i],[c,f,j],[a,e,j],[c,e,h]]
                cWinners = map (\c -> if (all (==head c) c) then a else 0) combinations


-- DrawingLine Generators --------------------------------------------------------------

border :: Bool -> DrawingLine
border act = if act then "....................." 
                    else "                     "

labels :: Bool -> Char -> DrawingLine
labels act name = add_vertical_border act (" ["++[name]++"] 1   2   3     ") 

won_labels :: Bool -> Char -> DrawingLine
won_labels act name = add_vertical_border act (" ["++[name]++"]               ") 

separator :: Bool -> DrawingLine
separator act = add_vertical_border act "   |---|---|---|   " 

empty_separator :: Bool -> DrawingLine
empty_separator act = add_vertical_border act "   |           |   " 

row :: Bool -> Char -> [Char] -> DrawingLine
row act i [a,b,c] = add_vertical_border act (" "++[i]++" | "++[a]++" | "++[b]++" | "++[c]++" |   ")

won_row :: Bool -> Char -> DrawingLine
won_row act winner = add_vertical_border act ("   |     "++[winner]++"     |   ")

add_vertical_border :: Bool -> DrawingLine -> DrawingLine
add_vertical_border act str = if act then ":"++str++":" else " "++str++" "

----------------------------------------------------------------------------------------

draw_ultimate_board :: UltimateBoard -> Int -> IO ()
draw_ultimate_board ub ai = draw (get_ultimate_board_drawing ub ai)

draw :: Drawing -> IO ()
draw d = do 
    putStr "\ESC[2J" -- clears terminal
    putStrLn (show d)

----------------------------------------------------------------------------------------

test = get_ultimate_board_drawing testUBoard 5

test2 = get_ultimate_board_drawing testUBoard 5
