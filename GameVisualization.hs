module GameVisualization where

import TypeDef

type DrawingLine = String
data Drawing = Drawing [DrawingLine]
instance Show Drawing where
  show (Drawing d) = (foldl (\str acc -> str++['\n']++acc ) "" d)

-- Drawing Generators-------------------------------------------------------------------

get_ultimate_board_drawing :: UltimateBoard -> Int -> [[Cell]] -> Drawing
get_ultimate_board_drawing [r1,r2,r3] actB [bws1,bws2,bws3] = Drawing(
                           (get_ub_row_drawing r1 "ABC" actB bws1)++
                           (get_ub_row_drawing r2 "DEF" (actB-3) bws2)++
                           (get_ub_row_drawing r3 "GHI" (actB-6)) bws3)

get_ub_row_drawing :: [Board] -> [Char] -> Int -> [Cell] -> [DrawingLine]
get_ub_row_drawing [x,y,z] [a,b,c] ai [bw1,bw2,bw3] = [(\i -> (
                                        (get_board_drawing x (ai==0) a bw1)!!i ++" "++
                                        (get_board_drawing y (ai==1) b bw2)!!i ++" "++
                                        (get_board_drawing z (ai==2) c bw3)!!i)) i | i <- [0..9]]

get_board_drawing :: Board -> Bool -> Char -> Cell -> [DrawingLine]
get_board_drawing [r1,r2,r3] act name Nothing = [
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
    (won_row act (cellToChar winner) ),  -- 5  :   |     x     |   :
    (empty_separator act),               -- 6  :   |           |   :
    (won_row act ' ' ),                  -- 7  :   |           |   :
    (separator act),                     -- 8  :   |---|---|---|   :
    (border act)]                        -- 9  .....................

-- Helpers -------------- --------------------------------------------------------------

row_chars :: [Cell] -> [Char]
row_chars r = [cellToChar x | x <- r]

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

draw_ultimate_board :: UltimateBoard -> Int -> [[Cell]] -> IO ()
draw_ultimate_board ub ai bws = draw (get_ultimate_board_drawing ub ai bws)

draw :: Drawing -> IO ()
draw d = do 
    -- putStr "\ESC[2J" -- clears terminal
    putStrLn (show d)



