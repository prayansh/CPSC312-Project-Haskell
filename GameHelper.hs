module GameHelper where

import TypeDef

get_valid_actions :: UltimateBoard -> Int -> [Action]
get_valid_actions ub (-1) = [(ChooseBoard i) | i <- (get_valid_boards ub)]
get_valid_actions ub actB =  [(PlaceAt r c) | (r,c) <- (foldr (++) [] valid_rows)]
    where 
        board = (ub!!(div actB 3))!!(mod actB 3)
        valid_rows = [valid_cells (r,ri) | (r,ri) <- (zip board [0..2])]
        valid_cells (r,ri) = [(ri,ci) | (c,ci) <- (zip r [0..2]), (c==Nothing)]

get_valid_boards :: UltimateBoard -> [Int]
get_valid_boards ub = [ i | (i,available) <- (zip [0..8] [(==Nothing) x | x <- (foldr (++) [] (uboard_winners ub))]), available]

uboard_winners :: UltimateBoard -> [[Cell]]
uboard_winners ub = [row_winners r | r <- ub]
    where row_winners row = [board_winner b | b <- row]

board_winner:: Board -> Cell
board_winner [[a,b,c],
              [d,e,f],
              [h,i,j]] = head cWinners -- assumes there will be one winner only
              where 
                combinations = [[a,b,c],[d,e,f],[h,i,j],[a,d,h],[b,e,i],[c,f,j],[a,e,j],[c,e,h]]
                cWinners = map (\c -> if (all (==(head c)) c) then (head c) else Nothing) combinations