module UltimateTicTacToe where

import TypeDef

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
put_board board row col cell = [if row == i then (put_h x col cell) else x | (i, x) <- indexer board]

-- replaces the cell in row_c at col with cell value
put_h :: [CellState] -> Int -> CellState -> [CellState]
put_h row_c col cell = [if col == i then cell else x
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
