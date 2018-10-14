module UltimateTicTacToe where

import TypeDef
import Data.Maybe
import GameHelper

---- Game

-- | Ultimate Tic Tac Toe Game
ultimateTicTacToe :: Game
ultimateTicTacToe Invalid s = (ContinueGame s)
ultimateTicTacToe (ChooseBoard b) (State ub actB nextP) = ContinueGame (State ub b nextP)
ultimateTicTacToe (PlaceAt row col) (State ub actB nextP)
    | is_u_board_draw nextUBoard = EndOfGame Nothing (State nextUBoard (-1) nextP)
    | winner == Nothing = ContinueGame (State nextUBoard nextActiveBoard (nextSymbol nextP))
    | otherwise = EndOfGame winner (State nextUBoard (-1) nextP)
    where
        nextUBoard = put_u_board ub actB row col (Just nextP)
        winner = get_winner_u_board nextUBoard
        index = (3 * row) + col
        nextActiveBoard = if (elem index (get_playable_boards nextUBoard))
                            then index
                            else (-1)

---- Game Logic Functions

-- | put cell in position (row,col) at b_no in u_board
put_u_board :: UltimateBoard -> Int -> Int -> Int -> Cell -> UltimateBoard
put_u_board u_board b_no row col cell =
      [if (quot b_no 3) == i
       then (put_u_board_h x (rem b_no 3) row col cell)
       else x
       | (i, x) <- indexer u_board]
put_u_board_h boards b_no row col cell = [if b_no == i then (put_board x row col cell)
                                          else x |
                                          (i, x) <- indexer boards]

-- | put cell in position (row,col) in board
put_board :: Board -> Int -> Int -> Cell -> Board
put_board board row col cell = [if row == i then (put_h x col cell)
                                    else x |
                                    (i, x) <- indexer board]

-- | replaces the cell in row_c at col with cell value
put_h :: [Cell] -> Int -> Cell -> [Cell]
put_h row_c col cell = [if col == i then cell else x
                        | (i, x) <- indexer row_c]

-- | function to check if valid move
is_valid_move :: UltimateBoard -> Int -> Int -> Int -> Bool
is_valid_move u_board b_no row col = ((((concat u_board)!!b_no)!!row)!!col) == Nothing

-- | Function to check if board is won
is_board_won :: Board -> Bool
is_board_won board = not (isNothing $ get_winner_board board)

-- | Function to check if uBoard is draw
is_u_board_draw :: UltimateBoard -> Bool
is_u_board_draw uBoard = and (map (\x -> is_board_draw x || is_board_won x) (concat uBoard))

-- | Function to check if board is draw
is_board_draw :: Board -> Bool
is_board_draw board = null [c | c <- concat board, isNothing c]

-- | function to get list of playable boards
get_playable_boards :: UltimateBoard -> [Int]
get_playable_boards ub =
  [ i
  | (i, b) <- (indexer $ concat ub)
  , (not $ is_board_won b) && (not $ is_board_draw b)
  ]

-- | function to check who won u_board
get_winner_u_board :: UltimateBoard -> Maybe Symbol
get_winner_u_board ub = get_winner_board $ u_board_to_board ub

-- | Function to represent big board as a smaller board with winners as cell values
u_board_to_board :: UltimateBoard -> Board
u_board_to_board ub = (split3 (map get_winner_board (concat ub)))

-- | function to check who won small_board
get_winner_board :: Board -> Maybe Symbol
get_winner_board board = head $ [x |
                                    x <- (map get_winner $ gen_wins_rows board),
                                    x/=Nothing]++[Nothing]

-- | return the winner given a row
get_winner :: [Cell] -> Maybe Symbol
get_winner [Just X, Just X, Just X] = Just X
get_winner [Just O, Just O, Just O] = Just O
get_winner _                        = Nothing

-- | gen_wins_rows flattens the board into a list of rows that can be won
gen_wins_rows :: Board -> [[Cell]]
gen_wins_rows board = horiz ++ vert ++ diag
   where horiz = [r | r <- board]
         col1  = [head r | r <- board]
         col2  = [head (drop 1 r) | r <- board]
         col3  = [head (drop 2 r) | r <- board]
         vert  = [col1, col2, col3]
         diag  = [[head col1, head (drop 1 col2), head (drop 2 col3)]
                 ,[head (drop 2 col1), head (drop 1 col2), head col3]]

-- | return the list of valid actions for given ub
get_valid_actions :: UltimateBoard -> Int -> [Action]
-- return chooseBoard actions
get_valid_actions ub (-1) = [ChooseBoard i |
                                (i,b)<-(indexer $ concat ub),
                                (not $ is_board_won b) && (not $ is_board_draw b)]
-- return placeAt actions
get_valid_actions ub actB
    | is_board_won b = [] -- Board is already won, so no valid moves
    | otherwise = [PlaceAt (quot i 3) (rem i 3) | (i,cell) <- indexer $ concat b, isNothing cell]
    where b = (concat ub) !! actB
