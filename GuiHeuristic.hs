module GuiHeuristic (gui_heuristic) where

import TypeDef
import UltimateTicTacToe

type ParsedUBoard = [[[[Double]]]]
type Parameter = Double


-- Parameter 1
-- Degree of significance for a comb with 2 opposite player dominated squares. 
-- Shoud that combination be included in the algorithm? 
-- High value means its more stric, so  a small difference will desquilify the row 

-- Parameter 2
-- Degree of importance for high comb values. 
-- How important is a comb with +1 compared to a +2, and a +2.5
-- High values place little significance in changes close to 0 and a lot in changes close to +3 

gui_heuristic :: (Parameter, Parameter) -> Heuristic
gui_heuristic p state = uboard_score p (parse_state state)

-- converts all cell of ps (player symbol) into 1, all empty cells into 0, 
-- and all oppiste player cells into -1 
parse_state :: State -> ParsedUBoard
parse_state (State ub _ ps) = [parse_ubr ps ubr | ubr <- ub]
    where 
        parse_ubr ps ubr = [parse_b ps b | b <- ubr]
        parse_b ps b = [parse_br ps br | br <- b]
        parse_br ps br = [parse_s ps s| s <- br]
        parse_s ps Nothing = 0
        parse_s ps (Just s) = if (ps==s) then 1 else (-1)

pair_sig :: Parameter -> (Double, Double) -> Double
pair_sig p1 (x,y)
    | absv<1 = 1
    | otherwise = absv**p1
    where absv = abs (x-y)

comb_val :: Parameter -> [Double] -> Double
comb_val p1 [x,y,z] = (((v1*s1)+(v2*s2))+(v3*s3))/((s1+s2)+s3) 
    where
        ps = pair_sig p1
        s1 = ps (x,y)
        s2 = ps (x,z)
        s3 = ps (y,z)
        v1 = (x+y)/2
        v2 = (x+z)/2
        v3 = (y+z)/2

ub_comb_val :: (Parameter, Parameter) -> [[[Double]]] -> Double
ub_comb_val (p1,p2) ubc = comb_val p1 [board_score (p1,p2) b | b <- ubc]

win_combs_score :: Parameter -> ([a] -> Double) -> [[a]] -> [Double]
win_combs_score p2 cvf b = [comb_score p2 (cvf c) | c <- (win_combs b)]

win_combs :: [[a]] -> [[a]]
win_combs [[a,b,c],
           [d,e,f],
           [g,h,i]] = [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]

comb_score :: Parameter -> Double -> Double
comb_score p2 cv 
    | cv < 0 = (- ((abs cv)**p2))
    | otherwise = (cv)**p2

sum_scores :: ([[a]] -> [Double]) -> [[a]] -> Double
sum_scores wcsf b = max (min (foldl (+) 0 (wcsf b)) 1) (-1)

board_score :: (Parameter, Parameter) -> ([[Double]] -> Double)
board_score (p1,p2) =  sum_scores (win_combs_score p2 (comb_val p1) )

uboard_score :: (Parameter, Parameter) -> (ParsedUBoard -> Double)
uboard_score (p1,p2) = sum_scores (win_combs_score p2 (ub_comb_val (p1,p2)))
