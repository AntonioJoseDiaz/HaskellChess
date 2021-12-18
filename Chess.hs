--hi
import Data.List
import Data.Matrix

fnQ = ["TN1", "CN1", "AN1", "QN", "KN", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8"]
fbQ = ["TB1", "CB1", "AB1", "KB", "QB", "AB2", "CB2", "TB2"]

tablero :: Matrix String
tablero = creaTablero 4

vacio :: Int -> [[String]]
vacio n = replicate n [" * " | x<-[1..8]]
-- función con lista por compresión

listaPosiciones :: Int -> [[String]]
listaPosiciones n
    | n <=3 = []
    |otherwise = [fnQ, fnP] ++ vacio n ++ [fbP, fbQ]
--función con uso de case of


creaTablero :: Int -> Matrix String
creaTablero n  
    |n<=3 = fromLists [["ERROR : numero de lineas invalido, debe ser >=4"]]
    |otherwise =fromLists pos
    where pos = listaPosiciones n
--función con uso de case of

finalizado :: Matrix String -> Int -> Bool
finalizado m 1 = not(or [elem 'B' (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])
finalizado m 2 = not(or [elem 'N' (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])

mover :: Matrix String -> (Int, Int) -> (Int, Int) -> Matrix String
mover m (x,y) (xf, yf) = setElem ficha (xf, yf) (setElem hueco (x, y) m)
    where ficha = getElem x y m
          hueco = getElem xf yf m


valido ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Bool
valido m (x,y) (xf, yf) 1= ((getElem xf yf m) == " * ") || elem 'B' (getElem xf yf m)
valido m (x,y) (xf, yf) 2= ((getElem xf yf m) == " * ") || elem 'N' (getElem xf yf m)