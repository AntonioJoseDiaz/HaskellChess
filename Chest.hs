--hi
import Data.List
import Data.Matrix

fnQ = ["TN1", "CN1", "AN1", "QN", "KN", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8"]
fbQ = ["TB1", "CB1", "AB1", "KB", "QB", "AB2", "CB2", "TB2"]

vacio = [" * " | x<-[1..8]]

listaPosiciones :: Int -> [[String]]
listaPosiciones 0 = []
listaPosiciones n = [fnQ, fnP] ++ replicate n vacio ++ [fbP, fbQ]

tablero :: Int -> Matrix String

tablero n  
    |n<=3 = fromLists [["ERROR : numero de lineas invalido, debe ser >=4"]]
    |otherwise =fromLists pos
    where pos = listaPosiciones n

