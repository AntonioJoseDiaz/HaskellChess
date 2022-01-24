module Tablero(
    tablero,
    vacio,
    listaPosiciones,
    creaTablero,
    finalizado,
    fichaValida,
    esFila
) where

import Data.Maybe
import Data.List
import Data.Matrix
import Data.Char
import Data.String
import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Array


{-- 1 DEFINICION DEL TABLERO JUNTO A SUS PIEZAS --}

fnQ = ["TN1", "CN1", "AN1", "QN0", "KN ", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8"]
fbQ = ["TB1", "CB1", "AB1", "KB ", "QB0", "AB2", "CB2", "TB2"]

{- 1.1 Creacion del Tablero -}
-- Se creara una constante Tablero que se usara de forma que sea la primera instancia de la partida
tablero :: Matrix String
tablero = creaTablero 4


{- 1.2 Funciones para crear el Tablero -}

-- Crea n lineas vacías (posiciones sin ficha en el tablero)
vacio :: Int -> [[String]]
vacio n = replicate n [" * " | x<-[1..8]]
-- función con lista por compresión


-- Crea una lista con todas las filas del tablero (con n == filas vacias)
listaPosiciones :: Int -> [[String]]
listaPosiciones n
    | n <=3 = []
    |otherwise = [fnQ, fnP] ++ vacio n ++ [fbP, fbQ]
--función con uso de case of

-- Crea el tablero como una de forma Maticial 
creaTablero :: Int -> Matrix String
creaTablero n  
    |n<=3 = fromLists [["ERROR : numero de lineas invalido, debe ser >=4"]]
    |otherwise =fromLists pos
    where pos = listaPosiciones n
--función con uso de case of



{- 1.3 Funciones de comprobacion de estado del Tablero -}

-- Comprobacion si el juego ha terminado
finalizado :: Matrix String -> Int -> Bool
finalizado m 1 = not(or ["KB " == (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])
finalizado m 2 = not(or ["KN " == (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])

-- Comprobamos si la pieza que queremos mover se encuentra entre las piezas actuales en juego 
-- (comprobacion entre TUS FICHAS, Comprobacion completa Toda la ficha)
fichaValida :: String -> [String] -> Bool
fichaValida x piezas = or [x==y| y<-piezas]

-- Comprobar si la ficha se encuentra en la fila
esFila :: String -> [String] ->Bool
esFila x xs = not(or [x==y| y <-xs])
