
module Movimientos
(
    mover,
    peonReina,
    valido,
    primero_cumplir,
    recorridoTor,
    recorridoRei,
    recorridoAlf,
    recorridoPieza,
    movimiento,
    siguiente,
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

{- 2 DEFINICION DE MOVIMIENTOS Y ESTADOS-}

{- 2.1 Funciones para la realizacion del Movimiento -}

--Movemos pieza de la posición (x, y) a la posición (xf, yf) y nos comemos la del rival si está en la posición (xf, yf)
-- se debera realizar la comprobacion del peon reina despues de mover
mover :: Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Matrix String
mover m (x,y) (xf, yf) j = peonReina (setElem ficha (xf, yf) (setElem hueco (x, y) m)) j
    where ficha = getElem x y m
          pos = getElem xf yf m
          hueco = if j == 1 && (elem 'B' pos) then " * " else 
              if elem 'N' pos then " * "  else pos

-- Comprueba si un peon ha llegado al final del tablero y lo convierte a reina
peonReina :: Matrix String -> Int -> Matrix String
peonReina matriz 1 = if pos_N /= 0 then setElem ("QN"++ show cont_N) (8, pos_N) matriz else matriz
    where 
        pos_N = primero_cumplir True [elem 'P' (getElem 8 y matriz) | y<-[1..8] ] 
        cont_N = length (filter (elem 'N') (filter (elem 'Q') (toList matriz)))

peonReina matriz 2 = if pos_B /= 0 then setElem ("QB"++ show cont_B) (1, pos_B) matriz else matriz
    where
        pos_B = primero_cumplir True [elem 'P' (getElem 1 y matriz) |  y<-[1..8] ]
        cont_B = length (filter (elem 'B') (filter (elem 'Q') (toList matriz)))


        {- 2.1.1 Funciones Auxiliares para la Validacion de un movimiento -}
-- Comprobar si el movimiento es válido y no intentamos intercambiar una pieza con nosotros mismos
valido ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Bool
valido m (x,y) (xf, yf) j = if (j==1) then not (elem 'N' sust) && mov else not (elem 'B' sust) && mov
    where sust = getElem xf yf m
          pieza = getElem x y m
          mov = movimiento m (x,y) (xf, yf) j pieza sust
                
-- Obtenermos la posicion del primero en cumplir si es False o True 
-- Utilizada en peonReina para comprobar si existe algun Peon en las posiciones de conversion 
primero_cumplir :: Bool -> [Bool]-> Int
primero_cumplir n xs = if  isNothing (findIndex (==n) xs) then 0 else fromJust (findIndex (==n) xs)

-- Obtecion del recorrio horizontal y vertical y diagon que las torres pueden realizar
recorridoTor :: Matrix String -> (Int, Int) -> (Int, Int) -> [String]
recorridoTor matriz (x,y) (xf,yf)
    | (x==xf) = [getElem x b matriz | b<-[min y yf..max y yf]]
    | otherwise = if(y==yf) then [getElem b y matriz | b<-[min x xf..max x xf]] else ["-"]


-- Obtecion del recorrio horizontal, vertical y diagonal que las reinas pueden realizar
recorridoRei :: Matrix String -> (Int, Int) -> (Int, Int) -> [String]
recorridoRei matriz (x,y) (xf,yf)
    | (x==xf) = [getElem x b matriz | b<-[min y yf..max y yf]]
    | (y==yf) = [getElem b y matriz | b<-[min x xf..max x xf]]
    | (x<xf) && (y<yf)= [getElem a b matriz | (a,b)<- zip [x..xf] [y..yf]]
    | (x<xf) && (y>yf)= [getElem a b matriz | (a,b)<- zip [x..xf] [y,(y-1)..yf]]
    | (x>xf) && (y<yf)= [getElem a b matriz | (a,b)<- zip [x,(x-1)..xf] [y..yf]]
    | otherwise = if ((x>xf)&&(y>yf)) then [getElem a b matriz | (a,b)<- zip [x,(x-1)..xf] [y,(y-1)..yf]] else ["-"]


-- Obtecion del recorrio diagonal que los alfiles pueden realizar
recorridoAlf :: Matrix String -> (Int, Int) -> (Int, Int) -> [String]
recorridoAlf matriz (x,y) (xf,yf)
    | (x<xf) && (y<yf)= [getElem a b matriz | (a,b)<- zip [x..xf] [y..yf]]
    | (x<xf) && (y>yf)= [getElem a b matriz | (a,b)<- zip [x..xf] [y,(y-1)..yf]]
    | (x>xf) && (y<yf)= [getElem a b matriz | (a,b)<- zip [x,(x-1)..xf] [y..yf]]
    | otherwise = if ((x>xf)&&(y>yf)) then [getElem a b matriz | (a,b)<- zip [x,(x-1)..xf] [y,(y-1)..yf]] else ["-"]


-- Comporbacion del estado del recorrido (que este no este ocupado)
recorridoPieza :: Matrix String -> (Int, Int) -> (Int, Int) -> String -> String -> Bool
recorridoPieza matriz (x,y) (xf,yf) pieza sust
    |elem 'T' pieza= and [a== " * " || a == pieza || a == sust  | a <-recorridoTor matriz (x,y) (xf,yf) ]
    |elem 'A' pieza= and [a== " * " || a == pieza || a == sust  | a <-recorridoAlf matriz (x,y) (xf,yf) ]
    |otherwise= and [a== " * " || a == pieza || a == sust  | a <-recorridoRei matriz (x,y) (xf,yf) ]


-- Comprobamos si el movimiento de las diferentes piezas para saber si el movimiento es correcto
movimiento ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> String -> String -> Bool
movimiento matriz (x,y) (xf, yf) j pieza sust
    |elem 'T' pieza = if  (x == xf || y == yf) then recorridoPieza matriz (x,y) (xf, yf) pieza sust else False
    |elem 'C' pieza =((abs (xf -x)) == 2 && (abs (yf - y)) == 1) || ((abs (xf -x)) == 1 && (abs (yf - y)) == 2)
    |elem 'K' pieza =((abs (xf -x)) <= 1 && (abs (yf - y)) <= 1)
    |elem 'A' pieza = if((abs (x-xf))==(abs(yf-y))) then recorridoPieza matriz (x,y) (xf, yf) pieza sust else False
    |elem 'Q' pieza = if( ((abs (x-xf))==(abs(yf-y))) || (x == xf || y == yf) ) then recorridoPieza matriz (x,y) (xf, yf) pieza sust else False
    |elem 'P' pieza = if j==1 then if elem 'B' sust then (xf-x==1) && abs(yf - y)==1 else (xf-x==1) && yf==y 
                    else if elem 'N' sust then (x - xf==1) && abs(yf - y)==1 else (x - xf==1) && yf==y
    |otherwise = False
-- otherwise False para los " * "

{- 2.2 Funcion para el Cambio de Jugador -}

--Siguiente jugador
siguiente :: Int -> Int
siguiente j = 1 + (mod j 2)
