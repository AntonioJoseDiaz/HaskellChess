module Juegos
(
    leeDigito,
    leeModo,
    leeFicha,
    posicionXFicha,
    posicionYFicha,
    fichasJugador,
    fichasLinea,
    filaString,
    escribeFila,
    escribeTablero,
    jugar,
    juego,
    juegoIA,
    aleatorio,
    mueveIA
) where

import Tablero
import Movimientos

import Data.Maybe
import Data.List
import Data.Matrix
import Data.Char
import Data.String
import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Array

{- 3 DEFINICION DE MODOS DE JUEGO-}

{- 3.1 Funciones para la Lectura del los parametros propuestos por el usuario -}

-- Leemos la posicion (xf,yf) al que queremos mover la pieza
leeDigito :: String -> IO Int
leeDigito c = do
    putStr c
    ts <- getLine
    let d = head ts
    if (not.null) ts && isDigit d then
        return (digitToInt d)
    else do
        putStrLn "ERROR: Entrada incorrecta"
        leeDigito c


-- Leemos el modo de juego
leeModo :: String -> IO String
leeModo c = do
    putStr c
    ts <- getLine
    if (ts=="PvP" || ts=="PvC") then
        return ts
    else do
        putStrLn "ERROR: Entrada incorrecta"
        leeModo c

-- Leemos la pieza a mover introducida y comprobamos si es válida
leeFicha :: String  -> [String] ->IO String
leeFicha c piezas= do
    putStr c
    ts <- getLine
    let ficha = if elem 'K'ts then ts++" " else ts
    if (not.null) ficha && fichaValida ficha piezas then
        return ficha
    else do
        putStrLn "ERROR: Ficha incorrecta"
        leeFicha c piezas

{- 3.2 Funciones auxiliares para la Obtencion de Filas, Columnas o Fichas-}

--Obtener nº de fila en la que se encuentra la ficha
posicionXFicha :: Matrix String -> String -> Int
posicionXFicha m x = xp
    where filas = toLists m
          antes = takeWhile (esFila x) filas
          xp = length antes +1

--Obtener nº de columna en la que se encuentra la ficha
posicionYFicha :: Matrix String -> String -> Int -> Int
posicionYFicha m x xp = yp + 1
    where filas = toLists m
          linea=filas !!(xp -1)
          yp = sum[1 | s<- takeWhile (/=x) linea]
          
-- Obtenemos la lista de piezas en el tablero del jugador
fichasJugador :: Matrix String -> Int -> [String]
fichasJugador m j =piezas
     where lista = toLists m
           piezas = foldr f [] lista
           f xs zs 
                | length xs == 0 = zs
                | otherwise = zs ++ (fichasLinea xs j) 


-- Función auxiliar para conseguir las piezas del jugador en la línea indicada
fichasLinea :: [String] -> Int -> [String]
fichasLinea xs j = foldr f [] xs
    where f x zs 
            | j == 1 && (elem 'N' x) = (x:zs)
            | j == 2 && (elem 'B' x) = (x:zs)
            | otherwise = zs


{- 3.3 Funciones para la Escritura Por Pantalla del Tablero -}

--Convertimos la fila a un String para poderla imprimir
filaString :: String -> [String] -> String
filaString a (x:xs)
    | length xs == 0 = (a ++" | " ++ x)
    |otherwise = filaString (a ++" | " ++ x) xs

--Escribimos la fila por pantalla
escribeFila :: Int -> [String]-> IO ()
escribeFila f n = putStrLn $ show f ++ ": " ++ (filaString " " n)

--Imprimimos el tablero por pantalla
escribeTablero :: Matrix String -> IO ()
escribeTablero t = do 
    sequence_ [escribeFila i n | (i,n)<-zip [1 ..] filas]
        where filas = toLists t


{- 3.4 Funciones de Manejo del Juego-}

jugar :: IO()
jugar= do
    putStrLn "Seleccionar modo de juego: PvP o PvC"
    modo <- leeModo "Modo de juego: "
    if (modo == "PvP") then do
        juego tablero 2
    else do
        putStrLn "Tus fichas de juego son las NEGRAS\n"
        now <- getCurrentTime
        timezone <- getCurrentTimeZone
        let (TimeOfDay _ _ second) = localTimeOfDay $ utcToLocalTime timezone now
        
        if even (round second) then do 
             juegoIA tablero 2 (round second)
        else do
            juegoIA tablero 2 ((round second) +1)
        
{- 3.4.1 Funciones de Modo de Juego -}

juego :: Matrix String -> Int -> IO ()
juego t j = do
    putStr " \n"
    putStrLn "        1     2     3     4     5     6     7     8"
    escribeTablero t
    putStr " \n "
    putStrLn $ "J " ++ show ((mod j 2)+1)

    let piezasJugador = fichasJugador t j

    f <- leeFicha "Elije una ficha: " piezasJugador
    let x = posicionXFicha t f
    let y = posicionYFicha t f x
    xf <- leeDigito "Elije fila de movimiento: "
    yf <- leeDigito "Elije columna de movimiento: "



    if (1 <= xf &&  xf <= 8 && 1 <= yf &&  yf <= 8) then do

        if (valido t (x, y) (xf, yf) j) then do
            let t2 = mover t (x, y) (xf, yf) j
            
            if finalizado t2 j then do
                putStr " \n"
                putStrLn "        1     2     3     4     5     6     7     8"
                escribeTablero t2
                putStrLn $ " \n J " ++ show ((mod j 2)+1) ++ " ha ganado!"
            else do
             --   putStr "\ESC[2J"
                let j2 = siguiente j
                juego t2 j2
        else do
           -- putStr "\ESC[2J"
            putStrLn " \n Movimiento no valido "
            juego t j
            
    else do
       -- putStr "\ESC[2J"
        putStrLn $ " \n Se ha exedido el rango de la matriz " ++ show xf ++" "++ show yf
        juego t j

juegoIA :: Matrix String -> Int -> Int -> IO ()
juegoIA t j al= do

    if (j==2) then do
        let piezasJugador = fichasJugador t 2
        let ti = mueveIA t piezasJugador al
        let j2 = siguiente j
        putStrLn "Siguiente turno \n \n"
        juegoIA ti j2 (al+2)
    else do
        putStr " \n"
        putStrLn "        1     2     3     4     5     6     7     8"
        escribeTablero t
        putStr " \n "
        putStrLn $ "Jugador " ++ show j

        let piezasJugador = fichasJugador t j

        f <- leeFicha "Elije una ficha: " piezasJugador
        let x = posicionXFicha t f
        let y = posicionYFicha t f x
        xf <- leeDigito "Elije fila de movimiento: "
        yf <- leeDigito "Elije columna de movimiento: "

        if (1 <= xf &&  xf <= 8 && 1 <= yf &&  yf <= 8) then do
            if (valido t (x, y) (xf, yf) j) then do
                let t2 = mover t (x, y) (xf, yf) j
                if finalizado t2 j then do
                    putStr " \n"
                    putStrLn "        1     2     3     4     5     6     7     8"
                    escribeTablero t2
                    putStrLn $ " \n J " ++ show j ++ " ha ganado!"
                else do
                    putStr "\ESC[2J"
                    let j2 = siguiente j
                    putStrLn "        1     2     3     4     5     6     7     8"
                    escribeTablero t2
                    juegoIA t2 j2 al
            else do
                putStr "\ESC[2J"
                juegoIA t j al
        else do
            putStr "\ESC[2J"
            putStrLn $ " \n Se ha exedido el rango de la matriz " ++ show xf ++" "++ show yf
            juegoIA t j al

{- 3.4.2 Funciones Auxiliares para el Manejo del juego (Random, IA)-}

-- Genera número aleatorio en un rango
aleatorio :: (Int, Int) -> Int -> (Int, StdGen)
aleatorio (x, y) n = randomR (x, y) (mkStdGen n)


--Movimiento aleatorio de una ficha aleatoria
mueveIA :: Matrix String -> [String] -> Int -> Matrix String
mueveIA m xs al = if (valido m (x,y) (xf,yf) 2) then mover m (x,y) (xf,yf) 2 else mueveIA m xs (al+2)
    where a = fst (aleatorio (0, (length xs)-1) al)
          ficha = xs !! a
          x = posicionXFicha m ficha
          y = posicionYFicha m ficha x
          xf = fst (aleatorio (1,8) al)
          yf = fst (aleatorio (1,8) (al+2))


