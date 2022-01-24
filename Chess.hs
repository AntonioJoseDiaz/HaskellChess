
module Chess
(
    tablero,
    vacio,
    listaPosiciones,
    creaTablero,
    finalizado,
    mover,
    valido,
    primero_cumplir,
    recorridoPieza,
    movimiento,
    peonReina,
    siguiente,
    fichaValida,
    leeDigito,
    leeModo,
    leeFicha,
    esFila,
    posicionXFicha,
    posicionYFicha,
    filaString,
    escribeFila,
    escribeTablero,
    fichasJugador,
    fichasLinea,
    juego,
    aleatorio,
    mueveIA,
    juegoIA,
    jugar
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
             juegoIA tablero 1 (round second)
        else do
            juegoIA tablero 1 ((round second) +1)
        
{- 3.4.1 Funciones de Modo de Juego -}

juego :: Matrix String -> Int -> IO ()
juego t j = do
    putStr " \n"
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
                putStr " \n "
                escribeTablero t2
                putStrLn $ " \n J " ++ show ((mod j 2)+1) ++ " ha ganado!"
            else do
                putStr "\ESC[2J"
                let j2 = siguiente j
                juego t2 j2
        else do
            putStr "\ESC[2J"
            juego t j
    else do
        putStr "\ESC[2J"
        putStrLn $ " \n Se ha exedido el rango de la matriz " ++ show xf ++" "++ show yf
        juego t j

juegoIA :: Matrix String -> Int -> Int -> IO ()
juegoIA t j al= do

    if (j==2) then do
        let piezasJugador = fichasJugador t 2
        let ti = mueveIA t piezasJugador al
        let j2 = siguiente j
        putStrLn "\n\nSiguiente turno \n"
        juegoIA ti j2 (al+2)
    else do
        putStr " \n"
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
                    escribeTablero t2
                    putStrLn $ " \n J " ++ show j ++ " ha ganado!"
                else do
                    putStr "\ESC[2J"
                    let j2 = siguiente j
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


