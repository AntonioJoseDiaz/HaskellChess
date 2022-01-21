--hi
import Data.List
import Data.Matrix
import Data.Char
import Data.String
import System.Random
import Data.Array


fnQ = ["TN1", "CN1", "AN1", "QN0", "KN ", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PN0", "PN9"]
fbQ = ["TB1", "CB1", "AB1", "KB ", "QB0", "AB2", "CB2", "TB2"]


tablero :: Matrix String
tablero = creaTablero 4

--Crea lineas vacías (posiciones sin ficha en el tablero)
vacio :: Int -> [[String]]
vacio n = replicate n [" * " | x<-[1..8]]
-- función con lista por compresión


--Crea una lista con todas las filas del tablero
listaPosiciones :: Int -> [[String]]
listaPosiciones n
    | n <=3 = []
    |otherwise = [fnQ, fnP] ++ vacio n ++ [fbP, fbQ]
--función con uso de case of

--Crea el tablero como una Matriz
creaTablero :: Int -> Matrix String
creaTablero n  
    |n<=3 = fromLists [["ERROR : numero de lineas invalido, debe ser >=4"]]
    |otherwise =fromLists pos
    where pos = listaPosiciones n
--función con uso de case of


--Comprobar si el juego ha terminado
finalizado :: Matrix String -> Int -> Bool
finalizado m 1 = not(or ["KB " == (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])
finalizado m 2 = not(or ["KN " == (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])


--Movemos pieza de la posición (x, y) a la posición (xf, yf), nos comemos la del rival si está en la posición (xf, yf)
-- peon reina antes por que se tiene que mover y luego comprobar
mover :: Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Matrix String
mover m (x,y) (xf, yf) j = peonReina (setElem ficha (xf, yf) (setElem hueco (x, y) m)) j
    where ficha = getElem x y m
          pos = getElem xf yf m
          hueco = if j == 1 && (elem 'B' pos) then " * " else 
              if elem 'N' pos then " * "  else pos


--Comprobar si el movimiento es válido y no intentamos intercambiar una pieza con nosotros mismos
valido ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Bool
valido m (x,y) (xf, yf) j = if (j==1) then not (elem 'N' sust) && mov else not (elem 'B' sust) && mov
    where sust = getElem xf yf m
          pieza = getElem x y m
          mov = movimiento m (x,y) (xf, yf) j pieza sust
                
-- Obtenermos la posicion del primero en cumplir si es False o True
primero_cumplir :: Bool -> [Bool]-> Int
primero_cumplir n [] = 0
primero_cumplir n (x:xs) = if x == n then 1 else primero_cumplir n xs + 1 

recorridoHorizontal :: Matrix String -> (Int, Int) -> (Int, Int) -> String -> String -> Bool
recorridoHorizontal matriz (x,y) (xf,yf) pieza sust = and [(getElem a b matriz)== " * " || (getElem a b matriz) == pieza || (getElem a b matriz) == sust  |(a,b) <-range((x,y),(xf,yf)) ]


recorridoDiagonal :: Matrix String -> (Int, Int) -> (Int, Int) -> String -> String -> Bool 
recorridoDiagonal matriz (x,y) (xf,yf) pieza sust 
    |(abs (xf - x) == abs (yf - y)) && (abs (xf - x) == 0) && (abs (yf - y) == 0) = if getElem x y matriz == pieza || getElem x y matriz == sust || getElem x y matriz == " * " then True else False
    |(abs (xf - x) == abs (yf - y)) && (abs (xf - x) /= 0) && (abs (yf - y) /= 0) = if getElem x y matriz == pieza || getElem x y matriz == sust || getElem x y matriz == " * " then if direccion_x >=0 then if direccion_y >=0 then  cuadrante_a else cuadrante_d else if direccion_y >=0 then  cuadrante_b else cuadrante_c else False
    |otherwise = False
    where 
        direccion_x = xf - x
        direccion_y = yf - y
        cuadrante_a = recorridoDiagonal matriz (x+1,y+1) (xf,yf) pieza sust
        cuadrante_b = recorridoDiagonal matriz (x-1,y+1) (xf,yf) pieza sust
        cuadrante_c = recorridoDiagonal matriz (x-1,y-1) (xf,yf) pieza sust
        cuadrante_d = recorridoDiagonal matriz (x+1,y-1) (xf,yf) pieza sust

--        ((abs (a - x)) == (abs (b - y)))

--Comprobamos si el movimiento que queremos hacer sobre la pieza es correcto en función de qué pieza movamos
movimiento ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> String -> String -> Bool
movimiento matriz (x,y) (xf, yf) j pieza sust
    |elem 'T' pieza = if  (x == xf || y == yf) then if  recorridoHorizontal matriz (x,y) (xf, yf) pieza sust then True else False else False
    |elem 'C' pieza = if  ((abs (xf -x)) == 2 && (abs (yf - y)) == 1) || ((abs (xf -x)) == 1 && (abs (yf - y)) == 2) then True else False
    |elem 'K' pieza = if  ((abs (xf -x)) <= 1 && (abs (yf - y)) <= 1) then True else False
    |elem 'A' pieza = if  recorridoDiagonal matriz (x,y) (xf, yf) pieza sust then True else False
    |elem 'Q' pieza = if  recorridoDiagonal matriz (x,y) (xf, yf) pieza sust || (((x == xf || y == yf)) && ( recorridoHorizontal matriz (x,y) (xf, yf) pieza sust)) then True else False
    |elem 'P' pieza = if j==1 then if elem 'B' sust then (xf-x==1) && abs(yf - y)==1 else (xf-x==1) && yf==y 
                    else if elem 'N' sust then (x - xf==1) && abs(yf - y)==1 else (x - xf==1) && yf==y
    |otherwise = (abs(xf-x) == abs (yf-y)) || x==xf || y==yf
        
peonReina :: Matrix String -> Int -> Matrix String
peonReina matriz 1 = if pos_N /= 0 then setElem ("QN"++ show cont_N) (8, pos_N) matriz else matriz
    where 
        pos_N = primero_cumplir True [elem 'P' (getElem 8 y matriz) | y<-[1..8] ] 
        cont_N = length (filter (elem 'N') (filter (elem 'Q') (toList matriz)))

peonReina matriz 2 = if pos_B /= 0 then setElem ("QB"++ show cont_B) (1, pos_B) matriz else matriz
    where
        pos_B = primero_cumplir True [elem 'P' (getElem 1 y matriz) |  y<-[1..8] ]
        cont_B = length (filter (elem 'B') (filter (elem 'Q') (toList matriz)))

--Siguiente jugador
siguiente :: Int -> Int
siguiente j = 1 + (mod j 2)

--Comprobamos si la pieza que queremos mover se encuentra entre las piezas actuales en juego
fichaValida :: String -> [String] -> Bool
fichaValida x piezas = or [x==y| y<-piezas]


--Leemos el dígito al que queremos mover la pieza
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


--Leemos el modo de juego
leeModo :: String -> IO String
leeModo c = do
    putStr c
    ts <- getLine
    if (ts=="PvP" || ts=="PvC") then
        return ts
    else do
        putStrLn "ERROR: Entrada incorrecta"
        leeModo c

--Leemos la pieza a mover introducida y comprobamos si es válida
leeFicha :: String  -> [String] ->IO String
leeFicha c piezas= do
    putStr c
    ts <- getLine
    if (not.null) ts && fichaValida ts piezas then
        return ts
    else do
        putStrLn "ERROR: Ficha incorrecta"
        leeFicha c piezas


--Comprobar si la ficha se encuentra en la fila
esFila :: String -> [String] ->Bool
esFila x xs = not(or [x==y| y <-xs])

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


--Obtenemos la lista de piezas en el tablero del jugador
fichasJugador :: Matrix String -> Int -> [String]
fichasJugador m j =piezas
     where lista = toLists m
           piezas = foldr f [] lista
           f xs zs 
                | length xs == 0 = zs
                | otherwise = zs ++ (fichasLinea xs j) 

--Dunción auxiliar para conseguir las piezas del jugador en la línea indicada
fichasLinea :: [String] -> Int -> [String]
fichasLinea xs j = foldr f [] xs
    where f x zs 
            | j == 1 && (elem 'N' x) = (x:zs)
            | j == 2 && (elem 'B' x) = (x:zs)
            | otherwise = zs



juego :: Matrix String -> Int -> IO ()
juego t j = do
    putStr " \n"
    escribeTablero t
    putStr " \n "
    putStrLn $ "J " ++ show j

    let piezasJugador = fichasJugador t j

    f <- leeFicha "Elije una ficha: " piezasJugador
    let x = posicionXFicha t f
    let y = posicionYFicha t f x
    xf <- leeDigito "Elije fila de movimiento: "
    yf <- leeDigito "Elije columna de movimiento: "
    print(x)
    print(y)
    print(range((x,y),(xf,yf)))
    print(range((xf,yf),(x,y)))

    if (1 <= xf &&  xf <= 8 && 1 <= yf &&  yf <= 8) then do
        if (valido t (x, y) (xf, yf) j) then do
            let t2 = mover t (x, y) (xf, yf) j
            
            if finalizado t2 j then do
                putStr " \n "
                escribeTablero t2
                putStrLn $ " \n J " ++ show j ++ " ha ganado!"
            else do
                let j2 = siguiente j
                juego t2 j2
        else juego t j
    else do
        putStr "\ESC[2J"
        putStrLn $ " \n Se ha exedido el rango de la matriz " ++ show xf ++" "++ show yf
        juego t j


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
        escribeTablero t
        putStr " \n "
        putStrLn $ "Jugador " ++ show j

        let piezasJugador = fichasJugador t j

        f <- leeFicha "Elije una ficha: " piezasJugador
        let x = posicionXFicha t f
        let y = posicionYFicha t f x
        xf <- leeDigito "Elije fila de movimiento: "
        yf <- leeDigito "Elije columna de movimiento: "

        print(range((x,y),(xf,yf)))
        print( [(getElem a b t)  |(a,b) <-range((x,y),(xf,yf)) ])
        print([(getElem a b t)== " * "  |(a,b) <-range((x,y),(xf,yf)) ])
        print(valido t (x, y) (xf, yf) j)

        if (valido t (x, y) (xf, yf) j) then do
            let t2 = mover t (x, y) (xf, yf) j
            if finalizado t2 j then do
                putStr " \n"
                escribeTablero t2
                putStrLn $ " \n J " ++ show j ++ " ha ganado!"
            else do
                let j2 = siguiente j
                escribeTablero t2
                juegoIA t2 j2 al
        else juegoIA t j al


jugar :: IO()
jugar= do
    putStrLn "Seleccionar modo de juego: PvP o PvC"
    modo <- leeModo "Modo de juego: "
    if (modo == "PvP") then do
        juego tablero 1
    else do
        numero <- leeDigito "seleccionar número par para empezar: "
        juegoIA tablero 1 numero
        