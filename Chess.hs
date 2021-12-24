--hi
import Data.List
import Data.Matrix
import Data.Char
import Data.String
import System.Random

fnQ = ["TN1", "CN1", "AN1", "QN ", "KN ", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8"]
fbQ = ["TB1", "CB1", "AB1", "KB ", "QB ", "AB2", "CB2", "TB2"]


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
finalizado m 1 = not(or [elem 'B' (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])
finalizado m 2 = not(or [elem 'N' (getElem x y m) | x<-[1..8], y<-[1..(nrows m)]])


--Movemos pieza de la posición (x, y) a la posición (xf, yf), nos comemos la del rival si está en la posición (xf, yf)
mover :: Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Matrix String
mover m (x,y) (xf, yf) j = setElem ficha (xf, yf) (setElem hueco (x, y) m)
    where ficha = getElem x y m
          pos = getElem xf yf m
          hueco = if j == 1 && (elem 'B' pos) then " * " else 
              if elem 'N' pos then " * "  else pos


--Comprobar si el movimiento es válido y no intentamos intercambiar una pieza con nosotros mismos
valido ::  Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Bool
valido m (x,y) (xf, yf) j = if (j==1) then not (elem 'N' sust) && mov else not (elem 'B' sust) && mov
    where sust = getElem xf yf m
          pieza = getElem x y m
          mov = movimiento (x,y) (xf, yf) j pieza sust
                
--Comprobamos si el movimiento que queremos hacer sobre la pieza es correcto en función de qué pieza movamos
movimiento :: (Int, Int) -> (Int, Int) -> Int -> String -> String -> Bool
movimiento (x,y) (xf, yf) j pieza sust
    |elem 'T' pieza = (x==xf) || (y==yf)
    |elem 'C' pieza = ((abs (xf -x)) == 1 && (abs (yf - y)) == 2)
    |elem 'K' pieza = ((abs (xf -x)) == 1 && (abs (yf - y)) == 1)
    |elem 'P' pieza = if j==1 then if elem 'B' sust then (xf-x==1) && abs(yf - y)==1 else (xf-x==1) && yf==y 
                    else if elem 'N' sust then (x - xf==1) && abs(yf - y)==1 else (x - xf==1) && yf==y
    |otherwise = (abs(xf-x) == abs (yf-y)) || x==xf || y==yf


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
    
empezarJuego :: IO()
empezarJuego = juego tablero 1


aleatorio :: (Int, Int) -> Int -> (Int, StdGen)
aleatorio (x, y) n = randomR (x, y) (mkStdGen n)

mueveIA :: Matrix String -> [String] -> Int -> Matrix String
mueveIA m xs al = if (valido m (x,y) (xf,yf) 2) then mover m (x,y) (xf,yf) 2 else mueveIA m xs (al+2)
    where a = fst (aleatorio (0, (length xs)) al)
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
        putStr "ia \n"
        escribeTablero ti
        putStr " tablero"
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

        if (valido t (x, y) (xf, yf) j) then do
            let t2 = mover t (x, y) (xf, yf) j
            if finalizado t2 j then do
                putStr " \n "
                escribeTablero t2
                putStrLn $ " \n J " ++ show j ++ " ha ganado!"
            else do
                let j2 = siguiente j
                escribeTablero t2
                juegoIA t2 j2 al
        else juegoIA t j al