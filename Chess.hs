--hi
import Data.List
import Data.Matrix
import Data.Char
import Data.String

fnQ = ["TN1", "CN1", "AN1", "QN ", "KN ", "AN2", "CN2", "TN2"]
fnP = ["PN1", "PN2", "PN3", "PN4", "PN5", "PN6", "PN7", "PN8"]
fbP = ["PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8"]
fbQ = ["TB1", "CB1", "AB1", "KB ", "QB ", "AB2", "CB2", "TB2"]
fichasBlancas = fbP ++ fbQ
fichasNegras = fnP ++ fnQ

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

siguiente :: Int -> Int
siguiente j = 1 + (mod j 2)


fichaValida :: String -> Int -> Bool
fichaValida x 1 = or [x==y| y<-fichasNegras]
fichaValida x 2 = or [x==y| y<-fichasBlancas]



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


leeFicha :: String -> Int ->IO String
leeFicha c j = do
    putStr c
    ts <- getLine
    if (not.null) ts && fichaValida ts j then
        return ts
    else do
        putStrLn "ERROR: Entrada incorrecta"
        leeFicha c j

esFila :: String -> [String] ->Bool
esFila x xs = not(or [x==y| y <-xs])


posicionXFicha :: Matrix String -> String -> Int
posicionXFicha m x = xp
    where filas = toLists m
          antes = takeWhile (esFila x) filas
          xp = length antes +1

          
posicionYFicha :: Matrix String -> String -> Int -> Int
posicionYFicha m x xp = yp + 1
    where filas = toLists m
          linea=filas !!(xp -1)
          yp = sum[1 | s<- takeWhile (/=x) linea]
          

filaString :: String -> [String] -> String
filaString a (x:xs)
    | length xs == 1 = (a++" | " ++x)
    |otherwise = filaString (a++" | " ++x) xs

escribeFila :: Int -> [String]-> IO ()
escribeFila f n = putStrLn $ show f ++ ": " ++ (filaString " " n)


escribeTablero :: Matrix String -> IO ()
escribeTablero t = do 
    sequence_ [escribeFila i n | (i,n)<-zip [1 ..] filas]
        where filas = toLists t



juego :: Matrix String -> Int -> IO ()
juego t j = do
    putStr " \n "
    escribeTablero t
    putStr " \n "
    putStrLn $ "J " ++ show j
    f <- leeFicha "Elije una ficha: " j
    let x = posicionXFicha t f
    let y = posicionYFicha t f x
    xf <- leeDigito "Elije fila de movimiento: "
    yf <- leeDigito "Elije columna de movimiento: "

    if (valido t (x, y) (xf, yf) j) then do
        let t2 = mover t (x, y) (xf, yf)
        if finalizado t2 j then do
            putStr " \n "
            escribeTablero t2
            putStrLn $ " \n J " ++ show j ++ " ha ganado!"
        else do
            let j2 = siguiente j
            juego t2 j2
    else juego t j
    