# CHESS:

Es un proyecto realizado para la asignatura de Programación Declarativa impartida en la Universidad de Sevilla.

Este proyecto funciona sobre Haskell y trata sobre la realización de un Ajedrez con un tablero de 8x8, el cual podrá ser jugado entre dos personas o contra la máquina.

# REQUISITOS:

Para ejecutar la siguiente aplicacion se necesitará de tener instalado Haskell, además de ciertos modulos externos instalados para los que se debera de realizar:

    - cabal install matrix

Además de esto se debera de mantener los fichero de "Juegos.hs", "Movimientos.hs" y "Tablero.hs" en el mismo directorio (para asi poder usarse entre ellos)

En este proyecto se hará uso de estructuras básicas y avanzadas como el uso de listas o matrices ademas de funciones de orden superior, comprensión, recursión o I/O.

# Funcionamiento

Para ejecutar el proyecto se realizara a partir del siguiente comando:

Para complilar usaremos :l Juegos.

Una vez hecho esto deberemos excribir por la terminal "jugar" (función equivalente a main).

A continuación comenzará la partida y deberemos elegir entre "PvP" (jugador contra jugador) o "PvC" (jugador vs máquina que realiza movimientos aleatorios), para ello escribiremos por la terminal la opción que querida.

Trás esto comenzará (empieza con el turno de las BLANCAS) el turno del primer J1 este deberá elegir la ficha que desea mover y una posición correcta (posiciones correcta es aquella que esta contemplada como valida en el ajedrez)

Por último el juego acabara en el momento que uno de los jugadores elimine el Rey (KB o KN) contrario. 