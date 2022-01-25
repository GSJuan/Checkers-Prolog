
%%%%%%%%%%%%%%%%%%
%-----Hechos-----%
%%%%%%%%%%%%%%%%%%
esDama(pb). %pieza blanca o negra
esDama(pn).

esRey(rb). %rey blanco o negro
esRey(rn).

esBlanca(pb). %pieza dama o rey blancos
esBlanca(rb).

esNegra(pn). %pieza dama o rey negros
esNegra(rn).

esVacia(va). %casillavacia

esPieza(X) :- esDama(X) ; esRey(X). %si es dama o rey, se considera pieza

legal(negro, pn). % es legal que el jugador negro mueva el rey o las piezas negrs
legal(negro, rn).

legal(blanco, pb).% es legal que el jugador blanco mueva el rey o las piezas blancas
legal(blanco, rb).

%las blancas son opuestas a las negras y viceversa
piezaOpuesta(X,Y):- esBlanca(X), esNegra(Y).
piezaOpuesta(X,Y):- esNegra(X), esBlanca(Y).

%la piezas de cada color promocionan a rey del mismo color
promocionar(pb, rb).
promocionar(pn, rn).

%color en cada turno
siguienteJugador(blanco, negro).
siguienteJugador(negro, blanco).

%jugador en cada turno
siguienteTurno(minimax, humano).
siguienteTurno(humano, minimax).

% obtiene el rango de accion de una pieza, asi distinguimos por alcance entre una dama y un rey
rangoPieza(Pieza, Rango) :- esRey(Pieza), Rango is 8; esDama(Pieza), Rango is 1.

%conversion de valores al output Gráfico
simbolo(nl, ' _ ').
simbolo(va, ' _ ').
simbolo(rn, ' N ').
simbolo(rb, ' B ').
simbolo(pn, ' n ').
simbolo(pb, ' b ').

%%%%%%%%%%%%%%%%%%
%----Gráficos----%
%%%%%%%%%%%%%%%%%%
%imprime las filas de la malla para así intentar formar el efecto visual de las casillas
printGrid :- write('|---+---+---+---+---+---+---+---+---|'), nl.

%predicado para mostrar por consola el tablero de juego actual
printTablero :- %imprime el encabezado del tablero
  write('-------------------------------------'), nl,
  write('| _ | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |'), nl,
  write('|---+---+---+---+---+---+---+---+---|'), nl, printTablero(1), !. %despues, empieza a imprimir el tablero


printTablero(9) :- nl, !. %cuando termine, añade un retorno de carro

%para imprimir el tablero, solo hay que imprimir cada fila con su malla y repetirlo de forma recursiva hasta acabar el numero de filas del tablero
printTablero(Y) :- printFila(Y), printGrid, SigFila is Y + 1,  printTablero(SigFila).

%para imprimir una fila, lo primero es imprimir el numero de fila, y mostrarlo en la columna 0, es decir, fuera del tablero
printFila(Y):- write('| '), printFila(Y, 0).

%imprimimos el numero de fila, lo separamos del tablero y procedvaos a imprimir los valores de las casillas hasta el 9
printFila(Y, 0):- write(Y), write(' |'), printFila(Y, 1).

%para cada casilla obtenvaos la pieza, la pasamos a un simbolo y mostramos el simbolo por pantalla. Advaas avanazamos a la siguiente columna 
printFila(Y, X):- getPieza(X, Y, Pieza), simbolo(Pieza, Symbol), write(Symbol), write('|'), NextCol is X + 1, printFila(Y, NextCol).

%cnd llegamos al final de cada fila, saltamos a la siguiente
printFila(_, 9):- nl, !.

printJugadores :- write('Jugador 0: Humano'), nl,
	write('Jugador 1: IA Minimax con poda Alfa-Beta'), nl.

%%%%%%%%%%%%%%%%%%
%---Utilidades---%
%%%%%%%%%%%%%%%%%%
%comprueba si la posicion está vacia, un ejvaplo de uso es paraver si la casilla a la que va a avanzar está vacia
casillaVacia(X, Y):- getPieza(X, Y, Casilla), esVacia(Casilla).

%no hay pieza en esa posicion
noHayPieza(Posicion) :- getTablero(Tablero), nth0(Posicion, Tablero, Casilla), esVacia(Casilla).

% convierte las cordenadas X, Y en un umero de dos digitos, para asi poder acceder a los elementos de la lista en la que se almacena el tablero
convertCoord(X, Y, Pos):- dentroTablero(X, Y), (Pos is ((Y-1) * 8 + (X-1))). %calculamos la posicion en la lista de la coordenada X Y, dado que el tablero posee 8 columnas, calculamos el indice en base a eso

% comprueba si las coordenadas estan dentro de los limites del tablero de tamañao 8x8 (vapezando en 1)
dentroTablero(X, Y) :- between(1, 8, X), between(1, 8, Y).

% funcion para cambiar un elemento en un array (Tablero, Indice, Nuevo elemento, Nuevo Tablero)
% intercambia el elemento del Indice del Tablero por el Nuevo elemento y devuelve el nuevo tablero con la modificacion realizada
reemplaza([_|T], 0, X, [X|T]). %estando en la posicion seleccionada de la lista (el indice ha llegado a cero), vapujamos el Nuevo elemento a la cabeza del Nuevo Tablero, pero en ese indice
reemplaza([H|T], I, X, [H|R]):- I > -1, NI is I-1, reemplaza(T, NI, X, R), !. % comprobamos que el indice sea mayor que -1, decrecvaos el contador del indice y llamamos recursivamente a la funcion con la lista restante 
reemplaza(L, _, _, L). %caso base, los tableros se copian

%comprueba que el jugador este en los rangos del tablero
posValida(PosOriginal, PosComida, PiezaOriginal, PiezaComida) :-
  between(0, 63, PosOriginal), %piezas en posiciones dentro del tablero
  between(0, 63, PosComida), 
  PosOriginal - PosComida \= 0, % y no estan en la misma posicion

  %obten las piezas en esas posiciones
  findPieza(PosOriginal, PiezaOriginal),
  findPieza(PosComida, PiezaComida),

  %comprueba si son opuestas
  piezaOpuesta(PiezaOriginal, PiezaComida).

%convierte los temporales en vacios
tempToEmpty:-
  getTablero(Tablero),
  nth0(Pos, Tablero, tm),
  reemplaza(Tablero, Pos, va, NewTablero),
  setTablero(NewTablero),
  tempToEmpty.

tempToEmpty.

%%%%%%%%%%%%%%%%%%%%%%%%%
%---Getters y Setters---%
%%%%%%%%%%%%%%%%%%%%%%%%%
% getter de la pieza en las coordinadas X, Y del tablero
getPieza(X, Y, Pieza) :- convertCoord(X, Y, Pos), getTablero(Tablero), nth0(Pos, Tablero, Pieza).

%Como el getter de encima, pero vaplea la coordenada ya coonvertida en un numero de dos digitos
findPieza(PosPieza, Pieza) :- between(0, 63, PosPieza), getTablero(Tablero), nth0(PosPieza, Tablero, Pieza).

% getter y setter del estado actual del tablero
getEstado(Estado) :- b_getval(estado, Estado). %unifica el valor del átomo estado con la variable Estado
setEstado(Estado) :- b_setval(estado, Estado). %asocia el valor de Estado al átomo estado

%getters y setters del jugador
leerJugador(Jugador, Number):- nl, write('Elija el Jugador '), write(Number), write(" : "), nl, printJugadores, read(Jugador), between(0, 1, Jugador).
leerJugador(Jugador, Level):- nl, write('Ese Jugador no existe.'), nl, leerJugador(Jugador, Level).

setJugador(JugadorNumber, 0) :-
    b_setval(JugadorNumber, humano).
setJugador(JugadorNumber, 1) :-
    b_setval(JugadorNumber, minimax).

%getters del tablero actual
getTablero(Tablero):- getEstado(Estado), %obtiene el estado actual
                      getTablero(Tablero, Estado). %y devuelve el tablero asociado al estado actual
getTablero(Tablero, tablero):- b_getval(tablero, Tablero). %devuelve en tablero aquel asociado al átomo tablero
getTablero(Tablero, simulation):- b_getval(simulation, Tablero).
    
%setters del tablero actual
setTablero(NewTablero):- getEstado(Estado), %obtiene el estado actual
                         setTablero(NewTablero, Estado). %y establece el nuevo tablero en el estado actual
setTablero(NewTablero, tablero):- b_setval(tablero, NewTablero). %establece un tablero nuevo, sobreescribiendo un tablero dado
setTablero(NewTablero, simulation):- b_setval(simulation, NewTablero).

%%%%%%%%%%%%%%%%%%%%%%%%%
%---Lógica del Juego----%
%%%%%%%%%%%%%%%%%%%%%%%%%
%se obtiene el tablero (lista) y se comprueba si en la lista quedan piezas blancas y si quedan piezas negras. Desde que un jugador se quede sin piezas, finaliza la partida.
sigueJugando:- getTablero(Tablero), sigueJugando(Tablero, blanco), sigueJugando(Tablero, negro).
sigueJugando(Tablero, blanco):- member(pb, Tablero), ! ; member(rb, Tablero), !. %comprueba que queden piezas blancas (reyes o damas) en la lista que representa el tablero
sigueJugando(Tablero, negro):- member(pn, Tablero), ! ; member(rn, Tablero), !. %comprueba que queden piezas negras (reyes o damas) en la lista que representa el tablero

% se decide el ganador, que es aquél que no se haya quedado sin fichas. Para ello miramos el tablero
ganador(Ganador):- getTablero(Tablero), (ganador(Tablero, blanco, Ganador), ganador(Tablero, negro, Ganador)).
ganador(Tablero, blanco, negro):- not(member(pb, Tablero)), not(member(rb, Tablero)). %si no quedan piezas blancas (reyes o damas) en el tablero, ganan las negras
ganador(Tablero, negro, blanco):- not(member(pn, Tablero)), not(member(rn, Tablero)) . %si no quedan piezas negras (reyes o damas) en el tablero, ganan las blancas

% promociona la pieza que este en las coordenadas X,Y a rey, mira que pieza es, comprueba que sea promocionable, la promociona y modifica el tablero para colocarla
promocion(X, Y):- promocionable(X, Y), procesarPromocion(X,Y).
promocion(_,_).

% comprueba si la pieza que este en X,Y puede ser promocionada
promocionable(X,Y) :-  getPieza(X, Y, Pieza), promocionable(X, Y, Pieza).
promocionable(_, Y, pb):- Y == 8.
promocionable(_, Y, pn):- Y == 1.

procesarPromocion(X, Y):- getPieza(X, Y, Pieza), promocionar(Pieza, NewPieza), getTablero(Tablero), convertCoord(X, Y, Pos), reemplaza(Tablero, Pos, NewPieza, NewTablero), setTablero(NewTablero).
procesarPromocion(_, _).

% comprueba si un Jugador puede jugadar la pieza que se encuentra en las coordenadas X, Y del tablero
legal(Jugador, X, Y) :- getPieza(X, Y, Pieza), legal(Jugador, Pieza).

% Primero intentamos comer, si no, movemos. Siempre intentamos promocionar la ficha despues de alguna acción además de convertir todas las casillas temp a vacias
procesarTurno(_, X, Y, NewX, NewY):- comer(X, Y, NewX, NewY, FinalX, FinalY), promocion(FinalX, FinalY), tempToEmpty.
procesarTurno(_, X, Y, NewX, NewY):- mover(X, Y, NewX, NewY), promocion(NewX, NewY), tempToEmpty.
procesarTurno(_, _, _, _, _):- !.

%ejecuta el proceso inicial del turno en base al jugador que este jugando en este momento
turno(humano,  Color, X, Y, NewX, NewY):- jugada(Color, X, Y, NewX, NewY).
turno(minimax,  Color, X, Y, NewX, NewY):- minimaxIA(Color, X, Y, NewX, NewY).

%realiza el movimiento que el jugador haya introducido, comprobando que sea legal
jugada(Color, X, Y, NewX, NewY):- leerJugada(X,Y,NewX,NewY), comprobarJugada(Color, X, Y, NewX, NewY).
% si el movimiento es invalido, solicita que se introduzca de nuevo otro movimiento
jugada(Color, X, Y, NewX, NewY):- write('Movimiento Invalido, escoge otro'), nl, jugada(Color, X, Y, NewX, NewY).

%comprueba si una jugada es legal y factible
comprobarJugada(Jugador, X, Y, NewX, NewY):- legal(Jugador, X, Y), (comprobarComer(X, Y, _, _, NewX, NewY); comprobarMovimiento(X, Y, NewX, NewY)).

% Lee por consola el movimiento que desea realizar el jugador
leerJugada(X,Y,NewX,NewY):- write('Introduzca la coordenada X de la ficha que desea mover(columna)'), nl,read(X), write('Introduzca la coordenada Y de la ficha que desea mover(fila)'), nl, read(Y), write('Introduzca la coordenada X (columna) de la posicion hacia la que desea mover la ficha'), nl, read(NewX), write('Introduzca la coordenada Y (fila) de la posicion hacia la que desea mover la ficha'), nl, read(NewY).


%%%%%%%%%%%%%%%%%%%%%%%%%
%-Movimiento del Juego--%
%%%%%%%%%%%%%%%%%%%%%%%%%

%accion de movimiento, comprueba que es legal y lo ejecuta
mover(X, Y, NewX, NewY) :- comprobarMovimiento(X, Y, NewX, NewY), procesarMovimiento(X, Y, NewX, NewY).

%comprueba que el nuevo movimiento es legal en terminos de lugar(dentro de los limites, casilla vacia...)
comprobarMovimiento(X, Y, NewX, NewY):- dentroTablero(X, Y), dentroTablero(NewX, NewY), casillaVacia(NewX, NewY), getPieza(X, Y, Pieza), comprobarMovimientoPieza(Pieza, X, Y, NewX, NewY). %pone la casilla original vacia, mueve la ficha y guarda el mundo resultante

%procesa el movimiento, haciendolo efectivo
procesarMovimiento(X, Y, NewX, NewY):- getPieza(X, Y, Pieza), convertCoord(X, Y, Pos), convertCoord(NewX, NewY, NewPos), getTablero(Tablero), reemplaza(Tablero, Pos, va, NewTablero1), reemplaza(NewTablero1, NewPos, Pieza, NewTablero2), setTablero(NewTablero2).

%comprueba los movimientos permitidos asociados a cada pieza
comprobarMovimientoPieza(pb, X, Y, NewX, NewY):-
    PermitidoX1 is X+1, PermitidoX2 is X-1, PermitidoY is Y+1, %los movimientos de las piezas blancas son a casillas adyacentes en diagonal y avanzando hacia abajo
    (PermitidoX1 == NewX; PermitidoX2 == NewX), PermitidoY == NewY.

comprobarMovimientoPieza(pn, X, Y, NewX, NewY):- %los movimientos de las piezas negras son a casillas adyacentes en diagonal y avanzando hacia arriba
    PermitidoX1 is X-1, PermitidoX2 is X+1,
    PermitidoY is Y-1,
    (PermitidoX1 == NewX; PermitidoX2 == NewX),
    PermitidoY == NewY.

comprobarMovimientoPieza(rn, X, Y, NewX, NewY):- %los movimientos de las piezas negras son a casillas adyacentes en diagonal hacia arriba o hacia abajo
    PermitidoX1 is X-1, PermitidoX2 is X+1,
    PermitidoY1 is Y-1, PermitidoY2 is Y+1,
    (PermitidoX1 == NewX; PermitidoX2 == NewX),
    (PermitidoY1 == NewY; PermitidoY2 == NewY).
comprobarMovimientoPieza(rn, _, _, _, _):- !.

comprobarMovimientoPieza(rb, X, Y, NewX, NewY):-
    PermitidoX1 is X-1, PermitidoX2 is X+1,
    PermitidoY1 is Y-1, PermitidoY2 is Y+1,
    (PermitidoX1 == NewX; PermitidoX2 == NewX),
    (PermitidoY1 == NewY; PermitidoY2 == NewY).
comprobarMovimientoPieza(rb, _, _, _, _):- !.

%accion de comer, come ficha y comprueba si en la nueva posicion de llegada puede volver a comer de forma recursiva o solo come ficha.
comer(X, Y, NewX, NewY, FinalX, FinalY):- (comestible(X, Y, NewX, NewY), comer(NewX, NewY, _, _, FinalX, FinalY)); comestible(X, Y, NewX, NewY), FinalX is NewX, FinalY is NewY.

%comprueba que la ficha sea comestible y lo procesa
comestible(X, Y, NewX, NewY) :- comprobarComer(X, Y, XComida, YComida, NewX, NewY), convertCoord(X, Y, PosOrigen), convertCoord(NewX, NewY, NewPos), convertCoord(XComida, YComida, PosComida), procesarComida(PosOrigen, PosComida, NewPos).

%procesa y reemplaza las piezas comidas por vacios temporales, mas sencillo gestionar cada turno en caso de que haya multiples comidas.
procesarComida(PosOrigen, PosComida, NewPos) :-  findPieza(PosOrigen, Pieza), getTablero(Tablero), 
        reemplaza(Tablero, PosOrigen, va, NewTableroTemp), reemplaza(NewTableroTemp, PosComida, tm, NewTableroTemp2), reemplaza(NewTableroTemp2, NewPos, Pieza, NewTablero), setTablero(NewTablero).

%comprueba las coordenadas para comer
comprobarComer(X, Y, XComida, YComida, NewX, NewY) :- convertCoord(X, Y, PosOrigen), convertCoord(XComida, YComida, PosComida), findPieza(PosOrigen, Pieza), comprobarComer(PosOrigen, PosComida, PosLLegada), comprobarMovimientoPieza(Pieza,X,Y,XComida,YComida), convertCoord(NewX, NewY, PosLLegada). %permite evaluar multiples candidatos
comprobarComer(PosOrigen, PosComida, PosCandidataLlegada) :- posValida(PosOrigen, PosComida, Pieza, _), rangoPieza(Pieza, RangoPieza), comprobarComestiblesCercanas(PosOrigen, PosComida, RangoPieza, PosCandidataLlegada), findPieza(PosCandidataLlegada, Casilla), esVacia(Casilla).

% Buscamos las piezas más cercanas en la misma diagonal y en el rango de accion de la pieza
comprobarComestiblesCercanas(PosOrigen, PosComida, RangoPieza, PosCandidataLlegada) :-

  % estan en la misma diagonal
  piezasEnMismaDiagonal(PosOrigen, PosComida, CaminoAlObjetivo, DistanciaDiagonal),

  % Rango suficiente para alcanzar la pieza
  RangoPieza >= abs(CaminoAlObjetivo),

  % Obtenemos el camino despues de haber comido y haber saltado la ficha
  getCaminoNuevaPosicion(CaminoAlObjetivo, CaminoAlDestino),

  % comprobamos que no haya fichas en medio (pensado para caso en los que come el rey desde grandes distancias)
  sinPiezasEnmedio(PosOrigen, CaminoAlObjetivo, DistanciaDiagonal, CaminoAlDestino - CaminoAlObjetivo),

  % Posicion donde caera la ficha despues de comer
  PosCandidataLlegada is PosOrigen - DistanciaDiagonal * CaminoAlDestino.


piezasEnMismaDiagonal(PosOrigen, PosComida, CaminoAlObjetivo, DistanciaDiagonal) :-

  % evalua la posicion de origen con un factor de rango diagonal de +-8 (7 y 9)
  distanciaDiagonal(DistanciaDiagonal),

  % Si la posicion de la ficha comestible es mayor que la de la pieza de origen, obtenemos un valor negativo como  camino al objetivo
  CaminoAlObjetivo is div(PosOrigen - PosComida, DistanciaDiagonal), 0 is mod(PosOrigen - PosComida, DistanciaDiagonal).

distanciaDiagonal(9).
distanciaDiagonal(7).


% calculamos el camino a la posicion de llegada de la ficha despues de comer en base al camino de la misma ficha a la ficha comestible
getCaminoNuevaPosicion(CaminoAlObjetivo, CaminoAlDestino) :-
  %en cualquier caso, ya sea con un camino al objetivo negativo o positivo, la casilla de llegada solo esta una fila más adelante
  CaminoAlObjetivo > 0, CaminoAlDestino is CaminoAlObjetivo + 1;
  CaminoAlObjetivo < 0, CaminoAlDestino is CaminoAlObjetivo - 1.


% Itera sobre todas las posiciones de la diagonal entre el punto de origen y el punto de la pieza objetivo a comer, para verificar que el camino esta despejado
% y así la reina pueda comer desde largas distancias. En el caso del peon, la primera regla se cumple por defecto asi que no necesita evaluar la diagonal
sinPiezasEnmedio(PosOrigen, CaminoAlObjetivo, DistanciaDiagonal, CaminoActual) :-
  CaminoAlObjetivo is CaminoActual;
  Posicion is (PosOrigen - CaminoActual * DistanciaDiagonal), noHayPieza(Posicion), getCaminoNuevaPosicion(CaminoActual, NextCamino), sinPiezasEnmedio(PosOrigen, CaminoAlObjetivo, DistanciaDiagonal, NextCamino).

%%%%%%%%%%%%%%%%%%%%%%%%%
%-----Main del Juego----%
%%%%%%%%%%%%%%%%%%%%%%%%%
%inicializamos el tablero
tableroInicial :-
    b_setval(tablero,
            [nl,pb,nl,pb,nl,pb,nl,pb,
             pb,nl,pb,nl,pb,nl,pb,nl,
             nl,pb,nl,pb,nl,pb,nl,pb,
             va,nl,va,nl,va,nl,va,nl,
             nl,va,nl,va,nl,va,nl,va,
             pn,nl,pn,nl,pn,nl,pn,nl,
             nl,pn,nl,pn,nl,pn,nl,pn,
             pn,nl,pn,nl,pn,nl,pn,nl]), setEstado(tablero).

tableroTestPromocion :-
    b_setval(tablero,
            [nl,pb,nl,pb,nl,pb,nl,pb,
             pb,nl,pb,nl,pb,nl,pb,nl,
             nl,pb,nl,pb,nl,pb,nl,pb,
             va,nl,va,nl,va,nl,va,nl,
             nl,va,nl,va,nl,va,nl,va,
             va,nl,va,nl,pn,nl,pn,nl,
             nl,pb,va,pn,nl,pn,nl,pn,
             va,nl,va,nl,pn,nl,pn,nl]), setEstado(tablero).

tableroComerDoble :-
    b_setval(tablero,
            [nl,pb,nl,pb,nl,pb,nl,pb,
             pb,nl,pb,nl,pb,nl,pb,nl,
             nl,va,nl,pb,nl,pb,nl,pb,
             pb,nl,va,nl,va,nl,va,nl,
             nl,pn,nl,pn,nl,va,nl,pn,
             va,nl,va,nl,pn,nl,pn,nl,
             nl,pn,va,pn,nl,pn,nl,pn,
             pn,nl,pn,nl,va,nl,pn,nl]), setEstado(tablero).

play(Color, Jugador):- %juega el jugador
    sigueJugando, write("Turno de "), write(Jugador),nl,
    turno(Jugador, Color, X, Y, NewX, NewY),
    nl, write('Movimiento: ('), write(X), write(', '), write(Y), write(') a ('), write(NewX), write(' , '), write(NewY), write(').'), nl,
    procesarTurno(Color, X, Y, NewX, NewY),
    printTablero, 
    siguienteJugador(Color, NextColor), siguienteTurno(Jugador, NextJugador), play(NextColor, NextJugador).

play(_, _) :-
    not(sigueJugando),
    ganador(Ganador),
    write(Ganador), write(" gana! ¡Felicidades!").

main:-
  write("Bienvenido a las damas en Prolog."), nl, write("A continuación debera elegir dos jugadores, recuerde que el primero seran las blancas y el segundo las negras"),
  leerJugador(Jugador1, 1), setJugador(jugador1, Jugador1),
  leerJugador(Jugador2, 2), setJugador(jugador2, Jugador2),
  tableroInicial, 
  printTablero, 
  b_getval(jugador1, Jugador), play(blanco, Jugador).

testPromocion:- setJugador(jugador1, 0), setJugador(jugador2, 1),
  tableroTestPromocion, 
  printTablero, 
  b_getval(jugador1, Jugador), play(blanco, Jugador).

testComerDoble:- setJugador(jugador1, 0), setJugador(jugador2, 1),
  tableroComerDoble, 
  printTablero, 
  b_getval(jugador1, Jugador), play(blanco, Jugador).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-----Algoritmo MinMax----%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%obtiene la lista de todos los movimientos posibles para un jugador dado en el tablero actual (estado)
getMovimientosPosibles(Jugador, Lista):-
    findall([A, B, C, D], comprobarJugada(Jugador, A, B, C, D), Lista).


% inicializamos el tablero simulation, con el que la IA trasteara para probar movimientos y resultados
initSimulationTablero:-
	b_getval(tablero, Tablero),
  	b_setval(simulation, Tablero),
    setEstado(simulation). %establece como estado la simulacion


simulaProximoTablero(Jugador, HistorialMovimientos):-
	% Coge el Tablero global sin movimientos simulados para procesar todos los movimientos simulados 
  b_getval(tablero, InitialTablero),

	% trabajamos sobre el tablero inicial
	setTablero(InitialTablero),
  
	% Procesamos los movimientos de la lista de historial de movimientos
	simulaMovimientodeLista(Jugador, HistorialMovimientos).


simulaMovimientodeLista(_, []):- !.
%simula de forma recursiva los movimientos de una lista, alternando de jugador, tal y como se almacena originalmente en el histial de movimientos
simulaMovimientodeLista(Jugador, [[X, Y, NewX, NewY]|Tail]):- procesarTurno(Jugador, X,Y,NewX,NewY), siguienteJugador(Jugador, SiguienteJugador),	simulaMovimientodeLista(SiguienteJugador, Tail).


%algoritmo minimax con poda alfa beta
minimaxIA(Jugador, MejorX, MejorY, MejorXdest, MejorYdest):-
	initSimulationTablero, %crea una copia del tablero como simulacion en la que ejecutar los movimientos para ver el resultado
	getMovimientosPosibles(Jugador, MovimientosPosibles), %encuentra todos los movimientos posibles en cada momento de la simulacion
	findMinMax(Jugador, Jugador, MovimientosPosibles, [], 3, null, null, _, [MejorX, MejorY, MejorXdest, MejorYdest]), %encuentra de forma recursiva la lista que contiene el movimiento más óptimo para el momento inicial de la simulacion. Establecemos la profundidad en 3
	setEstado(tablero). %sale de la simulacion y establece el estado como el tablñero para que se puedan hacer efectivos los cambios


%cuando llega al nivel más profundo, calcula el valor heurístico del nodo hoja y lo devuelve (Hoja Peso, Hoja Movimiento)
findMinMax(Jugador, _, _, HistorialMovimientos, 0, _, _, HojaPeso, HojaMovimiento) :- simulaProximoTablero(Jugador, HistorialMovimientos),	evaluateTablero(Jugador, HojaPeso), nth0(0, HistorialMovimientos, HojaMovimiento).

%si el se acaban los movimiento y lleva a la victoria, su peso es enorme
findMinMax(Jugador, _, [], _, _, _, MovimientoAProcesar, NewPeso, MovimientoAProcesar):- 
	not(sigueJugando),
	ganador(Jugador),
	NewPeso is 500, !.

%en caso de que acabe la partida y no haya ganado, su peso es infimo
findMinMax(_, _, [], _, _, _, MovimientoAProcesar, NewPeso, MovimientoAProcesar):- 
	not(sigueJugando),
	NewPeso is -500, !.

%devuelve el mejor peso de esta profundidad al nivel anterior
findMinMax(_, _, [], _, _, Peso, MovimientoAProcesar, Peso, MovimientoAProcesar):- !.

% funcion recursiva que recorre el arbol
findMinMax(Jugador, Playing, [Movimiento|Tail], HistorialMovimientos, Profundidad, Peso, MovimientoAProcesar, NewMejorPeso, NewMejorMovimiento) :-

	% añade este movimiento a la lista de historial de movimientos para los proximos niveles
	append(HistorialMovimientos, [Movimiento], NewHistorialMovimientos),

	% se simula que Jugador realice el primer movimiento de la lista 
	simulaProximoTablero(Jugador, NewHistorialMovimientos),

  % debemos obtener los posible siguientes movimientos, asi que simulamos que pasado el turno le toca al jugador opuesto
	siguienteJugador(Playing, SiguienteJugador),
	getMovimientosPosibles(SiguienteJugador, NewMovimientosPosibles),
  %reducimos en uno la profundidad en la que estamos buscando en el arbol para así avanzar
	NewProfundidad is Profundidad -1,

	% llamada recursiva al algoritmo para avanzar en profundidad
	findMinMax(Jugador, SiguienteJugador, NewMovimientosPosibles, NewHistorialMovimientos, NewProfundidad, null, null, NewPeso, NewMovimiento),

	% Compara el NewPeso encontrado en este nodo con el mejor Peso calculado respecto de los nodos anteriores
	findMejorPeso(Jugador, Playing, Peso, NewPeso, MejorPeso),
    
	% Selecciona el movimiento asociado al mejor peso calculado en la funcion anterior
	findMejorMovimiento(Peso, NewPeso, MejorPeso, MovimientoAProcesar, NewMovimiento, MejorMovimiento), !,
	
	% llamada recursiva para el siguiente nodo en este mismo nivel, por ello empleamos el mismo valor de profundidad
	findMinMax(Jugador, Jugador, Tail, HistorialMovimientos, Profundidad, MejorPeso, MejorMovimiento, NewMejorPeso, NewMejorMovimiento).

%Comparamos el nuevo peso con el peso del nivel superior,
%Si peso o NewPeso estan null, el mejor peso será aquel del que dispongamos
findMejorPeso(_, _, Peso, null, Peso):- !.
findMejorPeso(_, _, null, NewPeso, NewPeso):- !.

% En caso de que sea el mismo jugador (propio jugador del turno, no el contrincante), el MejorPeso sera Peso si es mayor que NewPeso o, NewPeso en caso contrario (es decir, escogemos el maximo peso))
findMejorPeso(Jugador, Jugador, Peso, NewPeso, Peso):- Peso > NewPeso, !.
findMejorPeso(Jugador, Jugador, _, NewPeso, NewPeso).

% En caso de que sea el jugador contrario, si Peso es el mayor, nos interesa que el mejor peso sea el contrario, es decir, escogemos el minimo
findMejorPeso(_, _, Peso, NewPeso, NewPeso):-	Peso > NewPeso, !.
findMejorPeso(_, _, Peso, _, Peso).


%Si no hay movimiento a procesar(el mejor hasta el momento), el movimiento nuevo es el mejor
findMejorMovimiento(_, _, _, null, NewMovimiento, NewMovimiento).

%si peso es el MejorPeso (en este instante), el movimiento a procesar(el mejor hasta el momento) es el mejor disponible
findMejorMovimiento(Peso, _, Peso, MovimientoAProcesar, _, MovimientoAProcesar).

%si el NewPeso(calculado en este nodo) es el MejorPeso, significa que el NewMovimiento(el disponible en este nodo) es el mejor movimiento posible
findMejorMovimiento(_, NewPeso, NewPeso, _, NewMovimiento, NewMovimiento).


%calcula la puntuacion de un jugador en base al numero de fichas que haya en el tablero para cada color (es decir, la puntuacion de un jugador tambien depende de las fichas de las que dispone su oponentes)
evaluateTablero(blanco, Peso) :-
	count(pb, Npb), count(rb, Nrb), %cuenta piezas blancas
	count(pn, Npn), count(rn, Nrn), %cuenta piezas negras
	Peso is (((2*Nrb) + Npb) - ((2*Nrn) + Npn)). %los reyes valen el doble, total de piezas blancas menos total de piezas negras

evaluateTablero(negro, Peso) :-
	count(pb, Npb), count(rb, Nrb), %cuenta piezas blancas
	count(pn, Npn), count(rn, Nrn), %cuenta piezas negras
	Peso is (((2*Nrn) + Npn) - ((2*Nrb) + Npb)). %los reyes valen el doble, total de piezas negras menos total de piezas blancas


%contador de Pieza, devuelve valor en Res
count(Pieza, Res) :- getTablero(List), countL(List, Pieza, Res, 0).

%contador de lista básico, recorre la lista y suma uno a un valor cada vez que encuentra el valor en la lista
countL( [], _, Res, Res) :- !. % final de lista
countL( [Pieza|Xs], Pieza, Res, Counter) :- !, Counter1 is Counter + 1, countL(Xs, Pieza, Res, Counter1). %si la cabeza de la lista actual coincide con la pieza, suma 1 al contador y llama recursivamente a la funcion con la cola restante
countL( [_|Xs], Pieza, Res, Counter) :- countL(Xs, Pieza, Res, Counter). %cuando el valor no coincide, se llama recursivamente a si misma con la cola restante