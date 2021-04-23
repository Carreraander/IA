(deftemplate tablero
	(multislot fichas)
	(slot prof (type INTEGER))
	(slot padre)
)

(deffunction inicializar_tablero ()
	(printout t "Especifica si el tablero es 4, 6 u 8: ")
	(bind ?tablero_tamano (read))
	(printout t "Tamano " ?tablero_tamano "x" ?tablero_tamano crlf)
	(return ?tablero_tamano)
)

(deffunction inicializar_primero ()
	(printout t "Especifica quien empieza, jugador 1 o jugador 2: ")
	(bind ?jugador (read))
	(printout t  "Empieza el jugador: " ?jugador crlf)
	(return ?jugador)
)

(deffacts juego "Inicializacion del juego"
	(tablero (fichas)(prof 0)(padre 0))
	(inicializar_tablero)
	(inicializar_primero)
)

(defrule inicializacion
	?a <- (inicializar_tablero)
	?b <- (inicializar_primero)
=>
	(bind ?tamano (inicializar_tablero))
	(bind ?jugador (inicializar_primero))
	(assert (tamano ?tamano))
	(assert (jugadorInicial ?jugador))
	(retract ?a)
	(retract ?b)
)