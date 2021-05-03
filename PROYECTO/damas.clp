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
	(printout t "Especifica quien empieza, jugador 1 (maquina) o jugador 2 (persona): ")
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
	?tablero <- (tablero)
=>
	(bind ?tamano (inicializar_tablero))
	(bind ?jugador (inicializar_primero))
	(assert (tamano ?tamano))
	(assert (jugadorInicial ?jugador))
	(retract ?a)
	(retract ?b)
	(switch ?tamano
		(case 4 then
			(modify ?tablero (fichas o 12 o 14 x 41 x 43))
		)
		(case 6 then
			(modify ?tablero (fichas o 12 o 14 o 16 o 21 o 23 o 25 x 52 x 54 x 56 x 61 x 63 x 65))
		)
		(case 8 then
			(modify ?tablero (fichas o 12 o 14 o 16 o 18 o 21 o 23 o 25 o 27 o 32 o 34 o 36 o 38 x 61 x 63 x 65 x 67 x 72 x 74 x 76 x 78 x 81 x 83 x 85 x 87))
		)
	)
	(if (= ?jugador 1) then
		(assert (turno_maquina))
	else
		(if (= ?jugador 2) then
			(assert (turno_persona))
		)
	)
	(assert (empezar_juego))
)

(defrule movimiento_maquina
	(declare (salience 10))
	(empezar_juego)
	?turno <- (turno_maquina)
	(tamano ?tam)
	?tabl <- (tablero (fichas $?f))
=>
	(printout t "Turno de la maquinota" crlf)
	(printout t "Numero de fichas: " (div ?tam 2) crlf)
	(printout t "Ficha a mover: ")
	(bind ?num_ficha (read))
	; Jugador 2 no puede mover en caso de tablero 4 ni ficha 1 ni ficha 2 
	; No puede seleccionar ninguna ficha menor que el tamaño del tablero entre dos ni mayor que el tamaño ni menor que 0
	(while (or (or (> ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
		(printout t "No puedes seleccionar esa ficha")
		(bind ?num_ficha (read))
	)
	(bind ?ficha_tablero (nth$ (- (* ?num_ficha 2) 1) $?f))
	(bind ?pos_ficha (nth$ (* ?num_ficha 2) $?f))
	(printout t "Posicion de la ficha: " ?pos_ficha crlf)
	
	(printout t "A que fila la quieres mover: ")
	(bind ?fila (read))
	(while (or (> ?fila ?tam) (< ?fila 0)) do
		(printout t "Esa fila está fuera del tablero: ")
		(bind ?fila (read))
	)
	(bind ?fila (* ?fila 10))
	
	(printout t "A que columna la quieres mover: ")
	(bind ?columna (read))
	(while (or (> ?columna ?tam) (< ?columna 0)) do
		(printout t "Esa columna está fuera del tablero: ")
		(bind ?columna (read))
	)
	
	(bind ?nueva_pos_ficha (+ ?fila ?columna))

	;No comprendo este if

	;(if (< ?pos_ficha (* (div ?tam 2) 10)) then
	;else
	;	(if (or (= ?nueva_pos_ficha (- ?pos_ficha 21)) (= ?nueva_pos_ficha (- ?pos_ficha 19))) then
	;		(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
	;		(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
	;		(retract ?turno)
	;		(assert (turno_maquina))
	;	else
	;		(printout t "Movimiento inválido" crlf)
	;		(modify ?tabl (fichas $?f))
	;	)
	;)

	;Si:
		;	La posicion nueva es igual a la posicion anterior + 9 o + 11
	(if (or (= ?nueva_pos_ficha (+ ?pos_ficha 9)) (= ?nueva_pos_ficha (+ ?pos_ficha 11))) then
		(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
		(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
		(retract ?turno)
		(assert (turno_maquina))
	else
		(printout t "Movimiento inválido" crlf)
		(modify ?tabl (fichas $?f))
	)
	
	
)

(defrule movimiento_persona
	(empezar_juego)
	?turno <- (turno_persona)
	(tamano ?tam)
	?tabl <- (tablero (fichas $?f))
=>
	(printout t "Turno de la persona" crlf)
	(printout t "Numero de fichas: " (div ?tam 2) crlf)
	(printout t "Ficha a mover: ")
	(bind ?num_ficha (read))
	(while (or (or (< ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
		(printout t "No puedes seleccionar esa ficha")
		(bind ?num_ficha (read))
	)
	(bind ?ficha_tablero (nth$ (- (* ?num_ficha 2) 1) $?f))
	(bind ?pos_ficha (nth$ (* ?num_ficha 2) $?f))
	(printout t "Posicion de la ficha: " ?pos_ficha crlf)
	
	(printout t "A que fila la quieres mover: ")
	(bind ?fila (read))
	(while (> ?fila ?tam) do
		(printout t "Esa fila está fuera del tablero: ")
		(bind ?fila (read))
	)
	(bind ?fila (* ?fila 10))
	(printout t "A que columna la quieres mover: ")
	(bind ?columna (read))
	(while (> ?columna ?tam) do
		(printout t "Esa columna está fuera del tablero: ")
		(bind ?columna (read))
	)
	(bind ?nueva_pos_ficha (+ ?fila ?columna))
	
	(if (< ?pos_ficha (* (div ?tam 2) 10)) then

		(if (or (= ?nueva_pos_ficha (- ?pos_ficha 21)) (= ?nueva_pos_ficha (- ?pos_ficha 19))) then
			(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
			(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
			(retract ?turno)
			(assert (turno_maquina))
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	else
		(if (or (= ?nueva_pos_ficha (- ?pos_ficha 21)) (= ?nueva_pos_ficha (- ?pos_ficha 19))) then
			(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
			(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
			(retract ?turno)
			(assert (turno_maquina))
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	)

)
;TODO: Problemas con que no se repite indefinidamente comer_ficha
(defrule comer_ficha
	(declare (salience 100))
	; Caso de la maquina
	; Si (Ficha jugador se encuentra en ?pos_ficha + 9 o + 11)
	;  - Modify tablero quitando ficha comida
	(empezar_juego)
	(tamano ?tam)
	?turno <- (turno_maquina)
	?tabl <- (tablero (fichas $?f))
=>

	;(if (= ?turno turno_maquina) then
		(loop-for-count (?i 1 (div ?tam 2))
			(bind ?ficha_tablero1 (nth$ (- (* ?i 2) 1) $?f))
			(bind ?pos_ficha1 (nth$ (* ?i 2) $?f))
			(loop-for-count (?j (div ?tam 2) ?tam)
				(bind ?ficha_tablero2 (nth$ (- (* ?j 2) 1) $?f))
				(bind ?pos_ficha2 (nth$ (* ?j 2) $?f))
				(if (or (= ?pos_ficha1 (+ ?pos_ficha2 9)) (= ?pos_ficha1 (+ ?pos_ficha2 11))) then
					; guardar en una variable que ficha se puede comer desde que otra ficha
					(printout t "Ficha:" ?ficha_tablero1 "se puede comer a:" ?ficha_tablero2 crlf)
					(retract ?turno)
					(assert (turno_maquina))
				)
			)
		)
	;)
)




























