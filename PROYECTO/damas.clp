; TODO: 
;
;	- Automatizar (LA PARTE INTELIGENTE, QUE LA MÁQUINA JUEGE SOLA)
;
;	- Cuando llegue al final eliminar dicha ficha y sumar un punto.
;
;       - Hacer que se llegue a victoria cuando 2 fichas llegan al final o todas las fichas son comidas
;
;	- Hacer que las fichas comidas y/o que estén en posición de victoria (ficha en posición  0) no se puedan utilizar.


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
	(se_puede_comer)
	(control 0)
)

(defrule inicializacion
	?a <- (inicializar_tablero)
	?b <- (inicializar_primero)
	?tablero <- (tablero)
=>
	(bind ?tamano (inicializar_tablero))
	(bind ?jugador (inicializar_primero))
	(assert (jugadorInicial ?jugador))
	(retract ?a)
	(retract ?b)
	(switch ?tamano
		(case 4 then
			(modify ?tablero (fichas o 12 o 14 x 41 x 43))
			(assert (tamano 4))
		)
		(case 6 then
			(modify ?tablero (fichas o 23 o 25 o 0 o 0 o 0 o 0 x 34 x 52 x 54 x 0 x 0 x 0))
			(assert (tamano 12))
		)
		(case 8 then
			(modify ?tablero (fichas o 12 o 14 o 16 o 18 o 21 o 23 o 25 o 27 o 32 o 34 o 36 o 38 x 61 x 63 x 65 x 67 x 72 x 74 x 76 x 78 x 81 x 83 x 85 x 87))
			(assert (tamano 24))
		)
	)
	(if (= ?jugador 1) then
		(assert (turno 1)) ;Maquina
	else
		(if (= ?jugador 2) then
			(assert (turno 2)) ;Persona
		)
	)
        (assert (puntuacionPersona 0))
        (assert (puntuacionMaquina 0))
	(assert (empezar_juego))
)

(defrule movimiento_maquina
	(declare (salience 10))
	(empezar_juego)
	(jugadorInicial ?jugIni)
	?turnoMaq <- (turno ?turn)
	(tamano ?tam)
	?tabl <- (tablero (fichas $?f))
	(test (= ?turn 1))
	?contr <- (control ?val_control) ;Control de turnos enc caso de que se pueda hacer un movimineto de comida doble
        ?punt <- (puntuacionMaquina ?puntos)
=>
	(retract ?contr)
	(assert (control 0))
	(printout t "Turno de la maquinota" crlf)
	(printout t "Numero de fichas: " (div ?tam 2) crlf)
	(printout t "Ficha a mover: ")
	(bind ?num_ficha (read))
	; Jugador 2 no puede mover en caso de tablero 4 ni ficha 1 ni ficha 2 
	; No puede seleccionar ninguna ficha menor que el tamaño del tablero entre dos ni mayor que el tamaño ni menor que 0
	(if (= ?jugIni 1) then
		(while (or (or (> ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
			(printout t "No puedes seleccionar esa ficha" crlf)
			(printout t "Ficha a mover: ")
			(bind ?num_ficha (read))
		)
	else
		(while (or (or (<= ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
			(printout t "No puedes seleccionar esa ficha" crlf)
			(printout t "Ficha a mover: ")
			(bind ?num_ficha (read))
		)
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
	(bind ?puede_mover 1)
	;Si:
	;La posicion nueva es igual a la posicion anterior + 9 o + 11 (si son blancas) -9 o -11 (si son negras)
	(if (= ?jugIni 1) then ;Blancas
		(if (or (= ?nueva_pos_ficha (+ ?pos_ficha 9)) (= ?nueva_pos_ficha (+ ?pos_ficha 11))) then
			(loop-for-count (?i 1 ?tam)
				(bind ?pos_ficha2 (nth$ (* ?i 2) $?f))
				(if (or (= ?pos_ficha2 ?nueva_pos_ficha) (= ?pos_ficha2 ?nueva_pos_ficha)) then
					(bind ?puede_mover 0)
				)
			)
			(if (= ?puede_mover 1) then
				(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
				(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
				(retract ?turnoMaq)
				(assert (turno 2))

                                ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos 
                                (if (= ?fila 80) then
                                        (bind ?puntos (+ ?puntos 1))
                                        (retract ?punt)
                                        (assert (puntuacionMaquina ?puntos))
                                )
			else
				(printout t "Movimiento inválido" crlf)
				(modify ?tabl (fichas $?f))
			)
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	else ;Negras
		(if (or (= ?nueva_pos_ficha (- ?pos_ficha 9)) (= ?nueva_pos_ficha (- ?pos_ficha 11))) then
			(loop-for-count (?i 1 ?tam)
				(bind ?pos_ficha2 (nth$ (* ?i 2) $?f))
				(if (or (= ?pos_ficha2 ?nueva_pos_ficha) (= ?pos_ficha2 ?nueva_pos_ficha)) then
					(bind ?puede_mover 0)
				)
			)
			(if (= ?puede_mover 1) then
				(bind ?puede_mover 0)
				(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
				(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
				(retract ?turnoMaq)
				(assert (turno 2))

                                ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos 
                                (if (= ?fila 10) then
                                        (bind ?puntos (+ ?puntos 1))
                                        (retract ?punt)
                                        (assert (puntuacionMaquina ?puntos))
                                )

			else
				(printout t "Movimiento inválido" crlf)
				(modify ?tabl (fichas $?f))
			)
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	)
	(assert (se_puede_comer))
)

(defrule movimiento_persona
	(declare (salience 10))
	(empezar_juego)
	(jugadorInicial ?jugIni)
	?turnoPer <- (turno ?turn)
	(tamano ?tam)
	?tabl <- (tablero (fichas $?f))
	(test (= ?turn 2))
	?contr <- (control ?val_control) ;Control de turnos enc caso de que se pueda hacer un movimineto de comida doble
        ?punt <- (puntuacionPersona ?puntos)
=>
	(retract ?contr)
	(assert (control 0))
	(printout t "Turno de la persona" crlf)
	(printout t "Numero de fichas: " (div ?tam 2) crlf)
	(printout t "Ficha a mover: ")
	(bind ?num_ficha (read))


	(if (= ?jugIni 2) then
		(while (or (or (> ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
			(printout t "No puedes seleccionar esa ficha" crlf)
			(printout t "Ficha a mover: ")
			(bind ?num_ficha (read))
		)
	else
		(while (or (or (<= ?num_ficha (div ?tam 2)) (> ?num_ficha ?tam)) (< ?num_ficha 0))
			(printout t "No puedes seleccionar esa ficha" crlf)
			(printout t "Ficha a mover: ")
			(bind ?num_ficha (read))
		)
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
	(bind ?puede_mover 1)

	;La posicion nueva es igual a la posicion anterior + 9 o + 11 (si son blancas) -9 o -11 (si son negras)
	(if (= ?jugIni 2) then
		(if (or (= ?nueva_pos_ficha (+ ?pos_ficha 9)) (= ?nueva_pos_ficha (+ ?pos_ficha 11))) then
			(loop-for-count (?i 1 ?tam)
				(bind ?pos_ficha2 (nth$ (* ?i 2) $?f))
				(if (or (= ?pos_ficha2 ?nueva_pos_ficha) (= ?pos_ficha2 ?nueva_pos_ficha)) then
					(bind ?puede_mover 0)
				)
			)
			(if (= ?puede_mover 1) then
				(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
				(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
				(retract ?turnoPer)
				(assert (turno 1))

                                ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos 
                                (if (= ?fila 80) then
                                        (bind ?puntos (+ ?puntos 1))
                                        (retract ?punt)
                                        (assert (puntuacionPersona ?puntos))
                                )

			else
				(printout t "Movimiento inválido" crlf)
				(modify ?tabl (fichas $?f))
			)
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	else
		(if (or (= ?nueva_pos_ficha (- ?pos_ficha 9)) (= ?nueva_pos_ficha (- ?pos_ficha 11))) then
			(loop-for-count (?i 1 ?tam)
				(bind ?pos_ficha2 (nth$ (* ?i 2) $?f))
				(if (or (= ?pos_ficha2 ?nueva_pos_ficha) (= ?pos_ficha2 ?nueva_pos_ficha)) then
					(bind ?puede_mover 0)
				)
			)
			(if (= ?puede_mover 1) then
				(printout t "Ficha movida a: " ?nueva_pos_ficha crlf)
				(modify ?tabl (fichas (replace$ $?f (* ?num_ficha 2) (* ?num_ficha 2) ?nueva_pos_ficha)))
				(retract ?turnoPer)
				(assert (turno 1))

                                ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos 
                                (if (= ?fila 10) then
                                        (bind ?puntos (+ ?puntos 1))
                                        (retract ?punt)
                                        (assert (puntuacionPersona ?puntos))
                                )
			else 
				(printout t "Movimiento inválido" crlf)
				(modify ?tabl (fichas $?f))
			)		
		else
			(printout t "Movimiento inválido" crlf)
			(modify ?tabl (fichas $?f))
		)
	)

	(assert (se_puede_comer))

)

(deffunction efectuar_el_zampe (?posibles_comidas ?f ?control)
	(printout t ?posibles_comidas crlf)

	(if (> (length$ ?posibles_comidas) 0) then
		(printout t "Fichas que pueden comer:" crlf)
		(bind ?i 1)
		(while (< ?i (length$ ?posibles_comidas))
			;Ficha 21 se puede comer a la ficha 32 en la posicion 43
			(printout t "Ficha " (nth$ ?i ?posibles_comidas) " en la posicion: " (nth$ (* (nth$ ?i ?posibles_comidas) 2) ?f) " se puede comer a la ficha " (nth$ (+ ?i 1) ?posibles_comidas) " en la posicion: " (nth$ (* (nth$ (+ ?i 1) ?posibles_comidas) 2) ?f) " yendo a la posicion: " (nth$ (+ ?i 2) ?posibles_comidas) crlf)
			(bind ?i (+ ?i 3))
		)
		(if (> ?i 6) then
			(printout t "Que ficha quieres que coma 1 o 2: ")
			(bind ?ficha (read))
			(while (> ?ficha (/ (- ?i 1) 2)) do
				(printout t "Que ficha quieres que coma  ")
				(bind ?ficha (read))
			)

		else
			(printout t "SE HA COMIDO LA FICHA COMIBLE YA QUE ES EL MOVIMIENTO OBLIGATORIO" crlf)
			(bind ?ficha 1)
		)
		(assert (se_puede_comer))
		(if (> ?ficha 1) then
			(bind ?ficha (+ ?ficha 2))
			(printout t ?ficha crlf)
		)
		(return ?ficha)
	)

        ;Estos solo se devuelven cuando ya no hay posibles comidas, para el control de turnos.
	(if (= ?control 1) then
		(return -1)
	else
		(return 0)
	)
	
)




;TODO: Problemas con que no se repite indefinidamente comer_ficha
(defrule comer_ficha
	(declare (salience 11))
	(empezar_juego)
	(tamano ?tam)
	(jugadorInicial ?jugIni)
	?tur <- (turno ?turn)
	?tabl <- (tablero (fichas $?f))
	?comer <- (se_puede_comer)
	?contr <- (control ?val_control)
        ?pPer <- (puntuacionPersona ?puntosPersona)
        ?pMaq <- (puntuacionMaquina ?puntosMaquina)

=>
	
	; Fichas que comen indica que ficha come a queotra ficha y a donde se va a mover como consecuencia
	; En fichas_que_comen en caso de existir posicion 00 significa que no se come ninguna ficha

	; Algoritmo:
		; por cada ficha blanca
			; por cada ficha negra
				; Si la posicion de la negra es equivalente a un posible movimiento de la blanca:
					; Si no nos salimos del tablero:
						; por cada ficha negra adicional:
							; si a donde se moveria en caso de comer no hay ninguna ficha negra mas:
								; puede_comer true
	(retract ?comer) ;Para que no buclee infinitamente para revisar si puede comer

        ; Para las blancas
	(if (or (and (= ?jugIni 1)(= ?turn 1)) (and (= ?jugIni 2)(= ?turn 2))) then
		(bind ?puede_comer 1)
		(bind ?ya_ha_comido 0)
		(bind $?fichas_que_comen (create$ ))
		(loop-for-count (?i 1 (div ?tam 2))
			(bind ?pos_ficha1 (nth$ (* ?i 2) $?f))
			(loop-for-count (?j (+ (div ?tam 2) 1) ?tam)
				(bind ?pos_ficha2 (nth$ (* ?j 2) $?f))
				(if (or (= ?pos_ficha2 (+ ?pos_ficha1 9)) (= ?pos_ficha2 (+ ?pos_ficha1 11))) then
					(printout t "primer if" crlf)
					(bind ?diagonal (+ ?pos_ficha2 (- ?pos_ficha2 ?pos_ficha1)))
					(if (or (!= (div ?diagonal 10 ) 0) (< (div ?diagonal 10) ?tam) (!= (mod ?diagonal 10) 0) (< (mod ?diagonal 10) ?tam )) then
						(printout t "segundo if" crlf) 
						(loop-for-count (?k 1 ?tam)
							(bind ?pos_ficha_en_diag (nth$ (* ?k 2) $?f))
							(printout t ?diagonal " " ?pos_ficha_en_diag crlf)
							(if (= ?diagonal ?pos_ficha_en_diag) then
								;Aquí puede comer
								(printout t "en el if del puede comer" crlf)
								(bind ?puede_comer 0)
							)
						)
					)
					(if (= ?puede_comer 1) then
						(printout t "guardamos las fichas a comer" crlf)
						(printout t "i: " ?i " j: " ?j " diagonal: " ?diagonal crlf)
						(bind ?fichas_que_comen (insert$ ?fichas_que_comen 1 (create$ ?i ?j ?diagonal)))
						(printout t ?fichas_que_comen " fichitas insertadas" crlf)
					)
				)
			)
		)

		(bind ?ficha (efectuar_el_zampe ?fichas_que_comen ?f ?val_control))
		(printout t ?ficha crlf)
		(if (> ?ficha 0) then
			;(bind ?pos_ficha_comer (nth$ (* (nth$ ?ficha ?fichas_que_comen) 2) ?f)
			;(bind ?pos_ficha_comida (nth$ (* (nth$ (+ ?ficha 1) ?fichas_que_comen) 2) ?f))
			(bind ?pos_nueva_ficha (nth$ (+ ?ficha 2) ?fichas_que_comen))
			(bind ?f (replace$ $?f (* (nth$ ?ficha ?fichas_que_comen)2) (* (nth$ ?ficha ?fichas_que_comen)2) ?pos_nueva_ficha))
			(bind ?f (replace$ $?f (* (nth$ (+ ?ficha 1) ?fichas_que_comen)2) (* (nth$ (+ ?ficha 1) ?fichas_que_comen)2) 0))
			(modify ?tabl (fichas ?f))
			(retract ?contr)
			(assert (control 1))

                        ;CONTROL DE PUNTUACIÓN
                        (if (and (= ?jugIni 1)(= ?turn 1)) then ;Vemos si es la maquina
                                (if (>= ?pos_nueva_ficha 81) then ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos un punto
                                        (bind ?puntosMaquina (+ ?puntosMaquina 1))
                                        (retract ?pMaq)
                                        (assert (puntuacionMaquina ?puntosMaquina))
                                )
                        else
                                (if (and (= ?jugIni 2)(= ?turn 2)) then ;Vemos si es la persona
                                        (if (>= ?pos_nueva_ficha 81) then ;Miramos si la posición es de victoria, es decir, que la fila sea 8. Si lo es añadimos un punto
                                                (bind ?puntosPersona (+ ?puntosPersona 1))
                                                (retract ?pMaq)
                                                (assert (puntuacionMaquina ?puntosPersona))
                                        )
                                )
                        )

                ;CONTROL DE TURNOS
		else 
			(if (= ?ficha -1) then
				(if (and (= ?jugIni 1)(= ?turn 1)) then
					(retract ?tur)
					(assert (turno 2))
				)
				(if (and (= ?jugIni 2)(= ?turn 2)) then
					(retract ?tur)
					(assert (turno 1))
				)
			)
		)
	)


        ; Para las negras
	(if (or (and (= ?jugIni 2)(= ?turn 1)) (and (= ?jugIni 1)(= ?turn 2))) then
		(bind ?puede_comer 1)
		(bind ?ya_ha_comido 0)
		(bind $?fichas_que_comen (create$ ))
		;(retract ?contr)
		(loop-for-count (?i (+ (div ?tam 2) 1) ?tam)
			(bind ?pos_ficha1 (nth$ (* ?i 2) $?f))
			(loop-for-count (?j 1 (div ?tam 2))
				(bind ?pos_ficha2 (nth$ (* ?j 2) $?f))
				(if (or (= ?pos_ficha2 (- ?pos_ficha1 9)) (= ?pos_ficha2 (- ?pos_ficha1 11))) then
					(printout t "primer if" crlf)
					(bind ?diagonal (+ ?pos_ficha2 (- ?pos_ficha2 ?pos_ficha1)))
					(if (or (!= (div ?diagonal 10 ) 0) (< (div ?diagonal 10) ?tam) (!= (mod ?diagonal 10) 0) (< (mod ?diagonal 10) ?tam )) then
						(printout t "segundo if" crlf) 
						(loop-for-count (?k 1 ?tam)
							(bind ?pos_ficha_en_diag (nth$ (* ?k 2) $?f))
							(printout t ?diagonal " " ?pos_ficha_en_diag crlf)
							(if (= ?diagonal ?pos_ficha_en_diag) then
								;Aquí puede comer
								(printout t "en el if del puede comer" crlf)
								(bind ?puede_comer 0)
							)
						)
					)
					(if (= ?puede_comer 1) then
						(printout t "guardamos las fichas a comer" crlf)
						(printout t "i: " ?i " j: " ?j " diagonal: " ?diagonal crlf)
						(bind ?fichas_que_comen (insert$ ?fichas_que_comen 1 (create$ ?i ?j ?diagonal)))
						(printout t ?fichas_que_comen " fichitas insertadas" crlf)
					)
				)
			)
		)

		(bind ?ficha (efectuar_el_zampe ?fichas_que_comen ?f ?val_control))
		(printout t ?ficha crlf)
		(if (> ?ficha 0) then
			;(bind ?pos_ficha_comer (nth$ (* (nth$ ?ficha ?fichas_que_comen) 2) ?f)
			;(bind ?pos_ficha_comida (nth$ (* (nth$ (+ ?ficha 1) ?fichas_que_comen) 2) ?f))
			(bind ?pos_nueva_ficha (nth$ (+ ?ficha 2) ?fichas_que_comen))
			(bind ?f (replace$ $?f (* (nth$ ?ficha ?fichas_que_comen)2) (* (nth$ ?ficha ?fichas_que_comen)2) ?pos_nueva_ficha))
			(bind ?f (replace$ $?f (* (nth$ (+ ?ficha 1) ?fichas_que_comen)2) (* (nth$ (+ ?ficha 1) ?fichas_que_comen)2) 0))
			(modify ?tabl (fichas ?f))
			(retract ?contr)
			(assert (control 1))

                        ;CONTROL DE PUNTUACIÓN
                        (if (and (= ?jugIni 2)(= ?turn 1)) then ;Vemos si es la maquina
                                (if (and (>= ?pos_nueva_ficha 11) (< ?pos_nueva_ficha 20)) then ;Miramos si la posición es de victoria, es decir, que la fila sea 1. Si lo es añadimos un punto
                                        (bind ?puntosMaquina (+ ?puntosMaquina 1))
                                        (retract ?pMaq)
                                        (assert (puntuacionMaquina ?puntosMaquina))
                                )
                        else
                                (if (and (= ?jugIni 2)(= ?turn 2)) then ;Vemos si es la persona
                                        (if (and (>= ?pos_nueva_ficha 11) (< ?pos_nueva_ficha 20)) then ;Miramos si la posición es de victoria, es decir, que la fila sea 1. Si lo es añadimos un punto
                                                (bind ?puntosPersona (+ ?puntosPersona 1))
                                                (retract ?pMaq)
                                                (assert (puntuacionMaquina ?puntosPersona))
                                        )
                                )
                        )
			
		else
			(if (= ?ficha -1) then
				(if (and (= ?jugIni 2)(= ?turn 1)) then
					(retract ?tur)
					(assert (turno 2))
				)
				(if (and (= ?jugIni 1)(= ?turn 2)) then
					(retract ?tur)
					(assert (turno 1))
				)
			)
		)
	)	
)

(defrule hay_victoria
        (declare (salience 30))
        ?pPers <- (puntuacionPersona ?puntosPersona)
        ?pMaq <- (puntuacionMaquina ?puntosMaquina)

=>
        
        ;Miramos si la persona ha llegado a 2 puntos, es decir, 2 fichas en el final
        (if (= ?puntosPersona 2) then
                (printout t "VICTORIA" crlf)
                (printout t "Ha ganado el jugador" crlf)
                (halt)
        )
        ;Miramos si la mmáquina ha llegado a 2 puntos, es decir, 2 fichas en el final
        (if (= ?puntosMaquina 2) then
                (printout t "VICTORIA" crlf)
                (printout t "Ha ganado la maquinota" crlf)
                (halt)
        )
)