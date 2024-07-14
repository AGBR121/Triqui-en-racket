#lang racket

#|
- Version del código: 3.0
- Autor: Ing(c) Burbano Rodriguez Angel Gabriel
- Lenguaje utilizado: Racket
- Descripcion del programa: Este programa consiste en un algoritmo para jugar el juego del triqui con solo string.
|#

; Definición de la variable `numbers` que representa el tablero inicial con posiciones numeradas.
(define numbers "1 2 3\n4 5 6\n7 8 9 ")

; Función que toma una cadena `table` y devuelve una representación formateada del tablero de juego.
(define (FormatTable table)
  (string-append 
    "+---+---+---+\n| " 
    (substring table 0 1) " | " (substring table 2 3) " | " (substring table 4 5) " |\n"
    "+---+---+---+\n| " 
    (substring table 6 7) " | " (substring table 8 9) " | " (substring table 10 11) " |\n"
    "+---+---+---+\n| " 
    (substring table 12 13) " | " (substring table 14 15) " | " (substring table 16 17) " |\n+---+---+---+"
  )
)

; Imprime el tablero formateado usando la función `FormatTable`.
(printf (FormatTable numbers))

; Función que toma una tabla, un valor y una letra, y reemplaza el carácter en la posición `value` de `table` con `letter`.
(define (ChangeChar table value letter)
  (string-append 
    (substring table 0 value) letter (substring table (+ value 1))
  )
)

; Función que verifica si hay un ganador en el tablero.
(define (Check table)
  (if (or 
        ; Verifica filas
        (char=? (string-ref table 0) (string-ref table 2) (string-ref table 4))
        (char=? (string-ref table 6) (string-ref table 8) (string-ref table 10))
        (char=? (string-ref table 12) (string-ref table 14) (string-ref table 16))
        ; Verifica columnas
        (char=? (string-ref table 0) (string-ref table 8) (string-ref table 16))
        (char=? (string-ref table 4) (string-ref table 8) (string-ref table 12))
        (char=? (string-ref table 0) (string-ref table 6) (string-ref table 12))
        (char=? (string-ref table 2) (string-ref table 8) (string-ref table 14))
        (char=? (string-ref table 4) (string-ref table 10) (string-ref table 16))
      )
      #t ; Retorna verdadero si hay un ganador.
      #f ; Retorna falso si no hay un ganador.
  )
)

; Función principal del juego que maneja la lógica del turno y el conteo de movimientos.
(define (Game table turn counter)
  (newline)
  (printf "Jugador ~a: Entre número casilla: " turn) ; Solicita al jugador que ingrese el número de la casilla.
  (define respuesta (- (* (read) 2) 2)) ; Lee la respuesta del jugador y calcula el índice correspondiente en la tabla.
  (newline)
  (define newTable (ChangeChar table respuesta (if (= turn 1) "X" "O"))) ;; Actualiza el tablero con la jugada del jugador.
  (if (not (char-numeric? (string-ref table respuesta))) ; Verifica si la casilla ya ha sido jugada.
      (begin
        (printf "Casilla jugada~n")
        (Game table turn counter) ; Llama de nuevo a la función sin cambiar nada si la casilla ya está jugada.
      )
      (begin
        (newline)
        (printf (FormatTable newTable)) ; Imprime el tablero actualizado.
        (newline)
        (if (Check newTable) ; Verifica si hay un ganador después de la jugada.
            (printf "Jugador ~a ha ganado!" turn)
            (if (= counter 9) ; Verifica si se ha alcanzado el máximo de movimientos (empate).
                (printf "Empate")
                (Game newTable (if (= turn 1) 2 1) (+ 1 counter)) ;; Cambia de turno y continúa el juego.
            )
        )
      )
  )
)

; Inicia el juego con el tablero inicial, comenzando con el jugador 1 y un contador de movimientos en 1.
(Game numbers 1 1)
