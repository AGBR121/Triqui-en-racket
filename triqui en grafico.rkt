#lang racket

;; Requerir el módulo de gráficos
(require graphics/graphics)

;; Abrir la ventana gráfica
(open-graphics)

;; Definir el viewport para el juego de triqui (Tres en línea)
(define triqui (open-viewport "Triqui" 750 600))

;; Dibujar las líneas horizontales del tablero
((draw-line triqui) (make-posn 0 200) (make-posn 600 200))
((draw-line triqui) (make-posn 0 400) (make-posn 600 400))

;; Dibujar las líneas verticales del tablero
((draw-line triqui) (make-posn 200 0) (make-posn 200 600))
((draw-line triqui) (make-posn 400 0) (make-posn 400 600))

;; Definir el estado inicial del tablero con números del 1 al 9
(define textTriqui "1 2 3 4 5 6 7 8 9")

;; Definir los nombres de los archivos de imagen para "X" y "O"
(define imgx "imgx.png")
(define imgo "imgo.png")

;; Función para cambiar un carácter en una posición específica de una cadena
(define (Changechar str char pos)
  (string-append (substring str 0 pos) char (substring str (+ pos 1)))
)

;; Función para verificar si hay un ganador en el tablero
(define (Check table)
  (if (or (char=? (string-ref table 0) (string-ref table 2) (string-ref table 4))
          (char=? (string-ref table 6) (string-ref table 8) (string-ref table 10))
          (char=? (string-ref table 12) (string-ref table 14) (string-ref table 16))
          (char=? (string-ref table 0) (string-ref table 8) (string-ref table 16))
          (char=? (string-ref table 4) (string-ref table 8) (string-ref table 12))
          (char=? (string-ref table 0) (string-ref table 6) (string-ref table 12))
          (char=? (string-ref table 2) (string-ref table 8) (string-ref table 14))
          (char=? (string-ref table 4) (string-ref table 10) (string-ref table 16)))
      #t ;; Retorna verdadero si hay un ganador
      #f ;; Retorna falso si no hay un ganador
  )
)

;; Función principal del juego que maneja la lógica del turno y el conteo de movimientos
(define (Game turn string counter)
  ;; Obtener la posición del clic del ratón
  (define ubication (get-mouse-click triqui))
  (define x (posn-x (mouse-click-posn ubication)))
  (define y (posn-y (mouse-click-posn ubication)))

  ;; Calcular la posición en el tablero basada en las coordenadas del clic
  (define respuesta (- (* (+ (+ 1 (quotient x 200)) (* (quotient y 200) 3)) 2) 2))

  ;; Actualizar el estado del tablero con el movimiento del jugador
  (define newString (if (<= x 600)
                        (Changechar string (if (= turn 1) "X" "O") respuesta)
                        1))

  (if (> x 600)
      ;; Si el clic está fuera del tablero
      (begin
        (printf "Movimiento invalido~n")
        ((draw-string triqui) (make-posn 601 300) "Movimiento invalido" "red")
        (sleep 1)
        ((clear-string triqui) (make-posn 601 300) "Movimiento invalido")
        (Game turn string counter) ;; Reintentar el movimiento
      )
      ;; Si el clic está dentro del tablero
      (if (not (char-numeric? (string-ref string respuesta)))
          ;; Si la casilla ya está jugada
          (begin
            (printf "Casilla jugada~n")
            ((draw-string triqui) (make-posn 601 300) "Casilla jugada" "red")
            (sleep 3)
            ((clear-string triqui) (make-posn 601 300) "Casilla jugada")
            (Game turn string counter) ;; Reintentar el movimiento
          )
          ;; Si la casilla está libre
          (begin
            (newline)
            (printf newString)
            (((draw-pixmap-posn (if (= turn 1) imgx imgo)) triqui)
             (make-posn (* (quotient x 200) 200) (* (quotient y 200) 200)))
            (newline)
            (if (Check newString)
                ;; Si hay un ganador
                (begin
                  (printf "Jugador ~a ha ganado!" turn)
                  (if (= turn 1)
                      ((draw-string triqui) (make-posn 615 300) "Jugador 1 ha ganado!" "blue")
                      ((draw-string triqui) (make-posn 615 300) "Jugador 2 ha ganado!" "blue"))
                )
                ;; Si no hay un ganador
                (if (= counter 9)
                    ;; Si se ha alcanzado el máximo de movimientos (empate)
                    (begin
                      (printf "Empate")
                      ((draw-string triqui) (make-posn 615 300) "Empate" "dark green")
                    )
                    ;; Continuar el juego cambiando de turno y aumentando el contador
                    (Game (if (= turn 1) 2 1) newString (+ 1 counter))
                )
            )
          )
      )
  )
)

;; Iniciar el juego con el jugador 1, el estado inicial del tablero y el contador de movimientos en 1
(Game 1 textTriqui 1)
