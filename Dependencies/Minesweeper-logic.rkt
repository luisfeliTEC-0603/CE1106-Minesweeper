; Dependencies/Minesweeper-logic.rkt
#lang racket

; ================================
; CONFIGURACIÓN & ESTADO DEL JUEGO
; ================================

; Parametros para la configuración del juego
(define ancho-tablero 0)
(define alto-tablero 0)
(define num-minas 0)

; Estructura para el estado del juego
(define init-estado
  (cons null 0)) ; (valores-campo-minas . contador-campos-descubiertos)

; Obtener la matriz de valores del campo de minas desde el estado
(define (get-estado-minas estado)
  (car estado))

; Obtener el contador de campos que han sido descubiertos
(define (get-estado-minas-desc estado)
  (cdr estado))

; Actualizar los valores del campo de minas en el estado
(define (set-estado-val-minas estado valores)
  (cons valores (cdr estado)))

; Actualizar el contador de campos descubiertos
(define (set-estado-minas-desc estado contador)
  (cons (car estado) contador))

; Reiniciar el contador de campos descubiertos a cero
(define (reiniciar-contador-campos-descubiertos estado)
  (cons (car estado) 0))

; Incrementar en 1 el contador de campos descubiertos
(define (incrementar-contador-campos-descubiertos estado)
  (cons (car estado) (+ (cdr estado) 1)))

; ================================
; OPERACIONES SOBRE COORDENADAS
; ================================

; Generar todas las coordenadas de la cuadrícula
(define (generar-coordenadas filas columnas)

  ; Función auxiliar: generar coordenadas para una fila específica
  (define (generar-coordenadas-fila fila columna acumulador)
    (cond
      [(>= columna columnas) acumulador]
      [else (generar-coordenadas-fila fila (+ columna 1) (cons (cons fila columna) acumulador))]))
  
  ; Función auxiliar: generar coordenadas para filas
  (define (generar-todas-filas fila-actual acumulador)
    (cond
      [(>= fila-actual filas) acumulador]
      [else (generar-todas-filas (+ fila-actual 1) 
                     (append (generar-coordenadas-fila fila-actual 0 '()) acumulador))]))
  
  (generar-todas-filas 0 '()))

; Verificar de límites de cooredenadas
(define (coordenada-dentro-limites? fila columna filas columnas)
  (and (>= fila 0) (>= columna 0) (< fila filas) (< columna columnas)))

; Obtener coordenadas de campos adyacentes
(define (get-adyacentes fila columna filas columnas)
  (define todos-adyacentes
    (list (cons (- fila 1) (- columna 1))  ; Arriba-izquierda
          (cons (- fila 1) columna)        ; Arriba
          (cons (- fila 1) (+ columna 1))  ; Arriba-derecha
          (cons fila (- columna 1))        ; Izquierda
          (cons fila (+ columna 1))        ; Derecha
          (cons (+ fila 1) (- columna 1))  ; Abajo-izquierda
          (cons (+ fila 1) columna)        ; Abajo
          (cons (+ fila 1) (+ columna 1)))) ; Abajo-derecha
  
  (filter (lambda (coordenada) 
            (coordenada-dentro-limites? (car coordenada) (cdr coordenada) filas columnas))
          todos-adyacentes))

; Verificar coordenada específica está en lista
(define (coordenada-en-lista? coordenada lista-coordenadas)
  (cond
    [(null? lista-coordenadas) #f]
    [(and (= (car coordenada) (car (car lista-coordenadas)))
          (= (cdr coordenada) (cdr (car lista-coordenadas)))) #t]
    [else (coordenada-en-lista? coordenada (cdr lista-coordenadas))]))

; ================================
; OPERACIONES CON MINAS
; ================================

; Calcular la intersección entre dos listas (adyacentes y minas) 
(define (interseccion-conjuntos conjunto1 conjunto2)
  (cond
    [(null? conjunto1) '()]
    [(coordenada-en-lista? (car conjunto1) conjunto2)
     (cons (car conjunto1) (interseccion-conjuntos (cdr conjunto1) conjunto2))]
    [else (interseccion-conjuntos (cdr conjunto1) conjunto2)]))

; Contar minas adyacentes para una posición
(define (contar-minas-adyacentes fila columna posiciones-minas filas columnas)
  (define coordenadas-adyacentes (get-adyacentes fila columna filas columnas))
  (length (interseccion-conjuntos coordenadas-adyacentes posiciones-minas)))

; Generar una fila incluyendo minas
(define (generar-fila-campo fila columnas posiciones-minas filas-totales)
  (define (generar-columna columna acumulador)
    (cond
      [(>= columna columnas) (reverse acumulador)]
      [else 
       (define posicion-actual (cons fila columna))
       (cond
         ; Si la posición actual tiene mina, colocar "X"
         [(coordenada-en-lista? posicion-actual posiciones-minas)
          (generar-columna (+ columna 1) (cons "X" acumulador))]
         [else
          (define minas-adyacentes (contar-minas-adyacentes fila columna posiciones-minas filas-totales columnas))
          (generar-columna (+ columna 1) (cons minas-adyacentes acumulador))])]))
  
  (generar-columna 0 '()))

; ================================
; OPERACIONES CON LISTAS
; ================================

; Tomar primeros n elementos de una lista
(define (tomar-primeros elementos cantidad)
  (cond
    [(or (<= cantidad 0) (null? elementos)) '()]
    [else (cons (car elementos) (tomar-primeros (cdr elementos) (- cantidad 1)))]))

; Fisher-Yates para shuffle
(define (mezclar-lista elementos)
  (define (iterar elementos-restantes resultado)
    (cond
      [(null? elementos-restantes) resultado]
      [else
       (define indice (random (length elementos-restantes)))
       (define elemento (list-ref elementos-restantes indice))
       (define nuevos-restantes (append (tomar-primeros elementos-restantes indice) 
                                       (eliminar-primeros elementos-restantes (+ indice 1))))
       (iterar nuevos-restantes (cons elemento resultado))]))
  (iterar elementos '()))

; Eliminar elementos de una lista
(define (eliminar-primeros elementos cantidad)
  (cond
    [(or (<= cantidad 0) (null? elementos)) elementos]
    [else (eliminar-primeros (cdr elementos) (- cantidad 1))]))

; Implementación de longitud
(define (longitud-lista elementos)
  (define (iterar elementos contador)
    (cond
      [(null? elementos) contador]
      [else (iterar (cdr elementos) (+ contador 1))]))
  (iterar elementos 0))

; Implementación de append
(define (concatenar-listas lista1 lista2)
  (cond
    [(null? lista1) lista2]
    [else (cons (car lista1) (concatenar-listas (cdr lista1) lista2))]))

; Implementación de list-ref
(define (obtener-elemento-lista elementos indice)
  (cond
    [(null? elementos) (error "Índice fuera de límites")]
    [(= indice 0) (car elementos)]
    [else (obtener-elemento-lista (cdr elementos) (- indice 1))]))

; Implementación de reverse
(define (invertir-lista elementos)
  (define (iterar elementos acumulador)
    (cond
      [(null? elementos) acumulador]
      [else (iterar (cdr elementos) (cons (car elementos) acumulador))]))
  (iterar elementos '()))

; Implementación de filter
(define (filtrar-lista predicado elementos)
  (cond
    [(null? elementos) '()]
    [(predicado (car elementos)) (cons (car elementos) (filtrar-lista predicado (cdr elementos)))]
    [else (filtrar-lista predicado (cdr elementos))]))

; ================================
; OPERACIONES SOBRE TABLERO DE JUEGO
; ================================

; Generar el tablero
(define (generar-tablero filas columnas num-minas)
  (define todas-coordenadas (generar-coordenadas filas columnas))
  (define posiciones-minas (tomar-primeros (mezclar-lista todas-coordenadas) num-minas))
  
  (define (generar-filas fila-actual acumulador)
    (cond
      [(>= fila-actual filas) (invertir-lista acumulador)]
      [else 
       (generar-filas (+ fila-actual 1) 
                     (cons (generar-fila-campo fila-actual columnas posiciones-minas filas) acumulador))]))
  
  (generar-filas 0 '()))

; Obtener valor en coordenadas específicas
(define (obtener-valor-campo fila columna valores-campo)
  (define (obtener-fila fila-actual filas-totales elementos-restantes)
    (cond
      [(null? elementos-restantes) null]
      [(= fila-actual fila) (car elementos-restantes)]
      [else (obtener-fila (+ fila-actual 1) filas-totales (cdr elementos-restantes))]))
  
  (define datos-fila (obtener-fila 0 (longitud-lista valores-campo) valores-campo))
  (cond
    [(null? datos-fila) (error "Fila fuera de límites")]
    [else
     (define (obtener-columna columna-actual columnas-totales elementos-restantes)
       (cond
         [(null? elementos-restantes) null]
         [(= columna-actual columna) (car elementos-restantes)]
         [else (obtener-columna (+ columna-actual 1) columnas-totales (cdr elementos-restantes))]))
     (obtener-columna 0 (longitud-lista datos-fila) datos-fila)]))

; Exportar funciones para ser usadas por el módulo GUI
(provide
 ancho-tablero
 alto-tablero
 num-minas
 init-estado
 get-estado-minas
 get-estado-minas-desc
 set-estado-val-minas
 set-estado-minas-desc
 reiniciar-contador-campos-descubiertos
 incrementar-contador-campos-descubiertos
 generar-coordenadas
 coordenada-dentro-limites?
 get-adyacentes
 generar-tablero
 obtener-valor-campo
)