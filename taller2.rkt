#lang racket
;; Taller 2 – Programación Declarativa 
;; Cada ejercicio usa map/filter/foldl/ormap y recursión donde corresponde.

;; Ej.1: Contar elementos positivos
(define (count-positives xs)
  (length (filter (λ (n) (> n 0)) xs)))

;; Ej.2: Generar lista de cuadrados pares
(define (even-squares xs)
  (map (λ (n) (* n n))
       (filter even? xs)))

;; Ej.3: Factorial (recursión simple)
(define (fact n)
  (cond
    [(negative? n) (error 'fact "n must be >= 0")]
    [(zero? n) 1]
    [else (* n (fact (sub1 n)))]))

;; Ej.4: Elevar cada número al cubo (lambda + map)
(define (cubes xs)
  (map (λ (n) (* n n n)) xs))

;; Ej.5: Sumar elementos impares (filter + foldl)
(define (sum-odds xs)
  (foldl + 0 (filter odd? xs)))

;; Ej.6: ¿La lista contiene números negativos? (ormap)
(define (any-negative? xs)
  (ormap negative? xs))

;; Ej.7: Suma acumulada de una lista (foldl creando acumulador)
(define (prefix-sums xs)
  (cdr
   (reverse
    (foldl (λ (x acc) (cons (+ x (car acc)) acc))
           (list 0)
           xs))))

;; Ej.8: Concatenar cadenas en una lista (foldl sobre strings)
(define (concat-strings xs)
  (foldl (λ (x acc) (string-append acc x)) "" xs))

;; Ej.9: Doble de los números > 5 (filter + map)
(define (double-gt5 xs)
  (map (λ (n) (* 2 n))
       (filter (λ (n) (> n 5)) xs)))

;; Ej.10: Invertir el orden de una lista (foldl)
(define (my-reverse xs)
  (foldl (λ (x acc) (cons x acc)) '() xs))

;; Ej.11: Función que recibe una función como parámetro (orden superior)
(define (apply-to-list f xs)
  (map f xs))

;; Ej.12: Reto integrador — promedio de números > 5 (map + filter + foldl)
(define (avg-gt5 xs)
  (let* ([ys    (filter (λ (n) (> n 5)) xs)]
         [total (foldl + 0 ys)]
         [len   (length ys)])
    (if (zero? len) 0.0 (exact->inexact (/ total len)))))

;; ================== Pruebas rápidas (según el enunciado) ==================
(module+ main
  (printf "1) ~a\n"  (count-positives '(3 -2 7 0 -5 9)))           
  (printf "2) ~a\n"  (even-squares '(1 2 3 4 5 6 7 8)))             
  (printf "3) ~a\n"  (fact 5))                                      
  (printf "4) ~a\n"  (cubes '(2 3 4)))                              
  (printf "5) ~a\n"  (sum-odds '(1 2 3 4 5 6 7)))                   
  (printf "6) ~a\n"  (any-negative? '(5 9 -3 2)))                   
  (printf "7) ~a\n"  (prefix-sums '(1 2 3 4)))                      
  (printf "8) ~a\n"  (concat-strings '("Hola" " " "Mundo")))        
  (printf "9) ~a\n"  (double-gt5 '(3 6 8 2 10)))                    
  (printf "10) ~a\n" (my-reverse '(1 2 3 4)))                      
  (printf "11) ~a\n" (apply-to-list (λ (n) (* n n)) '(1 2 3 4)))    
  (printf "12) ~a\n" (avg-gt5 '(3 8 10 4 9 2 7))))                  
