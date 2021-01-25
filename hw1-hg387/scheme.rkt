;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;; Assuming the m <= n,
;; then increasing m unitl becomes equal to n 

(define (sigma m n)
  (if (equal? m n)
      n
      (+ m (sigma (+ m 1) n))))
;;
;; Problem 2
;; base case n < m, return 0
;; otherwise recursively add 1 with the new argument to be an integer = (n / m) 

(define (log m n)
  (if (< n m)
      0
      (+ 1 (log m (quotient n m)))))

;;
;; Problem 3
;;
;; base cases of n C 0 and n C n
;; otherwise use the recurrence relation recursivelly 
;; in any case where n < k, return 0
(define (choose n k)
  (cond
    [(< n k) 0]
    [(equal? n 0) 1]
    [(equal? k 0) 1]
    [(equal? n k) 1]
    [else (+ (choose (- n 1) (- k 1)) (choose (- n 1) k))]))

;;
;; Problem 4
;; Adding remainder of n / 2 in each recursion call at th place
;; n = 5
;; 5 // 2 -> 1 
;; 2 // 2 -> 0 (Tenth place 10*10*0)
;; 1 // 2 -> 1 (Hundrenth place 10*10*10*1)
;; 5 in binary -> 101

(define (binary n)
  (if (equal? n 0)
      0
      (+ (remainder n 2) (* 10 (binary (quotient n 2))))))

;;
;; Problem 5
;; returning null-empty list in the base case
;; otherwise similar to foldl, evaluating new z by applying f to (first l) and current z
;; recursively calling scan with this new z and (rest l) inside cons

(define (scan f z l)
  (if (null? l)
      (cons z '())
      (cons z (scan f (f z (car l)) (cdr l)))))
