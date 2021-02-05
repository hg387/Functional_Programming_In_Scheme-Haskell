;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;;
;; Storing the current value in the list
;; Applying f to the z and called the stream-scan recurively with this new values on the rest of l.
;; until the stream becomes empty
(define (stream-scan f z l)
  (if (stream-empty? l)
      (stream-cons z empty-stream)
      (stream-cons z (stream-scan f (f z (stream-first l)) (stream-rest l)))))

;;
;; Problem 2
;; Storing the first element of the stream in the list for every call
;; Subtracting 1 from n with the every recursive call on rest of the stream
;; until the stream becomes empty(for cases, length of stream < n) OR n becomes 0
(define (stream-take-n n s)
  (if (or (stream-empty? s) (equal? n 0))
      null
      (cons (stream-first s) (stream-take-n (- n 1) (stream-rest s)))))

;;
;; Problem 3
;; Using Stream map on the passed stream
;; The function mapped just return the pair of elements
;; where first element is the stream element mapped from
;; and the second element is calculated by calling f on the stream element mapped from
(define (stream-pair-with f s)
  (stream-map (lambda (x) (cons x (f x))) s))

;;
;; Problem 4
;; On each call of make-pairs, we are checking if any of the local refs of streams are empty
;; Any empty stream refs got replaced with the original reference of the respective streams
;; With that, function is building a new stream of pairs of first elements taken from each stream.
;; As Stream is a lazy eval, this solution runs in O(1)
(define (cycle-streams xs ys)
  (define (make-pairs tmpxs tmpys)
    (cond
      [(and (stream-empty? tmpxs) (stream-empty? tmpys))
       (stream-cons (cons (stream-first xs) (stream-first ys))  (make-pairs (stream-rest xs) (stream-rest ys))) ]
      [(stream-empty? tmpxs)
       (stream-cons (cons (stream-first xs) (stream-first tmpys))  (make-pairs (stream-rest xs) (stream-rest tmpys))) ]
      [(stream-empty? tmpys)
       (stream-cons (cons (stream-first tmpxs) (stream-first ys))  (make-pairs (stream-rest tmpxs) (stream-rest ys))) ]
      [else
       (stream-cons (cons (stream-first tmpxs) (stream-first tmpys))  (make-pairs (stream-rest tmpxs) (stream-rest tmpys))) ]))
  (make-pairs xs ys))

;;
;; Problem 5
;; Taking tmp as the local variable which initially is empty list
;; On each call to seen, adding the argument passed in the tmp and assigning the new list to tmp
;; Also, on each call before adding the argument to the tmp, checking if argument exists in the current tmp list
;; If not, ultimately returns false and vice-versa
(define seen
  (let ((tmp null))
    (lambda (x)
      (if (null? (filter (lambda (y) (equal? y x)) tmp))
        (begin (set! tmp (cons x tmp)) #f)
        #t))))
