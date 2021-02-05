#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "more-scheme.rkt")

(provide more-scheme-tests)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers
  (integers-from 0))

;; Map a function over a list with left-to-right order of evaluation guaranteed
(define (map-ltor f xs)
  (if (null? xs)
      null
      (let [(y (f (first xs)))]
        (cons y (map-ltor f (rest xs))))))

(define more-scheme-tests
  (test-suite
   "More Scheme Tests"

   (test-suite
    "stream-scan"
    
    (test-case
     "stream-scan base case"
     (check-equal? (stream->list (stream-scan + 0 empty-stream))
                   '(0)))
    
    (test-case
     "stream-scan non-zero base case"
     (check-equal? (stream->list (stream-scan + 10 empty-stream))
                   '(10)))
    
    (test-case
     "stream-scan inductive case (+)"
     (check-equal? (stream->list (stream-scan + 0 (stream 1 2 3 4 5 6)))
                   '(0 1 3 6 10 15 21)))
    
    (test-case
     "stream-scan inductive case (*)"
     (check-equal? (stream->list (stream-scan * 1 (stream 1 2 3 4 5 6)))
                   '(1 1 2 6 24 120 720))))

   (test-suite
    "stream-take-n"
    
    (test-case
     "stream-take-n base case with empty stream"
     (check-equal? (stream-take-n 0 empty-stream)
                   '()))
    
    (test-case
     "stream-take-n base case"
     (check-equal? (stream-take-n 0 integers)
                   '()))
    
    (test-case
     "(stream-take-n 10 integers)"
     (check-equal? (stream-take-n 10 integers)
                   '(0 1 2 3 4 5 6 7 8 9)))
    
    (test-case
     "stream-scan of infinite stream"
     (check-equal? (stream-take-n 7 (stream-scan + 0 (integers-from 1)))
                   '(0 1 3 6 10 15 21))))
   
   (test-suite
    "stream-pair-with"

    (test-case
     "(stream-pair-with (lambda (x) (+ x 1)) (1 2 3 4))"
     (check-equal? (stream->list (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4)))
                   '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))))
   
   (test-suite
    "cycle-streams"

    (test-case
     "(stream-take-n 8 (cycle-streams (stream 1 2 3) (stream \"a\" \"b\")))"
     (check-equal? (stream-take-n 8 (cycle-streams '(1 2 3) '("a" "b")))
                   '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b"))))

    (test-case
     "(stream-take-n 8 (cycle-streams (integers-from 0) (integers-from 1))"
     (check-equal? (stream-take-n 8 (cycle-streams (integers-from 0) (integers-from 1)))
                   '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6) (6 . 7) (7 . 8)))))

   (test-suite
    "seen"
    
    (test-case
     "(seen #f) (seen #f)"
     (check-equal? (map-ltor seen '(#f #f))
                   '(#f #t)))
    
    (test-case
     "(seen null) (seen null)"
     (check-equal? (map-ltor seen '(null null))
                   '(#f #t)))
    
    (test-case
     "(seen 1)"
     (check-equal? (seen 1)
                   #f))
    
    (test-case
     "(seen 2) (seen 2)"
     (check-equal? (map-ltor seen '(2 2))
                   '(#f #t)))
    
    (test-case
     "(seen 3) (seen 4)"
     (check-equal? (map-ltor seen '(3 4))
                   '(#f #f)))
    
    (test-case
     "(seen 10) (seen 11) (seen 12) (seen 13) (seen 14) (seen 15) (seen 16) (seen 17) (seen 10)"
     (check-equal? (map-ltor seen '(10 11 12 13 14 15 16 17 10))
                   '(#f #f #f #f #f #f #f #f #t)))
    
    (test-case
     "(begin (seen 'a) (seen 'b) (seen 'c) (seen 'd) (seen 'e) (seen 'a))"
     (check-equal? (map-ltor seen '(a b c d e a))
                   '(#f #f #f #f #f #t))))))

    
