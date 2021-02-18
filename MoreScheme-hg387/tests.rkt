#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "scheme.rkt")

(provide scheme-tests)

(define scheme-tests
  (test-suite
   "Scheme Homework Tests"
   
   (test-suite
    "sigma"
    (test-case "(sigma 0 0)"
               (check-equal? (sigma 0 0)
                             0))
    (test-case "(sigma 0 5)"
               (check-equal? (sigma 0 5)
                             15))
    (test-case "(sigma 162 10056)"
               (check-equal? (sigma 162 10056)
                             50553555)))

   (test-suite
    "log"
    (test-case "(log 33 1)"
               (check-equal? (log 33 1)
                             0))
    (test-case "(log 5 100)"
               (check-equal? (log 5 100)
                             2))
    (test-case "(log 3 4782968)"
               (check-equal? (log 3 4782968)
                             13)))

   (test-suite
    "choose"
    (test-case "(choose 55 1)"
               (check-equal? (choose 55 1)
                             55))
    (test-case "(choose 303 303)"
               (check-equal? (choose 303 303)
                             1))
    (test-case "(choose 13 4)"
               (check-equal? (choose 13 4)
                             715)))

   (test-suite
    "binary"
    (test-case "(binary 0)"
               (check-equal? (binary 0)
                             0))
    (test-case "(binary 1)"
               (check-equal? (binary 1)
                             1))
    (test-case "(binary 2)"
               (check-equal? (binary 2)
                             10))
    (test-case "(binary 13)"
               (check-equal? (binary 13)
                             1101))
    (test-case "(binary 7134)"
               (check-equal? (binary 7134)
                             1101111011110)))

   (test-suite
    "scan"
    (test-case "scan base case"
               (check-equal? (scan + 0 '())
                             '(0)))
    (test-case "scan inductive case"
               (check-equal? (scan + 0 '(1 2 3 4 5 6))
                             '(0 1 3 6 10 15 21))))))
