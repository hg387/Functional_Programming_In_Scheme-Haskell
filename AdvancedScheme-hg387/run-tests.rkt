#lang racket
(require rackunit/text-ui)
(require "more-scheme.rkt")
(require "tests.rkt")

(when (not (eq? (run-tests more-scheme-tests) 0))
    (exit 1))
