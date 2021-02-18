#lang racket/base
(require (except-in racket force delay))
(require racket/mpair)

;;;;Ported to Racket by Geoffrey Mainland <mainland@drexel.edu>

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;SECTION 4.1.1

(define (mceval exp env)
  (cond ((self-evaluating? exp) exp)
        ((error? exp) (mcapply (mceval (operator exp) env) '("Metacircular Interpreter Aborted")))
        ((let? exp) (let-eval exp env))
        ((stream? exp) (mceval (stream (cdr exp)) env))
        ((stream-cons? exp) (stream-cons exp))
        ((stream-first? exp) (mceval (stream-first exp) env))
        ((stream-rest? exp) (mceval (stream-rest exp) env))
        ;;;((delay? exp) (mceval (delay->lambda exp env) env))
        ((force? exp) (force (cdr exp) env))
        ((delay? exp) (mceval (delay->lambda exp) env))
        ;;;((delay? exp) (delay exp env))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mceval (cond->if exp) env))
        ((or? exp)(or (operands exp) env))
        ((and? exp)(and (operands exp) env))
        ((seq? exp) (mceval (seq (cdr exp)) env))
        ((application? exp)
         (mcapply (mceval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (stream? exp)
  (tagged-list? exp 'stream))

(define (seq? exp)
  (tagged-list? exp 'seq))

(define (seq exps)
  (cons (make-lambda (list 'v1 'vn) (list 'vn)) exps))

(define (stream exp)
  (cond
    ((null? exp) 'empty-stream)
    ((last-exp? exp) (list 'stream-cons (car exp) 'empty-stream))
    (else (list 'stream-cons (car exp) (stream (cdr exp))))))

(define (stream-cons? exp)
  (tagged-list? exp 'stream-cons))

(define (stream-first? exp)
  (tagged-list? exp 'stream-first))

(define (stream-rest? exp)
  (tagged-list? exp 'stream-rest))

(define (stream-cons exp)
  (list (make-delay (cadr exp)) (make-delay (caddr exp))))

(define (stream-first exp)
 ; (cond
 ;   ((stream? (cadr exp)) (list 'force (stream-cons (stream (cddr exp)))))
  ;  (else
     ;;;(list 'force (car (stream-cons (cadr exp)))))
  (list 'force (make-delay (cadadr exp))))

(define (stream-rest exp)
  (cond
    ((stream? (cadr exp))
     (list 'force (list 'delay (caddr (stream (cdadr exp))))))  
    (else
     (list 'force (cadr (stream-cons (cadr exp)))))))

(define (make-delay exp)
  (list 'delay exp))

;;;; Implement force
(define (force? exp)
  (tagged-list? exp 'force))

(define (force exp env)
  (mcapply (mceval (operator exp) env)
           (list-of-values (operands exp) env)))

;;;; Implement delay
(define (delay? exp)
  (tagged-list? exp 'delay))

(define (delay->lambda exp)
  (list 'memo-proc (list 'lambda '() (cadr exp)))
)

(define (delay exp env)
  (cond ((force? (cadr exp)) (mceval (cadr exp) env))
        (else (delay->lambda exp env))
        ))



;;;(define (memo-proc exp env)
;;  (let-eval
;   '(let ((already-run? false)
;           (result null))
;      (lambda ()
;        (if (not already-run?)
;            (begin (set! result exp)
;                   (set! already-run? true)
;                   result)
;            result))) env))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) (let-vals exp)))

(define (or exps env)
  (cond ((null? exps) #f)
        ((true? (mceval (first-exp exps) env)) #t)
        ((last-exp? exps) (mceval (first-exp exps) env))
        (else (or (rest-exps exps) env))        
        ))

(define (and exps env)
  (cond ((null? exps) #t)
        ((last-exp? exps) (mceval (first-exp exps) env))       
        ;;;;((true? (mceval (first-exp exps) env)) (and (rest-exps exps) env))
        ((false? (mceval (first-exp exps) env)) #f)
        (else (and (rest-exps exps) env))
    ))

(define (mcapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else (mceval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2
(define (error? exp)
  (tagged-list? exp 'error))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (self-evaluating? exp)
  (cond ((number? exp)  true)
        ((string? exp)  true)
        ((char? exp)    true)
        ((boolean? exp) true)
        (else           false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

;;; Implement let

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-vars exp)
  (map car (let-bindings exp)))

(define (let-vals exp)
  (map cadr (let-bindings exp)))

(define (let-body exp)
  (cddr exp))

(define (let-eval exp env)
  (eval-sequence (let-body exp)
                 (extend-environment (let-vars exp)
                      (list-of-values (let-vals exp) env)
                      env)))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'empty-stream empty-stream initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'null null initial-env)
    (eval-definition '(define (not x) (if x false true))
                     initial-env)
    (eval-definition '(define (memo-proc proc)
                        (let ((already-run? false) (result null))
                          (lambda ()(if (not already-run?)
                                        (begin (set! result (proc))
                                               (set! already-run? true)
                                               result)
                                        result)))) initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'list list)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'stream-empty? stream-empty?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list '= =)
        (list 'error error)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ((val (mceval exp (setup-environment))))
    (user-print val)))

(define the-global-environment (setup-environment))

(define input-prompt "> ")

(define (driver-loop)
  (display input-prompt)
  (when (with-handlers
            ([exn:fail? (lambda (exn)
                          (display "Error: ")
                          (display (exn-message exn))
                          (newline)
                          #t)])
          (let ([input (read)])
            (if (eof-object? input)
                (begin
                  (newline)
                  #f)
                (let ([output (mceval input the-global-environment)])
                  (user-print output)
                  (newline)
                  #t))))
    (driver-loop)))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)