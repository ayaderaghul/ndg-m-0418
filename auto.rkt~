#lang racket
(provide (all-defined-out))

;; AUTOMATON
(struct automaton (payoff initial cc cd dc dd) #:transparent)

;; classic automata
(define C (automaton 0 1  1 1 1 1))
(define D (automaton 0 0  0 0 0 0))
(define T (automaton 0 1  1 0 1 0))
(define G (automaton 0 1  1 0 0 0))
(define P (automaton 0 0  0 1 0 1))

(define PAYOFF-TABLE
  (list
   (list (cons 3 3) (cons 0 4))
   (list (cons 4 0) (cons 1 1))))

(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))

;; helper of round-auto and to aid making automaton
;; round to 2 decimal numbers
(define (round2 n)
  (/ (round (* n 100)) 100))

(define (round-auto auto)
  (match-define (automaton pay init cc cd dc dd) auto)
  (automaton pay (round2 init)
             (round2 cc)
             (round2 cd)
             (round2 dc)
             (round2 dd)))

(define (make-random-automaton)
  (define auto  (automaton 0 (random)
                           (random)
                           (random)
                           (random)
                           (random)))
  (round-auto auto))

(define (reset auto)
  (match-define (automaton pay init cc cd dc dd) auto)
  (automaton 0 init cc cd dc dd))

;; PAIR MATCH
;; helper of interact
;; this is to randomise the action C or D according to propensity
;; 0 is C, 1 is D
(define (randomise probability)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list probability 1))]
       #:final (< r p)) s))

;; return details
(define (interact-d au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (reverse results)
          (automaton p1 init1 cc1 cd1 dc1 dd1)
          (automaton p2 init2 cc2 cd2 dc2 dd2)))

;; return short version of details
(define (interact-ds au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (take (reverse results) 20)
   (automaton p1 init1 cc1 cd1 dc1 dd1)
   (automaton p2 init2 cc2 cd2 dc2 dd2)))

;; return no details on round results
(define (interact au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      ; #:final (> (random) delta)
      (match-define (list a1 a2)
        (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values
   (automaton p1 init1 cc1 cd1 dc1 dd1)
   (automaton p2 init2 cc2 cd2 dc2 dd2)))

;; MUTATION
;; helper of mutate, accommodate the flaw in floating numbers
(define (random-decimal prob)
  (define n (inexact->exact (round (* prob 100))))
  (if (zero? prob)
      0
      (round2 (exact->inexact (/ (random n) 100)))))

;; mutate branches
(define (mutate-b prob)
  (define r (random 2))
  (define decrease (random-decimal prob))
  (define increase (random-decimal (round2 (- 1 prob))))
  (if (zero? r)
      (+ prob increase)
      (- prob decrease)))

(define (mutate auto)
  (match-define (automaton pay initial cc cd dc dd) auto)
  (define r (random 5))
  (cond
   [(zero? r) (automaton pay (mutate-b initial) cc cd dc dd)]
   [(= r 1) (automaton pay initial (mutate-b cc) cd dc dd)]
   [(= r 2) (automaton pay initial cc (mutate-b cd) dc dd)]
   [(= r 3) (automaton pay initial cc cd (mutate-b dc) dd)]
   [(= r 4) (automaton pay initial cc cd dc (mutate-b dd))]))
