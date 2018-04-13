#lang racket
(provide (all-defined-out))

;; AUTOMATON
(struct automaton (payoff initial action-plan) #:transparent)

(struct action (l m) #:transparent)

;; round to 2 decimal numbers
(define (round2 n)
  (/ (round (* n 100)) 100))

;; accommodate the flaw in floating numbers
(define (random-decimal prob)
  (define n (inexact->exact (round (* prob 100))))
  (if (zero? n)
      0
      (round2 (exact->inexact (/ (random n) 100)))))

(define (generate-random-action-vector)
  (define l (random))
  (define m (random-decimal (- 1 l)))
  (action (round2 l) (round2 m)))

(define (generate-random-action-plan)
  (define (generate-random-contingency)
    (for/list ([i (in-range 3)])
      (generate-random-action-vector)))
  (for/list ([i (in-range 3)])
    (generate-random-contingency)))

(define (make-random-automaton)
  (automaton 0
             (generate-random-action-vector)
             (generate-random-action-plan)))

;; classic automata
(define L (automaton 0 (action 1 0)
                     (list 
                      (list (action 1 0) (action 1 0) (action 1 0))
                      (list (action 1 0) (action 1 0) (action 1 0))
                      (list (action 1 0) (action 1 0) (action 1 0)))))

(define M (automaton 0 (action 0 1)
                     (list 
                      (list (action 0 1) (action 0 1) (action 0 1))
                      (list (action 0 1) (action 0 1) (action 0 1))
                      (list (action 0 1) (action 0 1) (action 0 1)))))
(define H (automaton 0 (action 0 0)
                     (list 
                      (list (action 0 0) (action 0 0) (action 0 0))
                      (list (action 0 0) (action 0 0) (action 0 0))
                      (list (action 0 0) (action 0 0) (action 0 0)))))

(define A (automaton 0 (action 0 1)
                     (list
                      (list (action 0 0) (action 0 1) (action 1 0))
                      (list (action 0 0) (action 0 1) (action 1 0))
                      (list (action 0 0) (action 0 1) (action 1 0)))))
;; flatten
(define (flatten-automaton auto)
  (match-define (automaton pay init plan) auto)
  (define (flatten-action act)
    (match-define (action l m) act)
    (list l m))
  (define (flatten-contingency cont)
    (map flatten-action cont))
  (define (flatten-plan p)
    (map flatten-contingency p))
  (flatten (list 0 (flatten-action init) (flatten-plan plan))))
;; helper of round-auto and to aid making automaton
(define (round-action-scheme act)
  (match-define (action l m) act)
  (action (round2 l) (round2 m)))
(define (round-contingency contingency)
  (map round-action-scheme contingency))
(define (round-action-plan plan)
  (map round-contingency plan))

(define (round-auto auto)
  (match-define (automaton pay init plan) auto)
  (automaton
   0
   (round-action-scheme init)
   (round-action-plan plan)))

(define (reset auto)
  (match-define (automaton pay init plan) auto)
  (automaton 0 init plan))

(define PAYOFF-TABLE
  (list
   (list (cons 2 2) (cons 2 5) (cons 2 8))
   (list (cons 5 2) (cons 5 5) (cons 0 0))
   (list (cons 8 2) (cons 0 0) (cons 0 0))))

(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))


;; PAIR MATCH
;; helper of interact
;; this is to randomise the action C or D according to propensity
;; 0 is C, 1 is D
(define (randomise action-scheme)
  (match-define (action l m) action-scheme)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list l m 1))]
       #:final (< r p)) s))

(define (what-next? action1 action2 plan)
  (list-ref (list-ref plan action1) action2))

;; return details
(define (interact-d au1 au2 rounds delta)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action1 action2 plan2)))
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
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (reverse results)
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))

;; return short version of details
(define (interact-ds au1 au2 rounds delta)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action1 action2 plan2)))
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
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (take (reverse results) 20)
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))

;; return no details of round results

(define (interact au1 au2 rounds delta)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action1 action2 plan2)))
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
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values 
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))


;; MUTATION
(define (mutate-a action-scheme)
  (match-define (action l m) action-scheme)
  (define what-action? (random 2))
  (define total (+ l m))
  (define increase (random-decimal (round2 (- 1 total))))
  (define (mutate-helper act)
    (define r (random 2))
    (define decrease (random-decimal act))
    (if (zero? r)
        (round2 (+ act increase))
        (round2 (- act decrease)))) 
  (if (zero? what-action?)
      (action (mutate-helper l) m)
      (action l (mutate-helper m))))
      
(define (mutate-c posn contingency)
  (match-define (list l m h) contingency)
  (cond
   [(zero? posn) (list (mutate-a l) m h)]
   [(= posn 1) (list l (mutate-a m) h)]
   [(= posn 2) (list l m (mutate-a h))]))

(define (mutate auto)
  (match-define (automaton pay initial plan) auto)
  (match-define (list l- m- h-) plan)
  (define r (random 10))
  (cond
   [(zero? r) (automaton pay (mutate-a initial) plan)]
   
   [(<= r 3) (automaton pay initial (list (mutate-c (- r 1) l-) m- h-))]
   [(<= r 6) (automaton pay initial (list l- (mutate-c (- r 4) m-) h-))]
   [(<= r 9) (automaton pay initial (list l- m- (mutate-c (- r 7) h-)))]))

