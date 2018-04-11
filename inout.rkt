#lang racket

(require "csv.rkt" "auto.rkt")
(require csv-reading racket/string)

(provide (all-defined-out))

;; IN
(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

(define (input->numbers data)
  (map string->number (flatten data)))

(define (get-position n ls)
  (define l (length ls))
  (define ls2
    (for/list ([i (in-range l)][j (in-list ls)])
      #:break (equal? n j)
      (equal? n j)))
  (length ls2))

(define (population-at data cycle total)
  (define x (- total cycle))
  (define raw  (flatten data))
  (define start (get-position (number->string x) raw))
  (define x2 (- x 100))
  (define end  (get-position (number->string x2) raw))
  (drop (take raw end) (add1 start)))

(define (resurrect automata)
  (for/list ([i (in-list automata)])
    (define a (string-split i ")"))
    (match-define (list b1 b2) (map string-split a))
    (define c (drop b1 1))
    (define d (map string->number c))
    (apply automaton d)))

(define (resurrect-ethnic string)
  (define a (string-split string ")"))
  (match-define (list b1 b2) (map string-split a))
  (define c1 (drop b1 1))
  (define c2 (drop b2 1))
  (define d1 (map string->number c1))
  (define this-many (string->number (first c2)))
  (build-vector this-many (lambda (_) (apply automaton d1))))

(define (resurrect-p data)
  (define a (drop (flatten data) 1))
  (define ethnics (map resurrect-ethnic a))
  (apply vector-append ethnics))
 
(define (resurrect-po data)
  (define ethnics (map resurrect-ethnic data))
  (apply vector-append ethnics)) 

;; OUT
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-rank cycles rankings rank-file)
  (define sorted (sort (hash->list rankings) > #:key cdr))
  (define l (length sorted))
  (define export-this (if (> l 20) (take sorted 20) sorted))
  (out-data rank-file (list (list cycles)))
  (out-data rank-file (map list export-this)))

(define (out-population cycles rankings p-file)
  (out-data p-file (list (list cycles)))
  (out-data p-file (map list (sort (hash->list rankings) > #:key cdr))))
