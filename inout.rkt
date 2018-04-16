#lang racket

(require "csv.rkt" "auto.rkt")
(require plot csv-reading racket/string)
(plot-new-window? #t)
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

(define (resurrect-a automata)
  (for/list ([i (in-list automata)])
    (define a (string-split i ")"))
    (match-define (list b1 b2) (map string-split a))
    (define c (rest b1))
    (define d (map string->number c))
    (resurrect d)))

(define (resurrect-ethnic string)
  (define a (string-split string ")"))
  (match-define (list b1 b2) (map string-split a))
  (define c1 (drop b1 1))
  (define c2 (drop b2 1))
  (define d1 (map string->number c1))
  (define this-many (string->number (first c2)))
  (build-vector this-many (lambda (_) (resurrect d1))))

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


;; PLOT                                                                        

(define (population-mean->lines data)
  (define coors                                                                
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (λ (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-mean data delta rounds pic tit)
  (define h (* 8 (compound delta rounds)))
  (define m (* 5 (compound delta rounds)))
  (define l (* 2 (compound delta rounds)))
  (define h-line                                                               
    (function (λ (x) h) #:color "red"))
  (define m-line                                                                    (function (λ (x) m) #:color "green"))
  (define l-line                                                                    (function (λ (x) l) #:color "blue"))
  (plot (list h-line m-line l-line
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 h) #:width 1200 #:height 800
        #:out-file pic #:title tit))

(define (plot-mean-p data delta rounds)
  (define h (* 8 (compound delta rounds)))
  (define m (* 5 (compound delta rounds)))
  (define l (* 2 (compound delta rounds)))
  (define h-line (function (λ (x) h) #:color "red"))
  (define m-line (function (λ (x) m) #:color "green"))
  (define l-line (function (λ (x) l) #:color "blue"))
  (plot (list h-line m-line l-line
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 h) #:width 1200 #:height 800))

;; plot character

(define (plot-character au au-file)
  (define xs '(0 1 2 3 4))
  (define (gen-map type color label)
    (define result (match-classics type))
    (lines (map list xs result) 
           #:width 2
           #:color color 
           #:label label))
  (define au-map (gen-map au 'yellow "automaton"))
  (define low-map (gen-map L 'brown "lows"))
  (define medium-map (gen-map M 'green "medium"))
  (define high-map (gen-map H 'red "high"))
  (define acc-map (gen-map A 'blue "accommodator"))
  (plot (list au-map low-map medium-map high-map acc-map)
        #:y-min -5 #:y-max 1000 
        #:x-min 0 #:x-max 7
        #:x-label "match with: 0. itself    1. L    2. M    3. H    4. A"
        #:y-label "payoff"
        #:legend-anchor 'top-right #:width 600
        #:out-file au-file))
