#lang racket/base

(require racket/match)
(require racket/string)
(require plot)

(require "../main.rkt")
(require "../plot-utils.rkt")

(provide (all-defined-out))

(define (table-filename T)
  (define E (logbook-table-entry T))
  (format "~a-~a-~a.png"
	  (logbook-entry-project E)
	  (logbook-entry-name E)
	  (logbook-table-name T)))

(define (do-plot L project entry-name entry-type table table-type
		 plot-title plot-columns plot-output plot-size font-size)
  (match-define (list w h)
    (if plot-size
	(map string->number (string-split plot-size "x"))
	(list (plot-width) (plot-height))))
  (parameterize ((plot-width w)
		 (plot-height h)
		 (plot-font-size (if font-size font-size (plot-font-size))))
    (define E (if entry-name
		  (logbook-entry L project entry-name entry-type #:create? #f)
		  (latest-logbook-entry L project #:type entry-type)))
    (define T (logbook-table E table table-type #:create? #f))
    (define ps (logbook-table->points T #:columns plot-columns))
    (define-values (x-label y-label)
      (match (logbook-table-column-spec T)
	[#f (values (plot-x-label) (plot-y-label))]
	[ss (values ((@ (car plot-columns)) ss) ((@ (cadr plot-columns)) ss))]))
    (plot-new-window? #t)
    (match plot-output
      ['screen
       (plot (list (lines ps) (points ps))
	     #:x-label x-label #:y-label y-label
	     #:title (or plot-title (logbook-table-fullname T)))]
      ['stdout
       (plot-file (list (lines ps) (points ps)) (current-output-port)
		  #:x-label x-label #:y-label y-label
		  #:title (or plot-title (logbook-table-fullname T)))]
      ['default-file
       (plot-file (list (lines ps) (points ps)) (table-filename T)
		  #:x-label x-label #:y-label y-label
		  #:title (or plot-title (logbook-table-fullname T)))]
      [(? string? filename)
       (plot-file (list (lines ps) (points ps)) filename
		  #:x-label x-label #:y-label y-label
		  #:title (or plot-title (logbook-table-fullname T)))])))

;; (module+ test
;;   (plot-new-window? #t)
;;   (define L (open-logbook ':memory: #:verbose? #t))
;;   (define E (logbook-entry L "project1" "entry1" "etype1"))
;;   (define T (logbook-table E "table1" "ttype1"))
;;   (write-logbook-datum! T '(10 10))
;;   (write-logbook-datum! T '(20 20))
;;   (write-logbook-datum! T '(30 30))
;;   (logbook-table-plot-points T))
