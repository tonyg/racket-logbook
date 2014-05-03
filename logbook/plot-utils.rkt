#lang racket/base

(require racket/match)

(require "main.rkt")

(provide logbook-table->points
	 logbook-plot-columns
	 @)

(define logbook-plot-columns (make-parameter '(0 1)))

(define ((@ . path) datum)
  (let walk ((path path) (datum datum))
    (match path
      ['() datum]
      [(cons (? number? n) rest)
       (walk rest (cond
		   [(pair? datum) (list-ref datum n)]
		   [(vector? datum) (vector-ref datum n)]
		   [(hash? datum) (hash-ref datum n)]
		   [(procedure? datum) (datum n)]))]
      [(cons other rest)
       (walk rest (cond
		   [(pair? datum) ((case other [(car first) car] [(cdr rest) cdr]) datum)]
		   [(hash? datum) (hash-ref datum other)]
		   [(procedure? datum) (datum other)]))])))

(define (logbook-table-column-index T name)
  (define spec (logbook-table-column-spec T))
  (when (not (list? spec))
    (error 'logbook-table-column-index
	   "Cannot search non-list column-spec ~v of table ~a for name ~v"
	   spec
	   (logbook-table-name T)
	   name))
  (match (member name spec)
    [#f (error 'logbook-table-column-index
	       "Column ~v not found in column-spec ~v of table ~a"
	       name
	       spec
	       (logbook-table-name T))]
    [tail (- (length spec) (length tail))]))

(define (logbook-table->points T #:columns [columns #f])
  (for/list ((r (read-logbook-data T)))
    (for/list ((c (or columns (logbook-plot-columns))))
      ((@ c) r))))
