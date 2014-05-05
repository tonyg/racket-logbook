#lang racket/base

(require racket/match)

(require "store.rkt")

(provide (struct-out logbook-note)
	 get-logbook-note-history
	 get-logbook-note
	 set-logbook-note!)

(struct logbook-note (text created-time) #:transparent)

(define (notes-table E)
  (logbook-table E "notes" "notes" #:create? #t))

(define (decode-entry n)
  (logbook-note (bytes->string/utf-8 (logbook-datum-data n))
		(logbook-datum-created-time n)))

(define (get-logbook-note-history* E label)
  (raw-logbook-data (notes-table E) #:label label))

(define (get-logbook-note-history E-or-T)
  (define (fixup ns) ;; assumes sorted ascending by id (== ascending by time-of-entry)
    (map decode-entry (reverse ns)))
  (match E-or-T
    [(? logbook-entry? E) (fixup (get-logbook-note-history* E ""))]
    [(? logbook-table? T) (fixup (get-logbook-note-history* (logbook-table-entry T)
							    (logbook-table-name T)))]))

(define (get-logbook-note E-or-T)
  (match (get-logbook-note-history E-or-T)
    ['() #f]
    [(cons n _) n]))

(define (set-logbook-note! E-or-T text)
  (define-values (E label)
    (match E-or-T
      [(? logbook-entry? E) (values E "")]
      [(? logbook-table? T) (values (logbook-table-entry T) (logbook-table-name T))]))
  (raw-logbook-datum! (notes-table E) (string->bytes/utf-8 text) #:label label)
  (void))
