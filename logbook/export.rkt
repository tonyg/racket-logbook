#lang racket/base

(require racket/match)

(require "main.rkt")

(provide export-entry
	 import-entry)

(define (export-entry E)
  (match-define (<logbook-entry> _ _ project name type created-time) E)
  `(logbook-entry (version 0)
		  (project ,project)
		  (name ,name)
		  (type ,type)
		  (created-time ,created-time)
		  (tables
		   ,@(for/list [(T (reverse (logbook-tables E)))] ;; go newest-last
		       (match-define (<logbook-table> _ _ _ name type column-spec created-time) T)
		       `(table (name ,name)
			       (type ,type)
			       (column-spec ,column-spec)
			       (created-time ,created-time)
			       (rows
				,@(for/list [(R (raw-logbook-data T))]
				    (match-define (<logbook-datum> _ _ _ label data created-time) R)
				    `(,label ,data ,created-time))))))))

(define (import-entry book X)
  (match X
    [`(logbook-entry (version 0)
		     (project ,project)
		     (name ,name)
		     (type ,type)
		     (created-time ,created-time)
		     (tables ,tables ...))
     (log-info "logbook: Updating entry ~a/~a:~a" project name type)
     (define E (logbook-entry book project name type
			      #:create? #t
			      #:created-time created-time))
     (define entry-name name)
     (for [(TX tables)]
       (match TX
	 [`(table (name ,name)
		  (type ,type)
		  (column-spec ,column-spec)
		  (created-time ,created-time)
		  (rows ,rows ...))
	  (log-info "logbook: Updating table ~a/~a/~a:~a" project entry-name name type)
	  (define T (logbook-table E name type
				   #:column-spec column-spec
				   #:create? #t
				   #:created-time created-time))
	  (for [(RX rows)]
	    (match RX
	      [`(,label ,data ,created-time)
	       (raw-logbook-datum! T data #:label label #:created-time created-time)]))]))]))
