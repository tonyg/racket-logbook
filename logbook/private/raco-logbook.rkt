#lang racket/base

(require racket/pretty)
(require racket/set)
(require racket/match)
(require racket/cmdline)
(require raco/command-name)
(require racket/string)
(require racket/port)

(require csv-reading)

(require "../main.rkt")
(require "../export.rkt")

(provide (all-defined-out))

(define (do-plot . args)
  (define real-do-plot (dynamic-require 'logbook/private/raco-logbook-plot 'do-plot))
  (apply real-do-plot args))

(define (do-serve . args)
  (apply (dynamic-require 'logbook/private/raco-logbook-serve 'serve-logbook) args))

(define (do-load-csv L project entry entry-type table table-type column-names)
  (define rows (csv->list (current-input-port)))
  (cond
   [(eq? column-names 'first-line)
    (set! column-names (car rows))
    (set! rows (cdr rows))]
   [(string? column-names)
    (set! column-names (string-split column-names ","))])
  (define E (logbook-entry L project entry entry-type))
  (define T (logbook-table E table table-type #:column-spec column-names))
  (for ((r rows))
    (write-logbook-datum! T
			  (map (lambda (v)
				 (cond
				  [(string->number v)]
				  [else v]))
			       r))))

(define (values->csv-row vs)
  (string-join (for/list ((v vs))
		 (cond
		  [(number? v)
		   (number->string v)]
		  [else
		   (define s (with-output-to-string (lambda () (display v))))
		   (string-append "\""
				  (string-replace s "\"" "\"\"")
				  "\"")]))
	       ","))

(define (do-dump* row-handler L project entry-name entry-type table table-type)
  (define E (if entry-name
		(logbook-entry L project entry-name entry-type #:create? #f)
		(latest-logbook-entry L project #:type entry-type)))
  (define T (logbook-table E table table-type #:create? #f))
  (when (logbook-table-column-spec T)
    (row-handler (logbook-table-column-spec T)))
  (for ((r (read-logbook-data T)))
    (row-handler r)))

(define (do-dump-csv . args)
  (apply do-dump* (lambda (r) (printf "~a\n" (values->csv-row r))) args))

(define (do-dump-sexp . args)
  (apply do-dump* (lambda (r) (write r) (newline)) args))

(define (list-entries L project entry-type)
  (for ((E (logbook-entries L #:project project #:type entry-type)))
    (printf "~a\t~a\t~a\t~a\n"
	    (logbook-entry-project E)
	    (logbook-entry-name E)
	    (logbook-entry-type E)
	    (logbook-entry-created-time E))))

(define (list-table E T)
  (printf "~a\t~a\t~a\t~a\t~a\t~a\t~v\n"
	  (logbook-entry-project E)
	  (logbook-entry-name E)
	  (logbook-entry-type E)
	  (logbook-table-name T)
	  (logbook-table-type T)
	  (logbook-table-created-time T)
	  (logbook-table-column-spec T)))

(define (list-all-tables L entry-type table-type)
  (for ((E (logbook-entries L #:type entry-type)))
    (for ((T (logbook-tables E #:type table-type)))
      (list-table E T))))

(define (list-project-tables L project entry-type table-type)
  (for ((E (logbook-entries L #:type entry-type)))
    (for ((T (logbook-tables E #:type table-type)))
      (list-table E T))))

(define (list-tables L project entry entry-type table-type)
  (define E (logbook-entry L project entry entry-type #:create? #f))
  (when E
    (for ((T (logbook-tables E #:type table-type)))
      (list-table E T))))

(define (list-prefs L)
  (define seen (set))
  (for ((D (list->set (logbook-prefs/detail L #f))))
    (when (not (set-member? seen D))
      (match-define (list project entry-type table-type table-name name value) D)
      (printf "~a\t~a\t~a\t~a\t~a\t~a\n" project entry-type table-type table-name name value)
      (set! seen (set-add seen D)))))

(define (set-pref L project entry-type table-type table-name pref-name pref-value)
  (set-logbook-pref! L project
		     #:entry_type entry-type
		     #:table_type table-type
		     #:table_name table-name
		     (string->symbol pref-name)
		     (with-input-from-string pref-value read)))

(define (delete-pref L project entry-type table-type table-name pref-name)
  (delete-logbook-pref! L project
			#:entry_type entry-type
			#:table_type table-type
			#:table_name table-name
			(string->symbol pref-name)))

(define (do-export L project entry-type entry)
  (for [(E (logbook-entries L #:project project #:name entry #:type entry-type))]
    (pretty-write (export-entry E))
    (newline)))

(define (do-import L)
  (let loop ()
    (define X (read))
    (when (not (eof-object? X))
      (import-entry L X)
      (loop))))

(define (get-entry-note L project entry entry-type)
  (define E (logbook-entry L project entry entry-type #:create? #f))
  (define note (and E (get-logbook-note E)))
  (when note (display (logbook-note-text note))))

(define (set-entry-note L project entry entry-type)
  (define E (logbook-entry L project entry entry-type #:create? #f))
  (unless E (error 'set-entry-note "Entry ~a does not exist" entry))
  (set-logbook-note! E (port->string)))

(define (get-table-note L project entry entry-type table table-type)
  (define E (logbook-entry L project entry entry-type #:create? #f))
  (when E
    (define T (logbook-table E table table-type #:create? #f))
    (define note (and T (get-logbook-note T)))
    (when note (display (logbook-note-text note)))))

(define (set-table-note L project entry entry-type table table-type)
  (define E (logbook-entry L project entry entry-type #:create? #f))
  (when E
    (define T (logbook-table E table table-type #:create? #f))
    (unless T (error 'set-table-note "Table ~a does not exist" table))
    (set-logbook-note! T (port->string))))

(define (main)
  (define jobs '())
  (define logbook-name #f)
  (define entry-name #f)
  (define plot-output 'screen)
  (define plot-columns #f)
  (define plot-title #f)
  (define entry-type #f)
  (define table-type #f)
  (define plot-size #f)
  (define font-size #f)
  (define browser #t)
  (define server-ipaddr "localhost")
  (define column-names 'first-line)
  (define-syntax-rule (push-job! f arg ...)
    (let ((args (list arg ...)))
      (set! jobs (cons (lambda (L) (apply f L args)) jobs))))
  (define positional-args
    (command-line
     #:program (short-program+command-name)
     #:once-each
     ["--logbook" logbook "sqlite file to use; default: $RACKET_LOGBOOK"
      (set! logbook-name logbook)]
     #:multi
     ["--plot-font-size" size "set plot font size" (set! font-size (string->number size))]
     ["--default-plot-font-size" "set default plot font size" (set! font-size #f)]
     ["--plot-size" WIDTHxHEIGHT "set plot size" (set! plot-size WIDTHxHEIGHT)]
     ["--default-plot-size" "set default plot size" (set! plot-size #f)]
     ["--plot-output-default-file"
      "output plot results to file named after the table"
      (set! plot-output 'default-file)]
     ["--plot-output-file" filename
      "output plot results to file"
      (set! plot-output (if (string=? filename "-") 'stdout filename))]
     ["--plot-output-screen"
      "output plot results to screen"
      (set! plot-output 'screen)]
     ["--plot-columns" column-indices "comma-separated column indices to use"
      (set! plot-columns (map string->number (string-split column-indices ",")))]
     ["--latest" "use the latest entry instead of a named one (default)" (set! entry-name #f)]
     ["--entry" entry "entry to use" (set! entry-name entry)]
     ["--entry-type" type "entry type to use" (set! entry-type type)]
     ["--default-entry-type" "use default (or unspecified) entry type" (set! entry-type #f)]

     ["--title" title "set plot title" (set! plot-title title)]
     ["--default-title" "use default plot titles" (set! plot-title #f)]

     ["--table-type" type "table type to use" (set! table-type type)]

     ["--column-names" names "comma-separated column names for loading csv"
      (set! column-names names)]
     ["--no-column-names" "specify that no column names are to be defined when loading csv"
      (set! column-names #f)]
     ["--first-line-column-names" "first line of loaded input contains column names"
      (set! column-names 'first-line)]

     ["--list-all-entries"
      "list all entries in the log book"
      (push-job! list-entries #f entry-type)]
     ["--list-project-entries" project
      "list all entries against a given project"
      (push-job! list-entries project entry-type)]
     ["--list-all-tables"
      "list all tables in every entry in every project"
      (push-job! list-all-tables entry-type table-type)]
     ["--list-project-tables" project
      "list all tables in the named project's entries"
      (push-job! list-project-tables project entry-type table-type)]
     ["--list-tables" project entry
      "list all tables in the named project entry"
      (push-job! list-tables project entry entry-type table-type)]

     ["--get-entry-note" project entry
      "print an entry note"
      (push-job! get-entry-note project entry entry-type)]
     ["--set-entry-note" project entry
      "set an entry note from stdin"
      (push-job! set-entry-note project entry entry-type)]
     ["--get-table-note" project entry table
      "print a table note"
      (push-job! get-table-note project entry entry-type table table-type)]
     ["--set-table-note" project entry table
      "set a table note from stdin"
      (push-job! set-table-note project entry entry-type table table-type)]

     ["--plot" project table
      "produce a simple plot of the named table"
      (push-job! do-plot
		 project entry-name entry-type table table-type
		 plot-title plot-columns plot-output plot-size font-size)]

     ["--dump-csv" project table
      "produce a csv dump of the named table"
      (push-job! do-dump-csv project entry-name entry-type table table-type)]
     ["--dump-sexp" project table
      "produce an s-expression dump of the named table"
      (push-job! do-dump-sexp project entry-name entry-type table table-type)]
     ["--load-csv" project entry table
      "load csv into a table"
      (push-job! do-load-csv project entry entry-type table table-type column-names)]

     ["--export-all"
      "export all entries"
      (push-job! do-export #f entry-type #f)]

     ["--export-project" project
      "export all entries in a project"
      (push-job! do-export project entry-type #f)]

     ["--export-entry" project entry
      "export a single entry"
      (push-job! do-export project entry-type entry)]

     ["--import-entries"
      "import entries from stdin"
      (push-job! do-import)]

     ["--ip" ipaddr
      "set IP address (or hostname) for --serve to listen on"
      (set! server-ipaddr ipaddr)]
     ["--serve" port
      "run webserver UI"
      (push-job! do-serve server-ipaddr (string->number port) browser)]
     ["--browser"
      "open browser when server starts (default)"
      (set! browser #t)]
     ["--no-browser"
      "do not open browser when server starts"
      (set! browser #f)]

     ["--list-prefs"
      "list all current pref settings"
      (push-job! list-prefs)]
     ["--set-pref" project entry-type table-type table-name pref-name pref-value
      "set a preference"
      (push-job! set-pref project entry-type table-type table-name pref-name pref-value)]
     ["--delete-pref" project entry-type table-type table-name pref-name
      "delete a preference"
      (push-job! delete-pref project entry-type table-type table-name pref-name)]
     ))
  (define L (open-logbook (or logbook-name (default-logbook-name))))
  (for ((j jobs)) (j L))
  )

;; (module+ test
;;   (plot-new-window? #t)
;;   (define L (open-logbook ':memory: #:verbose? #t))
;;   (define E (logbook-entry L "project1" "entry1" "etype1"))
;;   (define T (logbook-table E "table1" "ttype1"))
;;   (write-logbook-datum! T '(10 10))
;;   (write-logbook-datum! T '(20 20))
;;   (write-logbook-datum! T '(30 30))
;;   (logbook-table-plot-points T))
