#lang racket/base

(require (only-in racket/system system))
(require (only-in racket/port with-output-to-string))
(require (only-in racket/file file->string))
(require (only-in racket/string string-trim))

(require "store.rkt")

(provide logbook-record-machine-info!)

(define (capture-system cmd)
  (string-trim (with-output-to-string (lambda () (system cmd)))))

(define (logbook-record-machine-info! entry)
  (define T (logbook-table entry "machine-info" "machine-info"))
  (for [(system-type-mode (in-list '(os word gc link machine so-suffix so-mode fs-change)))]
    (write-logbook-datum! T #:label (symbol->string system-type-mode)
			  (system-type system-type-mode)))
  (when (file-exists? "/proc/cpuinfo")
    (write-logbook-datum! T #:label "cpuinfo" (file->string "/proc/cpuinfo")))
  (case (system-type)
    [(unix)
     (write-logbook-datum! T #:label "vmstat" (capture-system "vmstat 2>&1"))]
    [(macosx)
     (write-logbook-datum! T #:label "vm_stat" (capture-system "vm_stat"))
     (write-logbook-datum! T #:label "sysctl" (capture-system "sysctl -a"))]))

(module+ test
  (require racket/pretty)
  (require racket/list)
  (define L (open-logbook ':memory:))
  (define E (logbook-entry L "p" "e" "t"))
  (logbook-record-machine-info! E)
  (for ((T (logbook-tables E)))
    (for-each pretty-print (read-logbook-data/labels T))))
