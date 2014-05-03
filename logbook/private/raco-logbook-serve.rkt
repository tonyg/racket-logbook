#lang racket/base

(require racket/match)
(require racket/date)
(require racket/runtime-path)
(require racket/port)
(require racket/string)
(require racket/list)
(require net/base64)
(require plot/no-gui)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/dispatch)
(require web-server/templates)
(require xml)

(require "../main.rkt")
(require "../plot-utils.rkt")

(provide serve-logbook)

(define-runtime-path web-root "htdocs")
(define-runtime-path template-root ".")

(define plot-image-width 700)
(define plot-image-height 450)

(define (page title
	      content
	      #:nav [nav '()]
	      #:page-class [page-class "default"])
  (response/output
   (lambda (p)
     (display (include-template "templates/page.html") p))))

(define (value->string v)
  (call-with-output-string (lambda (p) (display v p))))

(define (value->xexpr v)
  (cond
   [(string? v)
    `(div ((class "racket-value"))
	  ,@(apply append
		   (for/list [(line (string-split v "\n"))]
		     (list line `(br)))))]
   [else
    `(span ((class "racket-value")) ,(value->string v))]))

(define (pretty-time t)
  (date->string (seconds->date t) #t))

;; Holy fuck this is stupid.
;; https://groups.google.com/d/msg/racket-users/-yVo3542Bew/6DzplG6UA4EJ
(define-namespace-anchor dynamic-template-anchor)
(define (dynamic-template* name bindings)
  (define p (path->string (build-path template-root name)))
  (define form #`(lambda (#,@(map car bindings)) (include-template (file #,p))))
  (apply (eval form (namespace-anchor->namespace dynamic-template-anchor))
	 (map cadr bindings)))

(define-syntax-rule (dynamic-template name (var val) ...)
  (dynamic-template* name `((var ,val) ...)))

(define-syntax-rule (static-template name (var val) ...)
  (let ((var val) ...)
    (include-template name)))

(define (serve-style req)
  (response/output
   #:mime-type #"text/css"
   (lambda (p)
     (display (static-template "templates/style.css"
			       (color1 "#4B4B61")
			       (color2 "#0ac")
			       (linkcolor "#ff8827")
			       (textfonts "'Lora', Georgia")
			       (headerfonts "'Bree Serif', Georgia"))
	      p))))

(define (serve-logbook L port)
  (define-values (logbook-dispatch logbook-url)
    (dispatch-rules
     [("") list-projects]
     [("style.css") serve-style]
     [("log" (string-arg)) list-entries]
     [("log" (string-arg) (string-arg)) list-tables]
     [("log" (string-arg) (string-arg) (string-arg)) show-table]
     [("log" (string-arg) (string-arg) (string-arg)
       "plot" (integer-arg) (integer-arg) (integer-arg) ...) plot-table]
     [("log" (string-arg) (string-arg) (string-arg) "defaultplot") default-plot-table]))

  (define (list-projects req)
    (page "All Projects"
	  `(table
	    (thead
	     (tr ((class "ruled"))
		 (th "Project name")))
	    (tbody
	     ,@(for/list [(p (logbook-projects L))]
		 `(tr (td (a ((href ,(logbook-url list-entries p))) ,p))))))
	  ))

  (define (list-entries req project)
    (page (format "Project ~a" project)
	  `(div
	    (a ((class "latest-entry")
		(href ,(logbook-url list-tables project "--latest--")))
	       "Latest entry")
	    (table
	     (thead
	      (tr ((class "ruled"))
		  (th "Entry name")
		  (th "Entry type")
		  (th "Created")))
	     (tbody
	      ,@(for/list [(E (logbook-entries L #:project project))]
		  (match-define (<logbook-entry> _ _ (== project) name type created-time) E)
		  `(tr (td (a ((href ,(logbook-url list-tables project name)))
			      ,name))
		       (td ,type)
		       (td ,(pretty-time created-time)))))))
	  ))

  (define (default-plot-columns T)
    (define E (logbook-table-entry T))
    (define prefs (logbook-prefs L (logbook-entry-project E)
				 #:entry_type (logbook-entry-type E)
				 #:table_type (logbook-table-type T)
				 #:table_name (logbook-table-name T)))
    (hash-ref prefs 'default-plot-columns (lambda () '(0 1))))

  (define (format-table entry0 T)
    (match-define (<logbook-table> _ _ (<logbook-entry> _ _ project entry entry-type _)
				   name type cols created-time) T)
    (match-define (list* xaxis yaxes) (default-plot-columns T))
    `(form ((name ,(format "table-~a" name)))
	   ,@(if cols
		 `((img ((class "display-none")
			 (width ,(number->string plot-image-width))
			 (height ,(number->string plot-image-height))
			 (src "")
			 (id ,(format "plot-~a" name)))))
		 '())
	   (table
	    (thead
	     (tr ((class "ruled"))
		 (th "Label")
		 ,@(if cols
		       (for/list [(c cols)] `(th ((class "rotate")) ,(value->xexpr c)))
		       `((th "Datum"))))
	     ,@(if cols
		   `((tr (th)
			 ,@(for/list [(i (in-naturals)) (c cols)]
			     `(th (input ((type "radio")
					  ,@(if (= xaxis i)
						`((checked "checked"))
						'())
					  (id ,(format "x-~a-~a" name i))
					  (name ,(format "x-~a" name))
					  (value ,(number->string i))))))))
		   '())
	     ,@(if cols
		   `((tr ((class "ruled"))
			 (th)
			 ,@(for/list [(i (in-naturals)) (c cols)]
			     `(th (input ((type "checkbox")
					  ,@(if (member i yaxes)
						`((checked "checked"))
						'())
					  (id ,(format "y-~a-~a" name i))
					  (name ,(format "y-~a" name))
					  (value ,(number->string i))))))))
		   '())
	     ,@(if cols
		   `((script ,(format "install_callbacks('~a','~a','~a',~a);\n"
				      project
				      entry0
				      name
				      (length cols))))
		   '()))
	    (tbody
	     ,@(for/list [(R (read-logbook-data/labels T))]
		 (match-define (list label data) R)
		 `(tr (td ,label)
		      ,@(if cols
			    (for/list [(c data)] `(td ,(value->xexpr c)))
			    (list `(td ,(value->xexpr data))))))))))

  (define (lookup-entry project name)
    (if (equal? name "--latest--")
	(latest-logbook-entry L project)
	(logbook-entry L project name #:create? #f)))

  (define (list-tables req project entry0)
    (define E (lookup-entry project entry0))
    (define entry (logbook-entry-name E))
    (page (format "~a/~a" project entry)
	  `(div
	    ,@(for/list [(T (logbook-tables E))]
		(match-define (<logbook-table> _ _ _ name type cols created-time) T)
		`(div
		  (h2 ((class "table-name"))
		      (a ((href ,(logbook-url show-table project entry0 name))) ,name))
		  (h3 ((class "table-type")) ,type)
		  (h3 ((class "table-created-time")) ,(pretty-time created-time))
		  ,(format-table entry0 T))))
	  #:nav (list (list (logbook-url list-entries project) project)
		      (list (logbook-url list-tables project "--latest--") "latest entry"))))

  (define (show-table req project entry0 table)
    (define E (lookup-entry project entry0))
    (define entry (logbook-entry-name E))
    (define T (logbook-table E table #:create? #f))
    (match-define (<logbook-table> _ _ _ name type cols created-time) T)
    (page (format "~a/~a/~a" project entry table)
	  `(div
	    (h2 ((class "table-type")) ,type)
	    (h2 ((class "table-created-time")) ,(pretty-time created-time))
	    ,(format-table entry0 T))
	  #:nav (list (list (logbook-url list-entries project) project)
		      (list (logbook-url list-tables project "--latest--") "latest entry")
		      (list (logbook-url list-tables project entry) entry)
		      (list (logbook-url show-table project "--latest--" table)
			    (format "latest ~a" table)))))

  (define (default-plot-table req project entry0 table)
    (define E (lookup-entry project entry0))
    (define entry (logbook-entry-name E))
    (define T (logbook-table E table #:create? #f))
    (match-define (list* xaxis yaxes) (default-plot-columns T))
    (plot-table* E T xaxis yaxes))

  (define (plot-table req project entry0 table xaxis yaxis0 yaxesN)
    (define E (lookup-entry project entry0))
    (define entry (logbook-entry-name E))
    (define T (logbook-table E table #:create? #f))
    (define yaxes (cons yaxis0 yaxesN))
    (plot-table* E T xaxis yaxes))

  (define (plot-table* E T xaxis yaxes)
    (define-values (x-label y-label)
      (match (logbook-table-column-spec T)
	[#f (values (plot-x-label) (plot-y-label))]
	[ss
	 (values (format "~a" ((@ xaxis) ss))
		 (string-join (map (lambda (y) (value->string ((@ y) ss))) yaxes) " / "))]))

    (set-logbook-pref! L (logbook-entry-project E)
		       #:entry_type (logbook-entry-type E)
		       #:table_type (logbook-table-type T)
		       #:table_name (logbook-table-name T)
		       'default-plot-columns (cons xaxis yaxes))

    (response/output
     #:mime-type #"image/png"
     (lambda (p)
       (parameterize ((plot-width plot-image-width)
		      (plot-height plot-image-height))
	 (plot-file (for/list [(yaxis yaxes)]
		      (define ps (logbook-table->points T #:columns (list xaxis yaxis)))
		      (list (lines ps) (points ps)))
		    p
		    'png
		    #:x-label x-label #:y-label y-label)))))

  (serve/servlet logbook-dispatch
		 ;; #:command-line? #f
		 #:port port
		 #:server-root-path web-root
		 #:extra-files-paths (list web-root)
		 #:servlet-path "/"
		 #:servlet-regexp #px""
		 ;; #:server-root-path server-root-path
		 ;; #:extra-files-paths extra-files-paths
		 ))
