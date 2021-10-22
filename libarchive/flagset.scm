;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive flagset)
  use-module: (oop goops)
  use-module: (srfi srfi-1)
  use-module: (srfi srfi-26)
  use-module: (ice-9 optargs)

  export: (<flagset-base>
		   universe
		   subsets
		   flag-lookup
		   subset-lookup
		   flaglist->number
		   as-flags
		   as-number
		   contains?
		   contains-all?
		   contains-any?
		   contains-any-of-subsets
		   ))

;;

(define (flaglist->number flags universe)
  (fold (lambda (e s) (logior s
							  (if (exact-integer? e)
								  e
								  (assq-ref universe e))))
		0
		flags))

;;

(define-class <flagset-base-class> (<class>))

;; this is to validate and fixup the UNIVERSE value

(define-method (initialize (class <flagset-base-class>) initargs)
  (let-keywords initargs #t ((universe '())
							 (subsets '()))
	(next-method)
	;; universe should be either a list of symbols or an alist
	;; of (symbol . mask). If the former, we generate our own
	;; mask values.
	(unless (and (list? universe)
				 (or (every symbol? universe)
					 (every (lambda (p)
							  (and (pair? p)
								   (symbol? (car p))
								   (exact-integer? (cdr p))))
							universe)))
	  (error "Invalid universe for <flagset-base>"))
	(let ((aval (if (or (null? universe) (pair? (car universe)))
					universe
					(map (lambda (s v) (cons s (ash 1 v)))
						 universe
						 (iota (length universe))))))
	  (class-slot-set! class 'universe aval)
	  ;; subsets should be either empty or an alist of either
	  ;; (symbol . mask) or (symbol flag flag...)
	  ;; subset symbols should not appear in the universe
	  ;; We normalize to (symbol . mask) format, and prepend
	  ;; to the universe
	  (unless (and (list? subsets)
				   (every (lambda (p)
							(and (pair? p)
								 (and (symbol? (car p))
									  (not (assq (car p) aval)))
								 (or (exact-integer? (cdr p))
									 (every (cut assq <> aval) (cdr p)))))
						  subsets))
		(error "Invalid subsets for <flagset-base>"))
	  (let ((sval (fold (lambda (p s)
						  (cons (if (pair? (cdr p))
									(cons (car p) (flaglist->number (cdr p) aval))
									(cons (car p) (cdr p)))
								s))
						aval
						subsets)))
		(class-slot-set! class 'subsets sval)))))

(define-method (universe (class <flagset-base-class>))
  (class-slot-ref class 'universe))

(define-method (subsets (class <flagset-base-class>))
  (class-slot-ref class 'subsets))

(define-method (flag-lookup (class <flagset-base-class>) (flag <symbol>))
  (assq-ref (class-slot-ref class 'universe) flag))

(define-method (flag-lookup (class <flagset-base-class>) (flag <integer>))
  (cond ((find (lambda (p) (= (cdr p) flag)) (class-slot-ref class 'universe))
		 => car)))

(define-method (subset-lookup (class <flagset-base-class>) (flag <symbol>))
  (assq-ref (class-slot-ref class 'subsets) flag))

(define-class <flagset-base> (<object>)
  (universe		init-value: '()
				getter: universe
				allocation: #:each-subclass)
  (subsets		init-value: '()
				getter: subsets
				allocation: #:each-subclass)
  (value		init-value: 0
				init-keyword: #:value
				getter: as-number)
  metaclass: <flagset-base-class>)


(define-method (initialize (self <flagset-base>) initargs)
  (let-keywords initargs #t ((flags '()))
	(next-method)
	;; subsets rather than universe, so that subset symbols can
	;; be used for initialization
	(let ((universe (subsets self)))
	  (when (null? universe)
		(error "can't create instances of an abstract flagset"))
	  (let ((flagval (flaglist->number flags universe)))
		(slot-set! self 'value (logior (as-number self) flagval))))))

(define-method (as-flags (self <flagset-base>))
  (let ((value (as-number self)))
	(fold (lambda (e s) (if (= (logand (cdr e) value) (cdr e))
							(cons (car e) s)
							s))
		  '()
		  (universe self))))

(define-method (contains? (self <flagset-base>) (flag <symbol>))
  (let ((flagval (assq-ref (universe self) flag)))
	(= (logand flagval (as-number self)) flagval)))

(define-method (contains-any? (self <flagset-base>) (flag <symbol>))
  (cond
   ((assq-ref (universe self) flag) => (lambda (f) (= (logand f (as-number self) f))))
   ((assq-ref (subsets self) flag) => (lambda (f) (logtest (as-number self) f)))
   (else (error "bad flag"))))

(define-method (contains? (self <flagset-base>) (flagval <integer>))
  (= (logand flagval (as-number self)) flagval))

(define-method (contains-all? (self <flagset-base>) . flags)
  (let ((flagval (flaglist->number flags (universe self))))
	(= (logand flagval (as-number self)) flagval)))

(define-method (contains-any? (self <flagset-base>) . flags)
  (any (lambda (f) (contains-any? self f)) flags))

(define-method (contains-any-of-subsets (self <flagset-base>))

  (define (fold-range proc init from to)
	(cond ((null? from) init)
		  ((eq? from to) init)
		  (else (fold-range proc (proc (car from) init) (cdr from) to))))

  (let ((value (as-number self))
		(universe (universe self))
		(subsets (subsets self)))
	(fold-range (lambda (e s) (if (logtest (cdr e) value)
								  (cons (car e) s)
								  s))
				'()
				subsets
				universe)))

;; end
