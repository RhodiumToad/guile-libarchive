;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive base)
  use-module:	(oop goops)
  use-module:	(ice-9 exceptions)
  use-module:	(ice-9 optargs)		;; let-keywords
  use-module:	(srfi srfi-1)		;; list lib
  use-module:	(srfi srfi-11)		;; let-values
  use-module:	(srfi srfi-26)		;; cut
  use-module:	((srfi srfi-71)
				 select: (unlist))
  use-module:	(system foreign)
  use-module:	((system foreign-object)
				 select: (make-foreign-object-type))

  replace:		(close)

  export:		(;; macros
				 assert
				 archive-ptr
				 entry-ptr
				 with-archive-ptr
				 with-entry-ptr
				 define-libarchive-fn

				 ;; non-method functions
				 archive-eof? archive-ok? archive-error?
				 archive-fatal? archive-retry? archive-warn?

				 archive-check-error archive-check-result
				 archive-check-object archive-check-offset-len
				 archive-check-entry-error

				 archive-report-error archive-report-parameter-error
				 archive-report-parameter-warning

				 with-libarchive-warning-handler

				 ;; error types and predicates
				 libarchive-warning?
				 libarchive-error?
				 libarchive-parameter-error?

				 ;; some utilities for FFI interfacing
				 make-out-param-struct
				 pointer->maybe-string
				 maybe-string->pointer
				 convert-null-string

				 <archive-base>
				 report-error free-entries new-entry
				 warning-handler

				 <archive-entry-base>
				 get-entry-ptr
				 report-entry-error
				 clone
				 owner

				 <archive-entry>
				 free   ;; also for <archive-base>

				 <archive-entry-linkresolver>
				 set-strategy
				 linkify
				 ))

;;; commonly needed type conversions

(define-inlinable (pointer->maybe-string ptr)
  (if (null-pointer? ptr)
	  #f
	  (pointer->string ptr)))

(define-inlinable (maybe-string->pointer str)
  (if str
	  (string->pointer str)
	  %null-pointer))

(define-inlinable (convert-null-string str)
  (if (not str)
	  %null-pointer
	  (archive-report-parameter-error "invalid string value"
									  'convert-null-string
									  str)))

;;; Error handling

(define (assert-fail e)
  (raise-exception
   (make-exception
	(make-assertion-failure)
	(make-exception-with-irritants (list e))
	(make-exception-with-message (format #f "Assertion failed: ~A" e)))))

(define-syntax-rule (assert e)
  (unless e (assert-fail (quote e))))

(define-inlinable (archive-eof? x)		(= x 1))
(define-inlinable (archive-ok? x)		(zero? x))
(define-inlinable (archive-error? x)	(negative? x))
(define-inlinable (archive-fatal? x)	(<= x -30))
(define-inlinable (archive-retry? x)	(= x -10))
(define-inlinable (archive-warn? x)		(= x -20))
(define-inlinable (archive-failed? x)	(= x -25))

;; here RETVAL is an integer return code:
;;  1 (eof), 0 (success), -x (retry/warn/fatal)
;; note that report-error can return if a warn or retry
;; is continued

(define-inlinable (archive-check-error self retval)
  (when (archive-error? retval)
	(report-error self retval))
  retval)

(define-inlinable (archive-check-entry-error self retval)
  (when (archive-error? retval)
	(report-entry-error self retval))
  retval)

;; here RETVAL is an integer result, such as count of returned bytes,
;; with negative values meaning a fatal error

(define-inlinable (archive-check-result self retval)
  (when (negative? retval)
	(report-error self -30))
  retval)

;; Check the state of an object, where PRED is a predicate that
;; indicates that the object is in a deallocated or terminated state
;; (such as after an explicit free or a free of an owning object).

(define-inlinable (archive-check-object self pred)
  (when pred
	(archive-report-parameter-error "access to deallocated object"
									'archive-check-object self)))

;; error types

(define-exception-type &libarchive-warning &warning
  make-libarchive-warning
  libarchive-warning?)

(define-exception-type &libarchive-error &external-error
  make-libarchive-error
  libarchive-error?)

(define-exception-type &libarchive-parameter-error &programming-error
  make-libarchive-parameter-error
  libarchive-parameter-error?)

(define (with-libarchive-warning-handler handler thunk)
  (with-exception-handler (lambda (e)
							(if (libarchive-warning? e)
								(handler e)
								(raise-exception e continuable?: #t)))
						  thunk))

;; report an error ERROR from ORIGIN without associating it with any
;; specific object

(define (archive-report-error str origin)
  (raise-exception
   (make-exception
	(make-libarchive-error)
	(make-exception-with-origin origin)
	(make-exception-with-message str))))

;; report an error with the parameters to ORIGIN

(define (archive-report-parameter-error str origin . irritants)
  (raise-exception
   (make-exception
	(make-libarchive-parameter-error)
	(make-exception-with-origin origin)
	(make-exception-with-irritants irritants)
	(make-exception-with-message str))))

(define (archive-report-parameter-warning str origin . irritants)
  (raise-exception
   (make-exception
	(make-libarchive-warning)
	(make-exception-with-origin origin)
	(make-exception-with-irritants irritants)
	(make-exception-with-message str))
   continuable?: #t))

;; check that OFFSET/LEN fall within bounds of a buffer of length
;; BUFLEN

(define-inlinable (archive-check-offset-len origin offset len buflen)
  (when (or (negative? offset)
			(negative? len)
			(> (+ offset len) buflen))
	(archive-report-parameter-error "invalid offset/len"
									origin
									offset
									len
									buflen)))

;; error checking macros for locally binding entry-ptr and archive-ptr

(define-syntax-parameter entry-ptr
  (lambda (s)
	(syntax-violation 'entry-ptr "entry-ptr used unbound" s)))

(define-syntax-rule (with-entry-ptr self body ...)
  (let ([%entry-ptr (get-entry-ptr self)])
	(syntax-parameterize ((entry-ptr (identifier-syntax %entry-ptr)))
	  (archive-check-object self (null-pointer? entry-ptr))
	  body ...)))

(define-syntax-parameter archive-ptr
  (lambda (s)
	(syntax-violation 'entry-ptr "archive-ptr used unbound" s)))

(define-syntax-rule (with-archive-ptr self body ...)
  (let ([%archive-ptr (libarchive-ptr self)])
	(syntax-parameterize ((archive-ptr (identifier-syntax %archive-ptr)))
	  (archive-check-object self (null-pointer? archive-ptr))
	  body ...)))

;;; Basic dynamic loading

;; no point trying to defer this since we look all the functions up
;; immediately

(define resolve-libarchive-function
  (let ((dylib			(dynamic-link "libarchive"))
		(name-map-chars	(char-set #\: #\-)))
	(lambda (rettype name . argtypes)
	  (define (fix-char c)
		(if (char-set-contains? name-map-chars c) #\_ c))
	  (let* ((str (symbol->string name))
			 (fn (dynamic-func (string-map fix-char str)
							   dylib)))
		(pointer->procedure rettype fn argtypes)))))

(define-syntax-rule (define-libarchive-fn rettype name argtypes ...)
  (define name (resolve-libarchive-function rettype 'name argtypes ...)))

(define-libarchive-fn int archive:version-number)

(let ((ver (archive:version-number)))
  (unless (> 4000000 ver 3003999)
	(error "incorrect libarchive version" ver)))

;;; Memory management

(define (make-wrapped-finalizer finalfn)
  (lambda (self)
	(let ((addr (struct-ref/unboxed self 0)))
	  (struct-set!/unboxed self 0 0)
	  (unless (= addr 0)
		(finalfn (make-pointer addr))))))

(define-generic free)
(define-generic disown)
(define-generic reown)

(define-syntax-rule (define-foreign-object-address-type name finalfn)
  (begin
	(define name
	  (let* ((fnz (make-wrapped-finalizer finalfn))
			 (type (make-foreign-object-type 'name
											 '(address)
											 finalizer: fnz)))
		(add-method! free (make <method>
							specializers: (list type)
							formals: '(self)
							procedure: fnz))
		(add-method! disown (make <method>
							  specializers: (list type)
							  formals: '(self)
							  procedure: (lambda (self)
										   (let ((oaddr (struct-ref/unboxed self 0)))
											 (struct-set!/unboxed self 0 0)
											 oaddr))))
		(add-method! reown (make <method>
							 specializers: (list type <integer>)
							 formals: '(self)
							 procedure: (lambda (self addr) (struct-set!/unboxed self 0 addr))))
		type))))

;;;
;; <archive-entry> represents an entry in an archive; they point to
;; the archive they belong to, for getting charset translation info,
;; so we have to be careful about making sure they are invalidated if
;; the owning archive is explicitly freed.
;;
;; <archive-entry-base> is an immutable object that we don't own, i.e.
;; as returned by next-header when reading. We allow cloning it, which
;; requires tracking the owner, but we don't store it in the owner's
;; entries hash or do any memory alloc/free.

;; an instance of <archive-entry-base> doesn't have free-able storage,
;; it belongs to the archive reader that returned it (and is recycled
;; on each use). We track the owner so that if we clone it, the clone
;; is still linked to the same owner (the library keeps an internal
;; pointer, so we have to respect lifetimes)

(define-class <archive-entry-base> (<object>)
  (entry-slot	init-keyword: #:entry-ptr
				init-value: %null-pointer
				getter: get-entry-ptr)
  (owner-slot	init-keyword: #:owner
				init-value: #f
				getter: owner))

;; an instance of <archive-entry> can be freed, but also must be
;; explicitly freed when we free the owning archive, otherwise we
;; leave a dangling pointer inside the library. We create an object
;; to handle freeing on garbage collection.

(define-libarchive-fn	void	archive:entry-free '*)

(define-foreign-object-address-type <archive-entry-address>
  archive:entry-free)

(define-class <archive-entry> (<archive-entry-base>)
  (entry-address))

(define-libarchive-fn	'*		archive:entry-new)
(define-libarchive-fn	'*		archive:entry-new2 '*)

(define-method (initialize (self <archive-entry>) initargs)
  (next-method)
  (let* ((owner (owner self))
		 (inptr (get-entry-ptr self))
		 (ptr (if (null-pointer? inptr)
				  (if owner
					  (with-archive-ptr owner
						(archive:entry-new2 archive-ptr))
					  (archive:entry-new))
				  inptr)))
	(when (null-pointer? ptr)
	  (archive-report-error "error" 'archive:entry-new))
  	(slot-set! self 'entry-slot ptr)
	(slot-set! self 'entry-address
			   (make <archive-entry-address>
				 address: (pointer-address ptr)))
	(when owner
	  (hashq-set! (owned-entries owner) self #t))))

(define-method (free (self <archive-entry>))
  (let ((ptr (get-entry-ptr self))
		(owner (owner self))
		(addr (slot-ref self 'entry-address)))
	(unless (null-pointer? ptr)
	  (when owner
		(hashq-remove! (owned-entries owner) self))
	  (slot-set! self 'entry-slot %null-pointer)
	  (slot-set! self 'owner-slot #f)
      (free addr))))

(define-method (disown (self <archive-entry>))
  (disown (slot-ref self 'entry-address)))

;; If we were explicitly freed while disowned, we have a problem; we
;; need to take back ownership to avoid leaking memory, but the entry
;; might have a dangling pointer to the archive that formerly owned
;; it. The safest bet is to do the explicit free that was avoided due
;; to the disown.

(define-method (reown (self <archive-entry>) (addr <integer>))
  (let* ((entry-ptr (get-entry-ptr self))
		 (entry-addr (pointer-address entry-ptr)))
	(assert (or (null-pointer? entry-ptr) (= addr entry-addr)))
	(cond
	 ((null-pointer? entry-ptr)
	  (reown (slot-ref self 'entry-address) addr)
	  (free (slot-ref self 'entry-address)))
	 (else
	  (reown (slot-ref self 'entry-address) entry-addr)))))

(define-libarchive-fn	'*		archive:entry-clone '*)

(define-method (clone (self <archive-entry-base>))
  (with-entry-ptr self
	(let ((ptr (archive:entry-clone entry-ptr)))
	  (when (null-pointer? ptr)
		(archive-report-error "error" 'archive:entry-clone))
	  (make <archive-entry> entry-ptr: ptr owner: (owner self)))))

;;;
;; <archive-base> is the common code between readers and writers

(define-libarchive-fn	int		archive:free '*)

(define-foreign-object-address-type <archive-address>
  archive:free)

(define-class <archive-base> (<object>)
  (libarchive-address)
  (libarchive-ptr	init-value: %null-pointer
					getter: libarchive-ptr)
  (warning-handler	init-value: #f
					init-keyword: #:warning-handler
					accessor: warning-handler)
  (owned-entries	init-form: (make-weak-key-hash-table)
					getter: owned-entries))

;; note that the 'allocator slot exists only in derived classes

(define-method (initialize (self <archive-base>) initargs)
  (next-method)
  (let* ((allocfn (slot-ref self 'allocator))
		 (ptr (allocfn)))
	(when (null-pointer? ptr)
      (archive-report-error "error" 'initialize))
	(slot-set! self 'libarchive-ptr ptr)
	(slot-set! self 'libarchive-address (make <archive-address>
										  address: (pointer-address ptr)))))

(define-method (free-entries (self <archive-base>))
  (let ((entries (hash-map->list (lambda (k v) k) (owned-entries self))))
	(for-each free entries)))

(define-method (free (self <archive-base>))
  (free-entries self)
  (let ((ptr (libarchive-ptr self))
		(addr (slot-ref self 'libarchive-address)))
	(when (and (pointer? ptr) (not (null-pointer? ptr)))
	  (slot-set! self 'libarchive-ptr %null-pointer)
	  (free addr))))

(define-method (new-entry (self <archive-base>))
  (make <archive-entry> owner: self))

(define-libarchive-fn int archive:format '*)

(define-method (format-as-number (self <archive-base>))
  (with-archive-ptr self
	(archive:format archive-ptr)))

;; close not defined here, since it varies between derived classes,
;; but we centralize the generic creation here

(define close (ensure-generic (@@ (guile) close) 'close))

;; error reporting on a known object, for which we may be able to
;; get details from the library

(define-libarchive-fn	'*		archive:error-string '*)

(define-method (report-error (self <archive-base>)
							 (retval <integer>))
  (when (archive-error? retval)
	(let* ((archive-ptr (libarchive-ptr self))
		   (errstr (if (null-pointer? archive-ptr)
					   "error on deallocated archive object"
					   (pointer->maybe-string (archive:error-string archive-ptr))))
		   (cont (and (not (archive-fatal? retval))
					  (not (null-pointer? archive-ptr))))
		   (handler (and cont (warning-handler self)))
		   (exc (make-exception
				 (if cont
					 (make-libarchive-warning)
					 (make-libarchive-error))
				 (make-exception-with-irritants (list self))
				 (make-exception-with-message (or errstr "(null)")))))
	  (if (procedure? handler)
		  (handler self retval exc)
		  (raise-exception exc continuable?: cont)))))

;; unfortunately the entry-* functions report errors without
;; any way to get an error message.

(define-method (report-entry-error (self <archive-entry-base>)
								   (retval <integer>))
  (when (archive-error? retval)
	(let* ((entry-ptr (get-entry-ptr self))
		   (cont (and (not (archive-fatal? retval))
					  (not (null-pointer? entry-ptr))))
		   (exc (make-exception
				 (if cont
					 (make-libarchive-warning)
					 (make-libarchive-error))
				 (make-exception-with-irritants (list self))
				 (make-exception-with-message (if cont
												  "warning on archive_entry_*"
												  "error on archive_entry_*")))))
	  (raise-exception exc continuable?: cont))))

;;; memory support for multiple out params

;; the basic idea here is to make a struct containing fields for all
;; the desired parameters, and then return a pointer to each
;; individual element. Then we pass those pointers to the function
;; call, and use parse-c-struct to read out the values it stored in
;; the elements. Note that the pointer to the first element is always
;; the same thing as the pointer to the whole struct.

(define-inlinable (align off alignment)
  (1+ (logior (1- off) (1- alignment))))

(define (make-out-param-struct types vals)
  (assert (pair? types))
  (let* ((struct (make-c-struct types vals))
		 (addr (pointer-address struct)))
	(let loop ((offset 0)
			   (ptrs '())
			   (types types))
	  (if (null? types)
		  (unlist (reverse ptrs))
		  (let* ((type (car types))
				 (newpos (align offset (alignof type))))
			(loop (+ newpos (sizeof type))
				  (cons (make-pointer (+ addr newpos)) ptrs)
				  (cdr types)))))))

;;; linkresolver

;; this is in this file mostly to avoid needing to export the memory
;; management functions

(define-libarchive-fn	'*		archive:entry-linkresolver-new)
(define-libarchive-fn	void	archive:entry-linkresolver-set-strategy '* int)
(define-libarchive-fn	void	archive:entry-linkresolver-free '*)

(define-foreign-object-address-type <archive-entry-linkresolver-address>
  archive:entry-linkresolver-free)

(define-class <archive-entry-linkresolver> (<object>)
  (linkresolver-address)
  (linkresolver-slot	init-value: %null-pointer
						getter: get-linkresolver-ptr)
  (owned-entries		init-form: (make-hash-table)
						getter: owned-entries))

(define-method (initialize (self <archive-entry-linkresolver>) initargs)
  (let-keywords initargs #t ((strategy #f))
	(next-method)
	(let* ((ptr (archive:entry-linkresolver-new)))
	  (when (null-pointer? ptr)
		(archive-report-error "error" 'archive:entry-linkresolver-new))
	  (slot-set! self 'linkresolver-slot ptr)
	  (slot-set! self 'linkresolver-address
				 (make <archive-entry-linkresolver-address>
				   address: (pointer-address ptr)))
	  (when strategy
		(let ((fmt (if (integer? strategy)
					   strategy
					   (format-as-number strategy))))
		  (archive:entry-linkresolver-set-strategy ptr fmt))))))

(define-method (set-strategy (self <archive-entry-linkresolver>)
							 (strategy <integer>))
  (let ((ptr (get-linkresolver-ptr self)))
	(archive-check-object self (null-pointer? ptr))
  	(archive:entry-linkresolver-set-strategy ptr strategy)))

(define-method (set-strategy (self <archive-entry-linkresolver>)
							 (strategy <archive-base>))
  (set-strategy self (format-as-number strategy)))

(define-method (free (self <archive-entry-linkresolver>))
  (let ((ptr (get-linkresolver-ptr self))
		(addr (slot-ref self 'linkresolver-address)))
	(let ((entries (hash-map->list (lambda (k v) k) (owned-entries self))))
	  (for-each free entries))
	(hash-clear! (owned-entries self))
	(unless (null-pointer? ptr)
	  (slot-set! self 'linkresolver-slot %null-pointer)
      (free addr))))

(define-method (entry-internalize (self <archive-entry-linkresolver>)
								  (entry <archive-entry>))
  (let ((addr (disown entry)))
	(hashv-set! (owned-entries self) addr entry)))

(define-method (entry-externalize-fold (self <archive-entry-linkresolver>)
									   candidate-entry
									   candidate-ptr
									   entry-ptr
									   seed)
  (cond
   ((null-pointer? entry-ptr) seed)
   ((equal? entry-ptr candidate-ptr)
	(cons candidate-entry seed))
   (else
	(let* ((entries (owned-entries self))
		   (addr (pointer-address entry-ptr))
		   (entry (hashv-ref entries addr)))
	  (assert entry)
	  (reown entry addr)
	  (hashv-remove! entries addr)
	  (cons entry seed)))))

(define-libarchive-fn	void	archive:entry-linkify '* '* '*)

(define %linkify-struct (list '* '*))

(define-method (linkify (self <archive-entry-linkresolver>)
						(entry <archive-entry>))
  (let ((linkresolver-ptr (get-linkresolver-ptr self)))
  	(archive-check-object self (null-pointer? linkresolver-ptr))
	(with-entry-ptr entry
	  (let*-values (((entry1 entry2)
					 (make-out-param-struct %linkify-struct (list entry-ptr %null-pointer))))
		(archive:entry-linkify linkresolver-ptr entry1 entry2)
		(let ((results (parse-c-struct entry1 %linkify-struct)))
		  (unless (member entry-ptr results)
			(entry-internalize self entry))
		  (fold-right (cut entry-externalize-fold self entry entry-ptr <> <>)
					  '()
					  results))))))

(define-method (linkify (self <archive-entry-linkresolver>))
  (let ((linkresolver-ptr (get-linkresolver-ptr self)))
  	(archive-check-object self (null-pointer? linkresolver-ptr))
	(let*-values (((entry1 entry2)
				   (make-out-param-struct %linkify-struct (list %null-pointer %null-pointer))))
	  (archive:entry-linkify linkresolver-ptr entry1 entry2)
	  (let ((results (parse-c-struct entry1 %linkify-struct)))
		(fold-right (cut entry-externalize-fold self #f #f <> <>)
					'()
					results)))))

;; end
