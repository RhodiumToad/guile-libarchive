;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive base)
  use-module:	(oop goops)
  use-module:	(ice-9 exceptions)
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

				 archive-report-error archive-report-parameter-error

				 <archive-base>
				 report-error free-entries new-entry

				 <archive-entry-base>
				 get-entry-ptr
				 clone

				 <archive-entry>
				 free   ;; also for <archive-base>
				 ))

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

;; report an error ERROR from ORIGIN without associating it with any
;; specific object

(define (archive-report-error str origin)
  (raise-exception
   (make-exception
	(make-error)
	(make-exception-with-origin origin)
	(make-exception-with-message str))))

;; report an error with the parameters to ORIGIN

(define (archive-report-parameter-error str origin . irritants)
  (raise-exception
   (make-exception
	(make-programming-error)
	(make-exception-with-origin origin)
	(make-exception-with-irritants irritants)
	(make-exception-with-message str))))

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
					   (pointer->string (archive:error-string archive-ptr)))))
	  (raise-exception
	   (make-exception
		(if (archive-fatal? retval)
			(make-external-error)
			(make-warning))
		(make-exception-with-irritants (list self))
		(make-exception-with-message errstr))
	   continuable?: (not (archive-fatal? retval))))))

;; end
