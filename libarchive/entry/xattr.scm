;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive entry xattr)
  use-module: (oop goops)
  use-module: (system foreign)
  use-module: (rnrs bytevectors)
  use-module: (srfi srfi-11)		;; let-values
  use-module: ((srfi srfi-71)
			   select: (unlist))
  use-module: (libarchive base)
  use-module: (libarchive entry)

  export: (entry-xattr-count
		   entry-xattr-reset
		   entry-xattr-next

		   entry-xattr-clear
		   entry-xattr-add-entry
		   )
  )

;;;

(define-libarchive-fn void archive:entry-xattr-clear '*)

(define-method (entry-xattr-clear (self <archive-entry>))
  (with-entry-ptr self
	(archive:entry-xattr-clear entry-ptr)))

(define-libarchive-fn void archive:entry-xattr-add-entry '* '* '* size_t)

(define-method (entry-xattr-add-entry (self <archive-entry>)
									  (key <string>)
									  (value <bytevector>))
  (with-entry-ptr self
	(archive:entry-xattr-add-entry entry-ptr
								   (string->pointer key)
								   (bytevector->pointer value)
								   (bytevector-length value))))

(define-libarchive-fn int archive:entry-xattr-count '*)

(define-method (entry-xattr-count (self <archive-entry-base>))
  (with-entry-ptr self
	(archive-check-entry-error self (archive:entry-xattr-count entry-ptr))))

(define-libarchive-fn int archive:entry-xattr-reset '*)

(define-method (entry-xattr-reset (self <archive-entry-base>))
  (with-entry-ptr self
	(archive-check-entry-error self (archive:entry-xattr-reset entry-ptr))))

(define-libarchive-fn int archive:entry-xattr-next '* '* '* '*)

(define %xattr-struct (list '* '* size_t))
(define %xattr-struct-init (list %null-pointer %null-pointer 0))

(define-method (entry-xattr-next (self <archive-entry-base>))
  (with-entry-ptr self
	(let*-values (((keyptr valptr valsize)
				   (make-out-param-struct %xattr-struct %xattr-struct-init)))
	  ;; doesn't use archive-check-entry-error, because at least some
	  ;; libarchive versions return ARCHIVE_WARN rather than ARCHIVE_EOF
	  ;; when no more xattrs exist :-(
	  (if (archive-ok? (archive:entry-xattr-next entry-ptr
												 keyptr
												 valptr
												 valsize))
		  (let-values (((rkey rval rvalsize)
						(unlist (parse-c-struct keyptr %xattr-struct))))
			(cons (pointer->string rkey)
				  (bytevector-copy (pointer->bytevector rval rvalsize))))
		  #f))))

;; end


