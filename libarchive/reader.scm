;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive reader)
  use-module:	(oop goops)
  use-module:	(system foreign)
  use-module:	(libarchive base)
  use-module:	(rnrs bytevectors)

  export:		(<archive-reader>
				 open-filename
				 next-header
				 read-data
				 )

  re-export-and-replace:	(close)

  re-export:	(free
				 free-entries
				 new-entry
				 warning-handler
				 with-libarchive-warning-handler
				 libarchive-warning?
				 libarchive-error?
				 libarchive-parameter-error?
				 ))

;;;

(define-libarchive-fn	'*		archive:read-new)

(define-class <archive-reader> (<archive-base>
								<archive-entry-base>)
  (allocator
   allocation: #:class
   init-value: archive:read-new)
  (current-header-addr
   init-form: (make-c-struct '(*) (list %null-pointer))
   getter: current-header-addr))

(define-libarchive-fn	int		archive:read-support-filter-all '*)
(define-libarchive-fn	int		archive:read-support-format-all '*)

(define-method (initialize (self <archive-reader>) initargs)
  (next-method)
  (slot-set! self 'owner-slot self)
  ;; todo: add a keyword to turn off universal support?
  (with-archive-ptr self
	(archive-check-error self (archive:read-support-format-all archive-ptr))
	(archive-check-error self (archive:read-support-filter-all archive-ptr))))

(define-method (free (self <archive-reader>))
  (slot-set! self 'entry-slot %null-pointer)
  (next-method))

(define-libarchive-fn	int		archive:read-open-filename '* '* size_t)

(define-method (open-filename (self <archive-reader>) (filename <string>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-open-filename archive-ptr
													 (string->pointer filename)
													 131072))))

(define-libarchive-fn	int		archive:read-next-header '* '*)

(define-method (next-header (self <archive-reader>))
  (with-archive-ptr self
	(let* ((addr (current-header-addr self))
		   (ret (archive:read-next-header archive-ptr addr)))
	  (slot-set! self 'entry-slot (if (archive-ok? ret)
									  (dereference-pointer addr)
									  %null-pointer))
      (archive-ok? (archive-check-error self ret)))))

(define-libarchive-fn	ssize_t	archive:read-data '* '* size_t)

(define-method (read-data (self <archive-reader>)
						  (buf <bytevector>)
						  (offset <integer>)
						  (len <integer>))
  (with-archive-ptr self
	(archive-check-offset-len 'read-data offset len (bytevector-length buf))
	(archive-check-result self (archive:read-data archive-ptr
												  (bytevector->pointer buf offset)
												  len))))

(define-method (read-data (self <archive-reader>)
						  (buf <bytevector>)
						  (offset <integer>))
  (read-data self buf offset (- (bytevector-length buf) offset)))

(define-method (read-data (self <archive-reader>)
						  (buf <bytevector>))
  (read-data self buf 0 (bytevector-length buf)))

(define-libarchive-fn	int		archive:read-close '*)

(define-method (close (self <archive-reader>))
  (with-archive-ptr self
	(archive-check-error self (archive:read-close archive-ptr))))

;; end
