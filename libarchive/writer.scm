;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive writer)
  use-module:	(oop goops)
  use-module:	(system foreign)
  use-module:	(srfi srfi-26)
  use-module:	(ice-9 match)
  use-module:	(ice-9 optargs)
  use-module:	(libarchive base)
  use-module:	(rnrs bytevectors)

  export:		(<archive-writer>
				 open-filename
				 set-format
				 add-filter
				 fail
				 write-data
				 write-header
				 )

  re-export-and-replace: (close)

  re-export:	(free
				 free-entries
				 new-entry
				 warning-handler
				 with-libarchive-warning-handler
				 libarchive-warning?
				 libarchive-error?
				 libarchive-parameter-error?))

;;;

(define-libarchive-fn	'*		archive:write-new)

(define-class <archive-writer> (<archive-base>)
  (allocator
   allocation: #:class
   init-value: archive:write-new))

(define-method (initialize (self <archive-writer>) initargs)
  (let-keywords initargs #t	((format #f)
							 (filename #f)
							 (default-ext #f)
							 (filters '()))
	(when (and (not (null? filters))
			   (not (or format filename default-ext)))
	  (archive-report-parameter-error "invalid keyword combination"
									  'initialize initargs))
	(when (and format (not (symbol? format)))
	  (archive-report-parameter-error "invalid format parameter"
									  'initialize format))
	(when (or (and filename (not (string? filename)))
			  (and default-ext (not (string? default-ext))))
	  (archive-report-parameter-error "invalid filename or ext"
									  'initialize initargs))

	(next-method)

	(cond
	 (format (set-format self format))
	 ((or filename default-ext)
	  (set-format self (or filename "") (or default-ext ""))))
	(for-each (cut add-filter self <>) filters)))

;; args should be one or two strings, or one or more symbols

(define-libarchive-fn int archive:write-set-format-filter-by-ext '* '*)
(define-libarchive-fn int archive:write-set-format-filter-by-ext-def '* '* '*)
(define-libarchive-fn int archive:write-set-format-by-name '* '*)
(define-libarchive-fn int archive:write-add-filter-by-name '* '*)

(define-method (set-format (self <archive-writer>)
						   (fn <string>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:write-set-format-filter-by-ext archive-ptr
																 (string->pointer fn)))))

(define-method (set-format (self <archive-writer>)
								  (fn <string>)
								  (def <string>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:write-set-format-filter-by-ext-def
						  archive-ptr
						  (string->pointer fn)
	  					  (string->pointer def)))))

(define-method (set-format (self <archive-writer>)
						   (fmt <symbol>)
						   . rest)
  (with-archive-ptr self
	(archive-check-error self
						 (archive:write-set-format-by-name
						  archive-ptr
						  (string->pointer (symbol->string fmt))))
	(for-each (cut add-filter self <>) rest)))

(define-method (add-filter (self <archive-writer>)
						   (filt <symbol>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:write-add-filter-by-name
						  archive-ptr
						  (string->pointer (symbol->string filt))))))

(define-libarchive-fn	int		archive:write-close '*)

(define-method (close (self <archive-writer>))
  (with-archive-ptr self
	(archive-check-error self (archive:write-close archive-ptr))))

(define-libarchive-fn	int		archive:write-fail '*)

(define-method (fail (self <archive-writer>))
  (with-archive-ptr self
	(archive:write-fail archive-ptr)))

(define-libarchive-fn	int		archive:write-open-filename '* '*)

(define-method (open-filename (self <archive-writer>)
							  (filename <string>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:write-open-filename
						  archive-ptr
						  (string->pointer filename)))))

(define-libarchive-fn	ssize_t	archive:write-data '* '* size_t)

(define-method (write-data (self <archive-writer>)
						   (data <bytevector>)
						   (offset <integer>)
						   (len <integer>))
  (with-archive-ptr self
	(archive-check-offset-len 'write-data offset len (bytevector-length data))
	(archive-check-result self (archive:write-data archive-ptr
												   (bytevector->pointer data offset)
												   len))))

(define-method (write-data (self <archive-writer>)
						   (data <bytevector>)
						   (offset <integer>))
  (write-data self data offset (- (bytevector-length data) offset)))

(define-method (write-data (self <archive-writer>)
						   (data <bytevector>))
  (write-data self data 0 (bytevector-length data)))

(define-libarchive-fn	int	archive:write-header '* '*)

(define-method (write-header (self <archive-writer>)
							 (header <archive-entry>))
  (with-archive-ptr self
	(with-entry-ptr header
	  (archive-check-error self
						   (archive:write-header archive-ptr entry-ptr)))))

;; end
