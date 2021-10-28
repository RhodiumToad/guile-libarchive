;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive entry macos)
  use-module: (oop goops)
  use-module: (system foreign)
  use-module: (rnrs bytevectors)
  use-module: (srfi srfi-11)		;; let-values
  use-module: (libarchive base)
  use-module: (libarchive entry)

  export: (entry-mac-metadata
		   entry-set-mac-metadata)
  )

;;;

(define-libarchive-fn '* archive:entry-mac-metadata '* '*)
(define-libarchive-fn void archive:entry-copy-mac-metadata '* '* size_t)

(define %mac-metadata-struct (list size_t))

(define-method (entry-mac-metadata (self <archive-entry-base>))
  (with-entry-ptr self
	(let* ((sizeptr (make-out-param-struct %mac-metadata-struct '(0)))
		   (ptr (archive:entry-mac-metadata entry-ptr sizeptr))
		   (size (car (parse-c-struct sizeptr %mac-metadata-struct))))
	  (if (or (null-pointer? ptr) (= size 0))
		  #f
		  (bytevector-copy (pointer->bytevector ptr size))))))

(define-method (entry-set-mac-metadata (self <archive-entry-base>) (data <bytevector>))
  (with-entry-ptr self
	(archive:entry-copy-mac-metadata entry-ptr
									 (bytevector->pointer data)
									 (bytevector-length data))))

;; end
