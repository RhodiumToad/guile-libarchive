;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive disk read)
  use-module:	(oop goops)
  use-module:	(system foreign)
  use-module:	(libarchive base)
  use-module:	(libarchive reader)
  use-module:	(libarchive flagset)
  use-module:	(rnrs bytevectors)

  export:		(<archive-disk-reader>
				 <archive-disk-read-behaviors>
				 set-behavior
				 set-symlink
				 user-name
				 group-name
				 set-standard-lookup
				 can-descend
				 descend
				 metadata-filter
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
				 <archive-entry-linkresolver>
				 set-strategy
				 linkify
				 ;; from reader
				 open-filename
				 next-header
				 read-data
				 ;; from flagset
				 universe
				 flag-lookup
				 subset-lookup
				 as-flags
				 as-number
				 contains?
				 contains-all?
				 contains-any?
				 contains-any-of-subsets
				 ))

;;;

(define-class <archive-disk-read-behaviors> (<flagset-base>)
  universe: '( ( restore-atime		. #x0001 )
			   ( honor-nodump		. #x0002 )
			   ( mac-copyfile		. #x0004 )
			   ( no-traverse-mounts	. #x0008 )
			   ( no-xattr			. #x0010 )
			   ( no-acl				. #x0020 )
			   ( no-fflags			. #x0040 )
			   ( no-sparse			. #x0080 )
			   ))

;;;

(define-libarchive-fn	'*		archive:read-disk-new)

(define-class <archive-disk-reader> (<archive-reader>)
  (allocator
   allocation: #:class
   init-value: archive:read-disk-new)
  (metadata-shim setter: set-metadata-shim))

(define-method (initialize (self <archive-disk-reader>) initargs)
  (next-method))

(define-method (set-format-support (self <archive-disk-reader>))
  #t)

(define-method (set-filter-support (self <archive-disk-reader>))
  #t)

(define-libarchive-fn	int		archive:read-disk-open '* '*)

(define-method (open-filename (self <archive-disk-reader>) (filename <string>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-disk-open archive-ptr
												 (string->pointer filename)))))

(define-libarchive-fn	int		archive:read-disk-set-behavior '* int)

(define-method (set-behavior (self <archive-disk-reader>) (flags <integer>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-disk-set-behavior archive-ptr flags))))

(define-method (set-behavior (self <archive-disk-reader>) (flag <archive-disk-read-behaviors>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-disk-set-behavior archive-ptr (as-number flag)))))

(define-method (set-behavior (self <archive-disk-reader>) . flags)
  (set-behavior self (flaglist->number flags (subsets <archive-disk-read-behaviors>))))

(define-libarchive-fn	int		archive:read-disk-set-symlink-logical '*)
(define-libarchive-fn	int		archive:read-disk-set-symlink-physical '*)
(define-libarchive-fn	int		archive:read-disk-set-symlink-hybrid '*)

(define-method (set-symlink (self <archive-disk-reader>) (mode <symbol>))
  (with-archive-ptr self
	(archive-check-error self
						 ((case mode
						   ((logical) archive:read-disk-set-symlink-logical)
						   ((physical) archive:read-disk-set-symlink-physical)
						   ((hybrid) archive:read-disk-set-symlink-hybrid))
						  archive-ptr))))

(define-libarchive-fn	'*		archive:read-disk-gname '* int64)
(define-libarchive-fn	'*		archive:read-disk-uname '* int64)

(define-method (user-name (self <archive-disk-reader>) (uid <integer>))
  (with-archive-ptr self
	(pointer->maybe-string (archive:read-disk-uname archive-ptr uid))))

(define-method (group-name (self <archive-disk-reader>) (gid <integer>))
  (with-archive-ptr self
	(pointer->maybe-string (archive:read-disk-gname archive-ptr gid))))

(define-libarchive-fn	int		archive:read-disk-set-standard-lookup '*)

(define-method (set-standard-lookup (self <archive-disk-reader>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-disk-set-standard-lookup archive-ptr))))

(define-libarchive-fn	int		archive:read-disk-can-descend '*)
(define-libarchive-fn	int		archive:read-disk-descend '*)

(define-method (can-descend (self <archive-disk-reader>))
  (with-archive-ptr self
	(< 0 (archive-check-error self
							  (archive:read-disk-can-descend archive-ptr)))))

(define-method (descend (self <archive-disk-reader>))
  (with-archive-ptr self
	(archive-check-error self
						 (archive:read-disk-descend archive-ptr))))

(define-libarchive-fn	int		archive:read-disk-set-metadata-filter-callback '* '* '*)

(define-method (metadata-filter (self <archive-disk-reader>) (proc <applicable>))
  (let* ((closure (lambda (_ __ entry-ptr)
					(let ((entry (make <archive-entry-base> entry-ptr: entry-ptr owner: self)))
					  (if (proc self entry) 1 0))))
		 (shim (procedure->pointer int closure '(* * *))))
	(set-metadata-shim self (cons closure shim))
	(with-archive-ptr self
	  (archive-check-error self
						   (archive:read-disk-set-metadata-filter-callback archive-ptr shim %null-pointer)))))


;; end
