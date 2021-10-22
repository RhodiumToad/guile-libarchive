;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive entry acl)
  use-module: (oop goops)
  use-module: (system foreign)
  use-module: (srfi srfi-1)
  use-module: (srfi srfi-11)		;; let-values
  use-module: ((srfi srfi-71)
			   select: (unlist))
  use-module: (libarchive base)
  use-module: (libarchive flagset)
  use-module: (libarchive entry)

  re-export: (universe
			  flag-lookup
			  subset-lookup
			  as-flags
			  as-number
			  contains?
			  contains-all?
			  contains-any?
			  contains-any-of-subsets)

  export: (<archive-entry-acl-types>
		   <archive-entry-acl-styles>
		   <archive-entry-acl-permset>
		   entry-acl-types
		   entry-acl-count
		   entry-acl-reset
		   entry-acl-next
		   entry-acl-to-text

		   entry-acl-clear
		   )
  )

;;;

(define-class <archive-entry-acl-types> (<flagset-base>)
  universe: '(	(access		. #x00000100)
				(default	. #x00000200)
				(allow		. #x00000400)
				(deny		. #x00000800)
				(audit		. #x00001000)
				(alarm		. #x00002000)
				)
  subsets: '(	(posix1e	access default)
				(nfs4		allow deny audit alarm)
			 )
  )

(define-class <archive-entry-acl-styles> (<flagset-base>)
  universe: '(	(access				. #x00000100)
				(default			. #x00000200)
				(extra-id			. #x00000001)
				(mark-default		. #x00000002)
				(solaris			. #x00000004)
				(separator-comma	. #x00000008)
				(compact			. #x00000010)
				))

(define-class <archive-entry-acl-permset> (<flagset-base>)
  universe: '(	(execute					. #x00000001)
				(write						. #x00000002)
				(read						. #x00000004)
				(read-data					. #x00000008)
				(list-directory				. #x00000008)
				(write-data					. #x00000010)
				(add-file					. #x00000010)
				(append-data				. #x00000020)
				(add-subdirectory			. #x00000020)
				(read-named-attrs			. #x00000040)
				(write-named-attrs			. #x00000080)
				(delete-child				. #x00000100)
				(read-attributes			. #x00000200)
				(write-attributes			. #x00000400)
				(delete						. #x00000800)
				(read-acl					. #x00001000)
				(write-acl					. #x00002000)
				(write-owner				. #x00004000)
				(synchronize				. #x00008000)

				(entry-inherited			. #x01000000)
				(entry-file-inherit			. #x02000000)
				(entry-directory-inherit	. #x04000000)
				(entry-no-propagate-inherit	. #x08000000)
				(entry-inherit-only			. #x10000000)
				(entry-successful-access	. #x20000000)
				(entry-failed-access		. #x40000000)
			  )
  subsets: '(	(posix1e	execute write read)
				(nfs4		execute read-data list-directory write-data
							add-file append-data add-subdirectory
							read-named-attrs write-named-attrs delete-child
							read-attributes write-attributes delete
							read-acl write-acl write-owner synchronize)
				(nfs4-inherit	entry-inherited entry-file-inherit
								entry-directory-inherit
								entry-no-propagate-inherit
								entry-inherit-only
								entry-successful-access
								entry-failed-access)
				)
  )

(define %acl-tagvals '((user		. 10001)
					   (user-obj	. 10002)
					   (group		. 10003)
					   (group-obj	. 10004)
					   (mask		. 10005)
					   (other		. 10006)
					   (everyone	. 10107)))

(define %acl-tagsyms '((10001	. user)
					   (10002	. user-obj)
					   (10003	. group)
					   (10004	. group-obj)
					   (10005	. mask)
					   (10006	. other)
					   (10107	. everyone)))

(define (acltag->number sym)
  (or (assq-ref %acl-tagvals sym)
	  (archive-report-parameter-error "bad tag symbol" 'acltag->number sym)))

(define (number->acltag num)
  (or (assv-ref %acl-tagsyms num)
	  (archive-report-parameter-error "bad tag value" 'number->acltag num)))

;;; getters

(define-libarchive-fn int archive:entry-acl-types '*)

(define-method (entry-acl-types (self <archive-entry-base>))
  (with-entry-ptr self
	(make <archive-entry-acl-types>
	  value: (archive:entry-acl-types entry-ptr))))

(define-libarchive-fn int archive:entry-acl-count '* int)

(define-method (entry-acl-count (self <archive-entry-base>) (types <integer>))
  (with-entry-ptr self
	(archive:entry-acl-count entry-ptr types)))

(define-method (entry-acl-count (self <archive-entry-base>) (type <archive-entry-acl-types>))
  (with-entry-ptr self
	(archive:entry-acl-count entry-ptr (as-number type))))

(define-method (entry-acl-count (self <archive-entry-base>) . types)
  (entry-acl-count self (flaglist->number types (subsets <archive-entry-acl-types>))))

(define-libarchive-fn void archive:entry-acl-reset '*)

(define-method (entry-acl-reset (self <archive-entry-base>) (types <integer>))
  (with-entry-ptr self
	(archive:entry-acl-reset entry-ptr types)))

(define-method (entry-acl-reset (self <archive-entry-base>) (type <archive-entry-acl-types>))
  (with-entry-ptr self
	(archive:entry-acl-reset entry-ptr (as-number type))))

(define-method (entry-acl-reset (self <archive-entry-base>) . types)
  (entry-acl-count self (flaglist->number types (subsets <archive-entry-acl-types>))))

(define-libarchive-fn int archive:entry-acl-next '* int '* '* '* '* '*)

(define %acl-struct (list int int int int '*))
(define %acl-struct-init (list 0 0 0 0 %null-pointer))

(define-method (entry-acl-next (self <archive-entry-base>) (types <integer>))
  (with-entry-ptr self
	(let*-values (((type permset tag qual name)
				   (make-out-param-struct %acl-struct %acl-struct-init)))
	  (if (archive-ok? (archive-check-entry-error self
						(archive:entry-acl-next entry-ptr
												types
												type
												permset
												tag
												qual
												name)))
		  (let-values (((rtype rperms rtag rqual rname)
						(unlist (parse-c-struct type %acl-struct))))
			(list (make <archive-entry-acl-types> value: rtype)
				  (make <archive-entry-acl-permset> value: rperms)
				  (number->acltag rtag)
				  qual
				  (pointer->string name)))
		  #f))))

(define-method (entry-acl-next (self <archive-entry-base>) (type <archive-entry-acl-types>))
  (entry-acl-next self (as-number type)))

(define-method (entry-acl-next (self <archive-entry-base>) . types)
  (entry-acl-next self (flaglist->number types (subsets <archive-entry-acl-types>))))

(define-libarchive-fn '* archive:entry-acl-to-text '* '* int)

(define-method (entry-acl-to-text (self <archive-entry-base>) (style <integer>))
  (with-entry-ptr self
	(pointer->maybe-string (archive:entry-acl-to-text entry-ptr %null-pointer style))))

(define-method (entry-acl-to-text (self <archive-entry-base>) (style <archive-entry-acl-styles>))
  (with-entry-ptr self
	(pointer->maybe-string (archive:entry-acl-reset entry-ptr (as-number style)))))

(define-method (entry-acl-to-text (self <archive-entry-base>) . styles)
  (entry-acl-to-text self (flaglist->number styles (universe <archive-entry-acl-styles>))))

;;; mutators

(define-libarchive-fn void archive:entry-acl-clear '*)

(define-method (entry-acl-clear (self <archive-entry>))
  (with-entry-ptr self
	(archive:entry-acl-clear entry-ptr)))

(define-libarchive-fn int archive:entry-acl-from-text '* '* int)

(define-method (entry-acl-from-text (self <archive-entry>) (acl <string>) (type <integer>))
  (with-entry-ptr self
	(archive-check-entry-error self
							   (archive:entry-acl-from-text entry-ptr (string->pointer acl) type))))

(define-method (entry-acl-from-text (self <archive-entry>) (acl <string>) (type <symbol>))
  (entry-acl-from-text self acl (flaglist->number (list type) (subsets <archive-entry-acl-types>))))

(define-method (entry-acl-from-text (self <archive-entry>) (acl <string>) (type <archive-entry-acl-types>))
  (entry-acl-from-text self acl (as-number type)))

;; end
