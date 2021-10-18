;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

(define-module (libarchive entry)
  use-module: (oop goops)
  use-module: (system foreign)
  use-module: (rnrs bytevectors)
  use-module: (ice-9 regex)
  use-module: (srfi srfi-11)
  use-module: (libarchive base)

  export: (entry-fflags
		   entry-set-fflags-text
		   )

  re-export: (<archive-entry>
			  free
			  clone))

;;;

(define-syntax-rule (define-entry-getter method-name internal-name result-type result-conv)
  (begin
	(define-libarchive-fn result-type internal-name '*)
	(define-method (method-name (self <archive-entry-base>))
	  (with-entry-ptr self
		(result-conv (internal-name entry-ptr))))
	(export method-name)))

(define-inlinable (pointer->maybe-string ptr)
  (if (null-pointer? ptr)
	  #f
	  (pointer->string ptr)))

(define-inlinable (maybe-string->pointer str)
  (if str
	  (string->pointer str)
	  %null-pointer))

;; would use identity, but it can't be inlined
(define-inlinable (no-conversion x) x)

(define-inlinable (nonzero? n) (not (zero? n)))

;;;

;; horrible and very incomplete hack, but short of parsing the header
;; files a la nyacc, what can one do?
;;
;; (though libarchive 4.x will supposedly remove the use of mode_t)

(define-values (time_t mode_t)
  (let ((m (string-match "^([^-]+)-[^-]*-([^0-9-]+).*$" %host-type)))
	(unless m (error "can't parse %host-type"))
	(let ((arch (string->symbol (match:substring m 1)))
		  (os (string->symbol (match:substring m 2))))
	  (values (case os
				((freebsd) (if (eq? arch 'i386) long int64))
				((openbsd netbsd) int64)
				(else long))
			  (case os
				((freebsd darwin) uint16)
				(else uint32))))))

(define (filetype->symbol x)
  (case (logand x #o170000)
	((#o000000) 'hardlink)
	((#o010000) 'fifo)
	((#o020000) 'cdev)
	((#o040000) 'dir)
	((#o060000) 'bdev)
	((#o100000) 'file)
	((#o120000) 'link)
	((#o140000) 'sock)
	(else => no-conversion)))

(define (symbol->filetype x)
  (case x
	((hardlink) #o000000)
	((fifo) #o010000)
	((cdev) #o020000)
	((dir) #o040000)
	((bdev) #o060000)
	((file) #o100000)
	((link) #o120000)
	((sock) #o140000)
	(else (error "unknown filetype" x))))

(define-entry-getter entry-pathname	archive:entry-pathname	'* pointer->string)
(define-entry-getter entry-hardlink	archive:entry-hardlink	'* pointer->maybe-string)
(define-entry-getter entry-symlink	archive:entry-symlink	'* pointer->maybe-string)
(define-entry-getter entry-uname	archive:entry-uname		'* pointer->maybe-string)
(define-entry-getter entry-gname	archive:entry-gname		'* pointer->maybe-string)
(define-entry-getter entry-strmode	archive:entry-strmode	'* pointer->maybe-string)
(define-entry-getter entry-fflags-text archive:entry-fflags-text '* pointer->maybe-string)

(define-entry-getter entry-size		archive:entry-size		int64 no-conversion)
(define-entry-getter entry-size-is-set archive:entry-size-is-set	int nonzero?)

(define-entry-getter entry-nlink	archive:entry-nlink		unsigned-int no-conversion)
(define-entry-getter entry-ino		archive:entry-ino64		int64 no-conversion)
(define-entry-getter entry-uid		archive:entry-uid		int64 no-conversion)
(define-entry-getter entry-gid		archive:entry-gid		int64 no-conversion)

;; watch out for possible change from mode_t to int
(define-entry-getter entry-perm		archive:entry-perm		mode_t no-conversion)
(define-entry-getter entry-mode		archive:entry-mode		mode_t no-conversion)
(define-entry-getter entry-filetype	archive:entry-filetype	mode_t filetype->symbol)

(define-entry-getter entry-atime-is-set	archive:entry-atime-is-set		int nonzero?)
(define-entry-getter entry-mtime-is-set	archive:entry-mtime-is-set		int nonzero?)
(define-entry-getter entry-ctime-is-set	archive:entry-ctime-is-set		int nonzero?)
(define-entry-getter entry-birthtime-is-set archive:entry-birthtime-is-set int nonzero?)

(define-entry-getter entry-atime	archive:entry-atime		time_t no-conversion)
(define-entry-getter entry-mtime	archive:entry-mtime		time_t no-conversion)
(define-entry-getter entry-ctime	archive:entry-ctime		time_t no-conversion)
(define-entry-getter entry-birthtime archive:entry-birthtime time_t no-conversion)

(define-entry-getter entry-atime-nsec archive:entry-atime-nsec	long no-conversion)
(define-entry-getter entry-mtime-nsec archive:entry-mtime-nsec	long no-conversion)
(define-entry-getter entry-ctime-nsec archive:entry-ctime-nsec	long no-conversion)
(define-entry-getter entry-birthtime-nsec archive:entry-birthtime-nsec long no-conversion)

(define-syntax-rule (define-entry-times method-name internal-name1 internal-name2)
  (begin
	(define-method (method-name (self <archive-entry-base>))
	  (with-entry-ptr self
		(+ (internal-name1 entry-ptr) (/ (internal-name2 entry-ptr) 1000000000))))
	(export method-name)))

(define-entry-times	entry-atime-r		archive:entry-atime		archive:entry-atime-nsec)
(define-entry-times	entry-mtime-r		archive:entry-mtime		archive:entry-mtime-nsec)
(define-entry-times	entry-ctime-r		archive:entry-ctime		archive:entry-ctime-nsec)
(define-entry-times	entry-birthtime-r	archive:entry-birthtime archive:entry-birthtime-nsec)

(define-libarchive-fn void archive:entry-fflags '* '* '*)

(define %ulong-struct (list unsigned-long))

;; returns 2 results: setflags, clrflags

(define-method (entry-fflags (self <archive-entry-base>))
  (with-entry-ptr self
	(let* ((setfl (make-c-struct %ulong-struct (list 0)))
		   (clrfl (make-c-struct %ulong-struct (list 0))))
	  (archive:entry-fflags entry-ptr setfl clrfl)
	  (values (car (parse-c-struct setfl %ulong-struct))
			  (car (parse-c-struct clrfl %ulong-struct))))))

;;; Mutators

(define-syntax-rule (define-entry-setter method-name internal-name (argname param-type internal-type param-conv) ...)
  (begin
	(define-libarchive-fn void internal-name '* internal-type ...)
	(define-method (method-name (self <archive-entry>) (argname param-type) ...)
	  (with-entry-ptr self
		(internal-name entry-ptr (param-conv argname) ...)))
	(export method-name)))

(define-entry-setter entry-set-hardlink archive:entry-copy-hardlink (arg <string> '* maybe-string->pointer))
(define-entry-setter entry-set-link archive:entry-copy-link (arg <string> '* maybe-string->pointer))
(define-entry-setter entry-set-pathname archive:entry-copy-pathname (arg <string> '* maybe-string->pointer))
(define-entry-setter entry-set-sourcepath archive:entry-copy-sourcepath (arg <string> '* maybe-string->pointer))
(define-entry-setter entry-set-symlink archive:entry-copy-symlink (arg <string> '* maybe-string->pointer))

(define-entry-setter entry-set-gname archive:entry-copy-gname (arg <string> '* maybe-string->pointer))
(define-entry-setter entry-set-uname archive:entry-copy-uname (arg <string> '* maybe-string->pointer))

;; this one has an error return
(define-libarchive-fn '* archive:entry-copy-fflags-text '* '*)

(define-method (entry-set-fflags-text (self <archive-entry>) (arg <string>))
  (with-entry-ptr self
	(let* ((str (string->pointer arg))
		   (rstr (archive:entry-copy-fflags-text entry-ptr str)))
	  (unless (null-pointer? rstr)
		(archive-report-parameter-error "bad fflags"
										'entry-set-fflags-text
										(- (pointer-address rstr) (pointer-address str))
										arg
										(pointer->string rstr))))))

(define-entry-setter entry-unset-birthtime archive:entry-unset-birthtime)
(define-entry-setter entry-unset-atime archive:entry-unset-atime)
(define-entry-setter entry-unset-ctime archive:entry-unset-ctime)
(define-entry-setter entry-unset-mtime archive:entry-unset-mtime)

(define-syntax-rule (define-entry-time-setters method-name internal-name)
  (begin
	(define-entry-setter method-name internal-name (sec <integer> time_t no-conversion) (nsec <integer> long no-conversion))
	(define-method (method-name (self <archive-entry>) (arg <real>))
	  (let*-values (((time) (floor (* (inexact->exact arg)
									  1000000000)))
					((sec nsec) (floor/ time 1000000000)))
		(method-name self sec nsec)))))

(define-entry-time-setters entry-set-birthtime archive:entry-set-birthtime)
(define-entry-time-setters entry-set-atime archive:entry-set-atime)
(define-entry-time-setters entry-set-ctime archive:entry-set-ctime)
(define-entry-time-setters entry-set-mtime archive:entry-set-mtime)

(define-entry-setter entry-set-size		archive:entry-set-size	(arg <integer> int64 no-conversion))
(define-entry-setter entry-unset-size	archive:entry-unset-size)

(define-entry-setter entry-set-nlink	archive:entry-set-nlink	(arg <integer> unsigned-int no-conversion))
(define-entry-setter entry-set-ino		archive:entry-set-ino64	(arg <integer> int64 no-conversion))
(define-entry-setter entry-set-uid		archive:entry-set-uid	(arg <integer> int64 no-conversion))
(define-entry-setter entry-set-gid		archive:entry-set-gid	(arg <integer> int64 no-conversion))

;; watch out for possible change from mode_t to int
(define-entry-setter entry-set-perm		archive:entry-set-perm	(arg <integer> mode_t no-conversion))
(define-entry-setter entry-set-mode		archive:entry-set-mode	(arg <integer> mode_t no-conversion))

(define-entry-setter entry-set-filetype	archive:entry-set-filetype (arg <integer> unsigned-int no-conversion))

(define-method (entry-set-filetype (self <archive-entry>) (arg <symbol>))
  (entry-set-filetype self (symbol->filetype arg)))

;; end
