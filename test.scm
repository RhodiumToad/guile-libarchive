;; -*- mode: Scheme; tab-width: 4 -*-

(add-to-load-path (dirname (current-filename)))

(use-modules (oop goops))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
	'(merge-generics replace warn-override-core warn last)))

(use-modules
 (system foreign)
 (rnrs bytevectors)
 (ice-9 optargs)
 (srfi srfi-88)
 (ice-9 binary-ports)
 ;; or just (libarchive) in place of the following 3
 (libarchive reader)
 (libarchive writer)
 (libarchive entry)
 (libarchive disk read)
 (libarchive entry acl))

(define (list-archive filename)
  (let ((archive (make <archive-reader>)))
    (open-filename archive filename)
    (while (next-header archive)
	  (format #t "filename: ~a: ~a ~a ~a ~a ~a ~o ~o ~a ~a ~a ~a ~a ~a ~d [~a]\n"
			  (entry-pathname archive)
			  (entry-hardlink archive)
			  (entry-symlink archive)
			  (entry-uname archive)
			  (entry-gname archive)
			  (entry-strmode archive)
			  (entry-mode archive)
			  (entry-perm archive)
			  (entry-filetype archive)
			  (entry-fflags-text archive)
			  (entry-birthtime-r archive)
			  (entry-atime-r archive)
			  (entry-ctime-r archive)
			  (entry-mtime-r archive)
			  (entry-size archive)
			  (entry-acl-to-text archive)
			  ))
    (close archive)
	(free archive)))

(define (copy-archive infile outfile)
  (let ((archive (make <archive-reader>))
		(out-archive (make <archive-writer> filename: outfile))
		(now (+ (current-time) 1/2))
		(databuf (make-bytevector 131072 0)))
    (open-filename archive infile)
	(open-filename out-archive outfile)
    (while (next-header archive)
	  (format #t "filename: ~a: ~a ~a ~a ~a ~a ~o ~o ~a ~a ~a ~a ~a ~a ~d\n"
			  (entry-pathname archive)
			  (entry-hardlink archive)
			  (entry-symlink archive)
			  (entry-uname archive)
			  (entry-gname archive)
			  (entry-strmode archive)
			  (entry-mode archive)
			  (entry-perm archive)
			  (entry-filetype archive)
			  (entry-fflags-text archive)
			  (entry-birthtime-r archive)
			  (entry-atime-r archive)
			  (entry-ctime-r archive)
			  (entry-mtime-r archive)
			  (entry-size archive)
			  )
	  (let ((newhdr (clone archive)))
		(entry-set-birthtime newhdr now)
		(entry-set-atime newhdr now)
		(entry-set-ctime newhdr now)
		(entry-set-mtime newhdr now)
		(entry-set-fflags-text newhdr #f)
		(write-header out-archive newhdr)
		(do ((bufcount (read-data archive databuf)
					   (read-data archive databuf)))
			((<= bufcount 0))
		  (let loop ((offset 0)
					 (resid bufcount))
			(let ((nbytes (write-data out-archive databuf offset resid)))
			  (when (and (> nbytes 0)
						 (> resid nbytes))
				(loop (+ offset nbytes) (- resid nbytes))))))))
	(close out-archive)
    (close archive)
	(free out-archive)
	(free archive)))

(define (make-archive inpath outfile)
  (let ((archive (make <archive-disk-reader>))
		(out-archive (make <archive-writer> filename: outfile))
		(linkifier (make <archive-entry-linkresolver>))
		(now (+ (current-time) 1/2))
		(databuf (make-bytevector 131072 0)))
	(letrec ((do-data (lambda (hdr)
						(when (> (entry-size hdr) 0)
						  (do ((bufcount (read-data archive databuf)
										 (read-data archive databuf)))
							  ((<= bufcount 0))
							(let loop ((offset 0)
									   (resid bufcount))
							  (let ((nbytes (write-data out-archive databuf offset resid)))
								(when (> resid nbytes 0)
								  (loop (+ offset nbytes) (- resid nbytes))))))))))
	  (open-filename archive inpath)
	  (metadata-filter archive (lambda (a e) (descend a)))
	  (set-standard-lookup archive)
	  (when (string-suffix? ".cpio" outfile)
		(set-format out-archive 'newc))
	  (open-filename out-archive outfile)
	  (set-strategy linkifier out-archive)
      (while (next-header archive)
		(format #t "filename: ~a: ~a ~a ~a ~a ~a ~o ~o ~a ~a ~a ~a ~a ~a ~d\n"
				(entry-pathname archive)
				(entry-hardlink archive)
				(entry-symlink archive)
				(entry-uname archive)
				(entry-gname archive)
				(entry-strmode archive)
				(entry-mode archive)
				(entry-perm archive)
				(entry-filetype archive)
				(entry-fflags-text archive)
				(entry-birthtime-r archive)
				(entry-atime-r archive)
				(entry-ctime-r archive)
				(entry-mtime-r archive)
				(entry-size archive)
				)
		(let* ((newhdr (clone archive))
			   (entlist (linkify linkifier newhdr)))
		  (for-each
		   (lambda (hdr)
			 (write-header out-archive hdr)
			 (do-data hdr))
		   entlist)))
	  (let entloop ((entlist (linkify linkifier)))
		(unless (null? entlist)
		  (for-each
		   (lambda (hdr)
			 (write-header out-archive hdr)
			 (do-data hdr))
		   entlist)
		  (entloop (linkify linkifier))))
	  (close out-archive)
      (close archive)
	  (free out-archive)
	  (free archive))))
