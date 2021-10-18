;; -*- mode: Scheme; tab-width: 4 -*-

(eval-when (expand load eval)
  (read-set! keywords 'postfix))

;; this hack of doing define-module twice is because define-module*
;; processes the duplicates: keyword too late, after doing re-exports.
;; this causes failures in the event that generics need to be merged
;; for a re-exported symbol.

(define-module (libarchive)
  use-module: (oop goops)
  duplicates: (merge-generics replace warn-override-core warn last))

(define-module (libarchive)
  use-module:	(libarchive base)
  use-module:	(libarchive reader)
  use-module:	(libarchive writer)
  use-module:	(libarchive entry)

  re-export-and-replace: (close)

  re-export:  (open-filename))

(module-use-interfaces! (module-public-interface (current-module))
						(list (resolve-interface '(libarchive reader))
							  (resolve-interface '(libarchive writer))
							  (resolve-interface '(libarchive entry))))

