;;;
;;; nevermore-package.lisp
;;;
;;; define the package for nevermore
;;;

(defpackage :nevermore (:use :common-lisp)
	    (:export #:*micro-instruction-trace*
		     #:*inhibit-nubus-trace*
		     #:microengine-force-halt
		     #:*microcycle-hooks*
		     #:add-microcycle-hook
		     #:run-microcycle-hooks
		     #:*cpu-nubus-slot*
	       #+nil #:*memory-data*
		     #:*nubus-error*
		     #:nubus-read
		     #:nubus-write
		     #:start-nubus-read
		     #:start-nubus-write
		     #:start-nubus-byte-read
		     #:start-nubus-byte-write
		     ;; exporting the following symbols is a nasty hack.
		     #:cpu-nubus-read
		     #:cpu-nubus-write
		     #:*machine-control-register*
		     #:*memory-busy*
		     #:*page-fault*))
(defpackage :raven (:use :common-lisp :nevermore)
            (:export #:*microcycles-executed*))
(defpackage :hummingbird (:use :common-lisp :nevermore))

;;; EOF
