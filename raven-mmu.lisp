;;;
;;; raven-mmu.lisp
;;;
;;; Memory address translation.
;;;

(in-package :raven)

(declaim (type (simple-array (unsigned-byte 32) ()) *cached-level-2-control*))
(defvar *cached-level-2-control* (make-array () :element-type '(unsigned-byte 32) :initial-element 0))

(declaim (type (simple-array (unsigned-byte 32) ()) *cached-level-1*))
(defvar *cached-level-1* (make-array () :element-type '(unsigned-byte 32) :initial-element 0))

(declaim (inline write-map-level-2-address))
(defun write-map-level-2-address ()
  (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))))
	 (map-2-address (dpb map-1 (byte 7 5)
			     (ldb (byte 5 8) (aref *memory-data*)))))
    (setf (aref *level-2-address* map-2-address) (aref *virtual-memory-address*)))
  (values))

(declaim (inline write-map-level-2-control))
(defun write-map-level-2-control ()
  (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))))
	 (map-2-address (dpb map-1 (byte 7 5)
			     (ldb (byte 5 8) (aref *memory-data*)))))
    (setf (aref *level-2-control* map-2-address) (aref *virtual-memory-address*)))
  (values))

(defun find-memory-location (mapped-address)
  (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) mapped-address)))
	 (map-2-index (dpb map-1 (byte 7 5) (ldb (byte 5 8) mapped-address)))
	 (map-2-control (aref *level-2-control* map-2-index))
	 (map-2-address (aref *level-2-address* map-2-index)))
    (if (and (logbitp 11 map-1)
	     (logbitp 9 map-2-control))
	(ash (dpb map-2-address (byte 22 8) mapped-address) 2)
	:unmapped)))

(defun check-memory-cycle-abort ()
  #+nil  (when (and *nubus-error* (logbitp 13 *machine-control-register*))
    (format t "Should probably trap.~%")
    (write-functional-destination 5 *micro-instruction-pointer*)
    #+nil(setf *inhibit-micro-execution* t)
    (setf *micro-instruction-pointer* 16)))

(defun check-memory-cycle-abort-2 ()
  (when (and *nubus-error* (logbitp 13 *machine-control-register*))
    (format t "Should probably trap.~%")
    (write-functional-destination 5 *last-micro-instruction-pointer*)
    (write-functional-destination 5 *last-micro-instruction-pointer*)
    (setf *inhibit-micro-execution* t)
    (setf *micro-instruction-pointer* 16)))

(defun start-unmapped-read ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index)))

    (setf (aref *level-1-map* map-1-index)
	  (dpb (logior 2 (- 1 (ldb (byte 1 9) *machine-control-register*)))
	       (byte 2 14) lvl1))

    (setf (aref *level-2-control* map-2-index)
	  (dpb (logior 6 (- 1 (ldb (byte 1 10) *machine-control-register*)))
	       (byte 3 13) map-2-control))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    (start-nubus-read address)
    (check-memory-cycle-abort)))

(defun start-unmapped-write ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index)))

    (setf (aref *level-1-map* map-1-index)
	  (dpb (logior 2 (- 1 (ldb (byte 1 9) *machine-control-register*)))
	       (byte 2 14) lvl1))

    (setf (aref *level-2-control* map-2-index)
	  (dpb (logior 4 (- 1 (ldb (byte 1 10) *machine-control-register*)))
	       (byte 3 13) map-2-control))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    (start-nubus-write address)
    (check-memory-cycle-abort)))

(defun start-unmapped-byte-read ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index)))

    (setf (aref *level-1-map* map-1-index)
	  (dpb (logior 2 (- 1 (ldb (byte 1 9) *machine-control-register*)))
	       (byte 2 14) lvl1))

    (setf (aref *level-2-control* map-2-index)
	  (dpb (logior 2 (- 1 (ldb (byte 1 10) *machine-control-register*)))
	       (byte 3 13) map-2-control))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    (start-nubus-byte-read address)
    (check-memory-cycle-abort)))

(defun start-unmapped-byte-write ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index)))

    (setf (aref *level-1-map* map-1-index)
	  (dpb (logior 2 (- 1 (ldb (byte 1 9) *machine-control-register*)))
	       (byte 2 14) lvl1))

    (setf (aref *level-2-control* map-2-index)
	  (dpb (logior 0 (- 1 (ldb (byte 1 10) *machine-control-register*)))
	       (byte 3 13) map-2-control))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    (start-nubus-byte-write address)
    (check-memory-cycle-abort)))

(defun start-read ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index))
	 (map-2-address (aref *level-2-address* map-2-index))
	 (m1-valid (not (zerop (logand #x800 lvl1))))
	 (m2-access    (not (zerop (logand #x200 map-2-control)))))
    (setf (aref *cached-level-2-control*) map-2-control)
    (setf *page-fault* nil)

    (unless m1-valid
      (setf *page-fault* t))
    
    (unless m2-access
      (setf *page-fault* t))
    
    (setf (aref *level-1-map* map-1-index)
	  (dpb (if *page-fault* 1 0)
	       (byte 2 12)
	       (dpb 1 (byte 2 14) lvl1)))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    (unless *page-fault*
      (start-nubus-read (ash (dpb map-2-address (byte 22 8)
				  (logand #xff address)) 2))
      (check-memory-cycle-abort))))

(defun start-write ()
  (let* ((address (aref *virtual-memory-address*))
	 (map-1-index (ldb (byte 12 13) address))
	 (lvl1 (aref *level-1-map* map-1-index))
	 (map-2-index (dpb lvl1 (byte 7 5) (ldb (byte 5 8) address)))
	 (map-2-control (aref *level-2-control* map-2-index))
	 (map-2-address (aref *level-2-address* map-2-index))
	 (m1-valid     (not (zerop (logand #x800 lvl1))))
	 (m2-writable  (not (zerop (logand #x100 map-2-control))))
	 (m2-access    (not (zerop (logand #x200 map-2-control))))
	 (m2-forceable (not (zerop (logand #x400 map-2-control))))
	 (force-request (not (zerop (logand #x200 *machine-control-register*)))))
    (setf (aref *cached-level-2-control*) map-2-control)
    (setf *page-fault* nil)

    (unless m1-valid
      (setf *page-fault* t))
    
    (unless m2-access
      (setf *page-fault* t))
    
    (unless (or m2-writable (and m2-forceable force-request))
      (setf *page-fault* t))
    
    #+nil (format t "write: ~X ~X ~A ~A ~A ~A ~A"
	    lvl1 map-2-control m1-valid m2-writable
	    m2-access m2-forceable force-request)
    
    (setf (aref *level-1-map* map-1-index)
	  (dpb (if *page-fault* 1 0)
	       (byte 1 13)
	       (dpb (if (and m2-forceable force-request) 0 1)
		    (byte 2 14) lvl1)))

    (setf (aref *cached-level-1*) (aref *level-1-map* map-1-index))

    #+nil (format t " final: ~X VMA ~X MD ~X~%"
	    (aref *level-1-map* map-1-index)
	    (aref *virtual-memory-address*)
	    (aref *memory-data*))

    (unless *page-fault*
      (start-nubus-write (ash (dpb map-2-address (byte 22 8)
				   (logand #xff address)) 2))
      (check-memory-cycle-abort))))


;;; EOF
