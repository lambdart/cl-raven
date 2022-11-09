;;;
;;; raven-cpu-nubus.lisp
;;;
;;; CPU NuBus slot space handlers.
;;;

(in-package :raven)

(declaim (fixnum *interrupt-sources*))
(defvar *interrupt-sources* 0
  "A bitmask of enabled interrupt sources")

(declaim (type (integer 0 15) *current-interrupt-level*))
(defvar *current-interrupt-level* 0
  "Highest-priority active interrupt source or 0")

(defvar *cpu-config-rom*)

(defun check-interrupt-status ()
#+nil  (format t "CIS: MCR ~X~%" *machine-control-register*)
#+nil  (format t "MIP: ~A~%" *micro-instruction-pointer*)
  (setf *current-interrupt-level* 0)
  (dotimes (i 14 #+nil 16)
    (unless (zerop (ldb (byte 1 (- 15 i)) *interrupt-sources*))
      (setf *current-interrupt-level* (- 15 i))))
  (if (or (zerop *current-interrupt-level*)
	  (zerop (logand #x8000 *machine-control-register*)))
      (setf *interrupt-pending* nil)
    (setf *interrupt-pending* t)))

(defun cpu-nubus-read (slot address width)
  (declare (type (unsigned-byte 8) slot)
	   (type (unsigned-byte 24) address)
	   (ignorable slot width))
  (cond ((= address #xc00000)
	 ;; FIXME: There has to be a better way to express this.
	 (case (logand #x080000c0 *machine-control-register*)
	   (#x00000000 (setf (aref nevermore::*memory-data*) 7))
	   (#x00000040 (setf (aref nevermore::*memory-data*) 5))
	   (#x00000080 (setf (aref nevermore::*memory-data*) 3))
	   (#x000000c0 (setf (aref nevermore::*memory-data*) 1))
	   (#x08000000 (setf (aref nevermore::*memory-data*) 6))
	   (#x08000040 (setf (aref nevermore::*memory-data*) 4))
	   (#x08000080 (setf (aref nevermore::*memory-data*) 2))
	   (#x080000c0 (setf (aref nevermore::*memory-data*) 0))))
	((= (logand address #xfffc00) #xfffc00)
	 (setf *inhibit-nubus-trace* t)
	 (setf (aref nevermore::*memory-data*)
	       (dpb (aref *cpu-config-rom*
			  (ldb (byte 8 2) address))
		    (byte 8 (* 8 (logand 3 address))) 0)))
	(t (setf *nubus-error* t))))

(defun cpu-nubus-write (slot address width)
  (declare (type (unsigned-byte 8) slot)
	   (type (unsigned-byte 24) address)
	   (ignorable slot width))
  (cond ((= (logand address #xffffc3) #xe00000)
	 ;; Interrupt control
	 (let ((interrupt-number (ldb (byte 4 2) address)))
	   (if (zerop (logand 1 (aref nevermore::*memory-data*)))
	       (setf *interrupt-sources* (logand *interrupt-sources* (dpb 0 (byte 1 interrupt-number) #xffff)))
	     (setf *interrupt-sources* (logior *interrupt-sources* (dpb 1 (byte 1 interrupt-number) 0)))))
	 (check-interrupt-status)
	 (setf *inhibit-nubus-trace* t))))


;;; EOF