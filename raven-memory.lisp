;;;
;;; raven-memory.lisp
;;;
;;; On-board CPU memory spaces (I-, D-, T-, A-, M-, etc.)
;;;

(in-package :raven)

(declaim (type (simple-array (unsigned-byte 32) (*)) *m-memory*))
(defvar *m-memory* (make-array '(#x40) :element-type '(unsigned-byte 32)
			       :initial-element 0)
  "M-Memory storage.")

(declaim (type (simple-array (unsigned-byte 32) (*)) *a-memory*))
(defvar *a-memory* (make-array '(#x400) :element-type '(unsigned-byte 32)
			       :initial-element 0)
  "A-Memory storage.")

(declaim (type (simple-array (unsigned-byte 32) (*)) *prom-memory-m*))
(defvar *prom-memory-m* (make-array '(#x800) :element-type '(unsigned-byte 32)
				  :initial-element 0)
  "CPU I-Memory PROM Overlay (low).")

(declaim (type (simple-array (unsigned-byte 24) (*)) *prom-memory-a*))
(defvar *prom-memory-a* (make-array '(#x800) :element-type '(unsigned-byte 24)
				  :initial-element 0)
  "CPU I-Memory PROM Overlay (high).")

(declaim (type (simple-array (unsigned-byte 32) (*)) *i-memory-m*))
(defvar *i-memory-m* (make-array '(#x4000) :element-type '(unsigned-byte 32)
			       :initial-element 0)
  "CPU I-Memory (low).")

(declaim (type (simple-array (unsigned-byte 24) (*)) *i-memory-a*))
(defvar *i-memory-a* (make-array '(#x4000) :element-type '(unsigned-byte 24)
			       :initial-element 0)
  "CPU I-Memory (high).")

(declaim (type (simple-array (unsigned-byte 32) (*)) *t-memory*))
(defvar *t-memory* (make-array '(16) :element-type '(unsigned-byte 32)
			       :initial-element 0)
  "Tag-Classifier Memory.")

(declaim (inline write-t-memory))
(defun write-t-memory (address data)
  (declare (type (unsigned-byte 32) data))
  (setf (aref *t-memory* address)
	;; This first version requires a full-call of lognot because the result overflows 32 bits.
	#+nil(dpb (ldb (byte 1 30) data)
	     (byte 1 (ldb (byte 5 25) data))
	     (aref *t-memory* address))
	(let* ((t-mem (aref *t-memory* address))
	       (mask (ash 1 (ldb (byte 5 25) data)))
	       (data-bit (ldb (byte 1 30) data))
	       (t-mem-2 (logand t-mem (logxor mask #xffffffff))))
	  (logior t-mem-2 (ash data-bit (ldb (byte 5 25) data))))))

(declaim (inline read-t-memory))
(defun read-t-memory (address data)
  (declare (type (unsigned-byte 32) data))
  (ldb (byte 1 (ldb (byte 5 25) data))
	     (aref *t-memory* address)))

(declaim (inline write-m-memory))
(defun write-m-memory (address data)
  (declare (type (unsigned-byte 32) data))
  (setf (aref *m-memory* address) data)
  (setf (aref *a-memory* address) data)
  (values))

(declaim (inline read-m-memory))
(defun read-m-memory (address)
  (declare (fixnum address))
  (if (< address #x40)
      (aref *m-memory* address)
    (read-functional-source (logand address #x3f))))

(declaim (inline write-a-memory))
(defun write-a-memory (address data)
  (setf (aref *a-memory* address) data)
  (values))

(declaim (inline read-a-memory))
(defun read-a-memory (address)
  (aref *a-memory* address))

(declaim (inline read-i-memory-a))
(defun read-i-memory-a (address)
  (if (and (< address #x800)
	   (zerop (logand *machine-control-register* #x800)))
      (aref *prom-memory-a* address)
    (aref *i-memory-a* address)))

(declaim (inline read-i-memory-m))
(defun read-i-memory-m (address)
  (if (and (< address #x800)
	   (zerop (logand *machine-control-register* #x800)))
      (aref *prom-memory-m* address)
    (aref *i-memory-m* address)))

(declaim (inline write-i-memory-a))
(defun write-i-memory-a (address data)
  (setf (aref *i-memory-a* address) data))

(declaim (inline write-i-memory-m))
(defun write-i-memory-m (address data)
  (setf (aref *i-memory-m* address) data))

#|
;; NOTE: Duplicate of function in dise1uc.
(defun read-one-instruction (stream)
  (read-byte stream)
  (let ((word 0))
    (dotimes (i 7)
      (setf word (dpb (read-byte stream)
		      (byte 8 (* i 8))
		      word)))
    word))

(defun load-prom-memory (filename)
  (with-open-file (romfile filename :direction :input
			   :element-type '(unsigned-byte 8))
		  (dotimes (i #x800)
		    (setf (aref *prom-memory* i)
			  (read-one-instruction romfile)))))
|#

#|
(load-prom-memory #p"/home/nyef/src/lisp/aek/E1_eproms/prom_combined")


If you have dise1uc loaded then

(disassemble-instruction (aref *prom-memory* 0))

Should print ((M-0 TEST-SYNCH) SETZ).
|#

;;; EOF
