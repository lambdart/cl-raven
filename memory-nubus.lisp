;;;
;;; memory-nubus.lisp
;;;
;;; NuBus interface to the memory board.
;;;

(in-package :nevermore)

(declaim (type (simple-array (unsigned-byte 8) (*)) *memory-config-rom*))
(defvar *memory-config-rom* (make-array '(#x800) :element-type '(unsigned-byte 8)
				     :initial-element 0))

(declaim (type (simple-array (unsigned-byte 32) (*)) *memory-buffer*))
(defvar *memory-buffer* (make-array '(#x200000) :element-type '(unsigned-byte 32)
				      :initial-element 0))

(defvar *memory-test-register* 0)

(defun memory-buffer-read (address width)
  (setf *inhibit-nubus-trace* t)
  (cond ((eq width :byte)
	 (setf (aref *memory-data*)
	       (mask-field (byte 8 (* 8 (logand 3 address)))
			   (aref *memory-buffer* (ldb (byte 21 2) address)))))
	((eq width :half)
	 (setf (aref *memory-data*)
	       (mask-field (byte 16 (* 8 (logand 2 address)))
			   (aref *memory-buffer* (ldb (byte 21 2) address)))))
	((eq width :word)
	 (setf (aref *memory-data*)
	       (aref *memory-buffer* (ldb (byte 21 2) address))))
	(t (error "Bogus width")))
  (values))

(defun memory-buffer-write (address width)
  (setf *inhibit-nubus-trace* t)
  (cond ((eq width :byte)
	 (setf (aref *memory-buffer* (ldb (byte 21 2) address))
	       (deposit-field (aref *memory-data*)
			      (byte 8 (* 8 (logand 3 address)))
			      (aref *memory-buffer* (ldb (byte 21 2) address)))))
	((eq width :half)
	 (setf (aref *memory-buffer* (ldb (byte 21 2) address))
	       (deposit-field (aref *memory-data*)
			      (byte 16 (* 8 (logand 2 address)))
			      (aref *memory-buffer* (ldb (byte 21 2) address)))))
	((eq width :word)
	 (setf (aref *memory-buffer* (ldb (byte 21 2) address))
	       (aref *memory-data*)))
	(t (error "Bogus width")))
  (values))

(defun memory-nubus-read (slot address width)
  (declare (type (unsigned-byte 8) slot)
	   (type (unsigned-byte 24) address)
	   (ignorable slot))
  (cond ((= (logand address #xffe000) #xffe000)
	 (setf *inhibit-nubus-trace* t)
	 (setf (aref *memory-data*) (dpb (aref *memory-config-rom* (ldb (byte 11 2) address)) (byte 8 (* 8 (logand 3 address))) 0)))

	((= (logand address #xffffe0) #xffc000)
	 ;; control space
	 (cond
	   ((= address #xffc011)
	    (setf (aref *memory-data*)
		  *memory-test-register*))
	   ((= address #xffc014)
	    ;; NuBus Termination Status
	    ;; Hack to make %NuBus-Read-8B-Careful work.
	    (setf (aref *memory-data*)
		  ;; Set NuBus TM1- and TM0- status to H and L, respectively.
		  ;; (NuBus time-out.)
		  (dpb 1 (byte 2 6) #x0000)))
	   (t (setf (aref *memory-data*) 0))))

	((= (logand address #x800000) #x000000)
	 (memory-buffer-read address width)
	 (if (not (zerop (ldb (byte 1 12) *memory-test-register*)))
	     ;; parity test hack.
	     (let ((value (ldb (byte 8 (* 8 (logand 3 address))) (aref *memory-data*)))
		   (parity (ldb (byte 1 (+ 8 (logand 3 address))) *memory-test-register*)))
	       (setf value (+ (logand #x55 value)
			      (logand #x55 (ash value -1))))
	       (setf value (+ (logand #x33 value)
			      (logand #x33 (ash value -2))))
	       (setf value (+ (logand #x0f value)
			      (logand #x0f (ash value -4))))
	       (when (= (logand 1 value) parity)
		 (setf *nubus-error* t)))))
	(t (setf *nubus-error* t)))
  #+nil  (when (= address #xffe03c) (break))
  (values))

(defun memory-nubus-write (slot address width)
  (declare (type (unsigned-byte 8) slot)
	   (type (unsigned-byte 24) address)
	   (ignorable slot))
  (cond ((= (logand address #x800000) #x000000)
	 (memory-buffer-write address width)
	 (if (not (zerop (ldb (byte 1 12) *memory-test-register*)))
	     ;; parity test hack.
	     (setf *inhibit-nubus-trace* nil)))
	((= (logand address #x00ffffe0) #x00ffc000)
	 ;; control space
	 (case (ldb (byte 3 2) address)
	   (0 ;; memory config register
	    (if (not (zerop (logand 1 (aref *memory-data*))))
		(setf *memory-test-register* 0)))
	   (2 ;; memory base test register
	    )
	   (4 ;; failure location / test register
	    )
	   (5 ;; status error latch
	    )
	   (6 ;; failure register
	    ))
	 (if (= address #xffc011) (setf *memory-test-register* (aref *memory-data*))))
	(t (setf *nubus-error* t)))
  (values))

#|
(with-open-file (romfile #p"/home/nyef/src/lisp/aek/E1_eproms/2243924-2_27S291.8MB"
			 :direction :input :element-type '(unsigned-byte 8))
		(dotimes (i #x800)
		  (setf (aref *memory-config-rom* (logxor #x400 i)) (read-byte romfile))))
|#

;;; EOF
