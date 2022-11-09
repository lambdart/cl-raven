;;;
;;; romload.lisp
;;;
;;; loaders for all of the romfiles needed for the emulator
;;;

(in-package :nevermore)

(defvar *rom-file-path* #p"/home/nyef/oldhome/src/lisp/lispm_stuff/aek/E1_eproms/")

;; with-open-files macro by antifuchs of freenode #lisp.
(defmacro with-open-files ((&body openspecs) &body body)
  (if (consp openspecs)
      `(with-open-file ,(first openspecs)
        (with-open-files ,(rest openspecs)
          ,@body))
      `(progn ,@body)))

(defun load-romfiles (&aux (*default-pathname-defaults* *rom-file-path*))
  (declare (notinline read-byte))

  (with-open-file (romfile (merge-pathnames #p"e1_cpu_config.rom")
			   :direction :input :element-type '(unsigned-byte 8))
    (setf raven::*cpu-config-rom* (make-array #x100 :element-type '(unsigned-byte 8)))
    (dotimes (i #x100)
      (setf (aref raven::*cpu-config-rom* i) (read-byte romfile))))

  (with-open-file (romfile (merge-pathnames #p"2236662_SIB")
			   :direction :input :element-type '(unsigned-byte 8))
    (dotimes (i #x2000)
      (setf (aref *sib-config-rom* i) (read-byte romfile))))
  
  (with-open-file (romfile (merge-pathnames #p"2243924-2_27S291.8MB")
			   :direction :input :element-type '(unsigned-byte 8))
    (dotimes (i #x800)
      (setf (aref *memory-config-rom* (logxor #x400 i)) (read-byte romfile))))

  (with-open-files ((f1 (merge-pathnames #p"2238056-5_NUPI")
			:direction :input :element-type '(unsigned-byte 8))
		    (f2 (merge-pathnames #p"2238057-5_NUPI")
			:direction :input :element-type '(unsigned-byte 8)))
    (dotimes (i #x2000)
      (setf (aref *nupi-config-rom* i)
	    (dpb (read-byte f1) (byte 8 8) (read-byte f2)))))

  (with-open-files ((f1 (merge-pathnames #p"2236480-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f2 (merge-pathnames #p"2236481-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f3 (merge-pathnames #p"2236482-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f4 (merge-pathnames #p"2236483-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f5 (merge-pathnames #p"2236484-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f6 (merge-pathnames #p"2236485-03")
			:direction :input :element-type '(unsigned-byte 8))
		    (f7 (merge-pathnames #p"2236486-03")
			:direction :input :element-type '(unsigned-byte 8)))
    (dotimes (i #x800)
      (let* ((val1 (read-byte f1))
	     (val2 (dpb (read-byte f2) (byte 8 8) val1))
	     (val3 (dpb (read-byte f3) (byte 8 16) val2))
	     (val4 (dpb (read-byte f4) (byte 8 24) val3))
	     (val5 (read-byte f5))
	     (val6 (dpb (read-byte f6) (byte 8 8) val5))
	     (val7 (dpb (read-byte f7) (byte 8 16) val6)))
	(setf (aref raven::*prom-memory-m* (- #x7ff i)) val4)
	(setf (aref raven::*prom-memory-a* (- #x7ff i)) val7)))))

;;; EOF
