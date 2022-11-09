;;;
;;; sib-graphics.lisp
;;;
;;; graphics framebuffer emulation on the SIB board, variables and utils.
;;;

(in-package :nevermore)

(declaim (type (simple-array (unsigned-byte 8) (*)) *sib-framebuffer*))
(defvar *sib-framebuffer* (make-array '(#x20000) :element-type '(unsigned-byte 8)
				      :initial-element 0))

(declaim (type (simple-array (unsigned-byte 32) ()) *sib-graphics-mask-register*))
(defvar *sib-graphics-mask-register* (make-array () :element-type '(unsigned-byte 32)
						 :initial-element 0))

(defvar *sib-graphics-logical-operation* 0)

(declaim (type (simple-array t (*)) *sib-bmp-header*))
(defparameter *sib-bmp-header* #(#x42 #x4d #x3e #x94 1 0 0 0 0 0 #x3e 0 0 0 #x28 0 0 0 0 4 0 0 #x28 3 0 0 1 0 1 0 0 0 0 0 0 #x94 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #xff #xff #xff 0))

(defun sib-write-screenshot (filename)
  (with-open-file (imagefile filename :direction :output
			     :element-type '(unsigned-byte 8))
    (dotimes (i #x3e)
      (write-byte (aref *sib-bmp-header* i) imagefile))
    (dotimes (y 808)
      (dotimes (x 128)
	(let ((data (aref *sib-framebuffer* (+ x (* (- 807 y) 128)))))
	  (setf data (logior (ash (logand data #x55) 1)
			     (ash (logand data #xaa) -1)))
	  (setf data (logior (ash (logand data #x33) 2)
			     (ash (logand data #xcc) -2)))
	  (setf data (logior (ash (logand data #x0f) 4)
			     (ash (logand data #xf0) -4)))
	  
	  (write-byte data imagefile))))))

(defvar *sib-character-recognizer* ()
  "A property list matching a (unique) hash of the bitmap for a character to the character itself.")

(defun sib-init-character-recognizer ()
  "Initialize the character recognizer for the text screenshot system. May be dependant on SIB ROM version."
  (let ((foo ()))
    (dotimes (i 95)
      (let ((sum 0))
	(dotimes (j 12)
	  (incf sum (ash (aref *sib-config-rom* (+ #x19a9 (* i 12) j)) (- 11 j))))
	(push (code-char (+ #x20 i)) foo)
	(push sum foo)))
    (setf *sib-character-recognizer* foo))
  (values))

(declaim (inline sib-recognize-character))
(defun sib-recognize-character (x y)
  (let ((address (+ (* (+ (* 13 y) 8) 128) x 1))
	(sum 0))
    (dotimes (row 12)
      (incf sum (ash (aref *sib-framebuffer* (+ address (* row 128))) (- 11 row))))
    (getf *sib-character-recognizer* sum)))

(defun sib-text-screenshot ()
  (dotimes (y 61)
    (dotimes (x 126)
      (format t "~A" (sib-recognize-character x y)))
    (terpri)))

;;; EOF
