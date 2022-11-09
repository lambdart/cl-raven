;;;
;;; sib-variables.lisp
;;;
;;; Variables required by the SIB board.
;;;

(in-package :nevermore)

(declaim (type (simple-array (unsigned-byte 8) (*)) *sib-config-rom*))
(defvar *sib-config-rom* (make-array '(#x2000) :element-type '(unsigned-byte 8)
				     :initial-element 0)
  "NuBus Configuration ROM image")

(defvar *sib-keyboard-recv-fifo* ()
  "Keyboard UART receive FIFO")

;;; EOF
