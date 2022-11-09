;;;
;;; raven-variables.lisp
;;;
;;; Assorted variables.
;;;

(in-package :raven)


;; Internal memory spaces

(declaim (type (simple-array (unsigned-byte 32) (*)) *microstack*))
(defvar *microstack* (make-array '(64) :element-type '(unsigned-byte 32)
				:initial-element 0)
  "Micro-stack data storage.")

(declaim (type (simple-array (unsigned-byte 32) (*)) *pdl-buffer*))
(defvar *pdl-buffer* (make-array '(#x400) :element-type '(unsigned-byte 32)
				 :initial-element 0))

(declaim (type (simple-array (unsigned-byte 32) (*)) *level-1-map*))
(defvar *level-1-map* (make-array '(#x1000) :element-type '(unsigned-byte 32)
				  :initial-element 0)
  "Level-1 Memory Map storage")

(declaim (type (simple-array (unsigned-byte 32) (*)) *level-2-control*))
(defvar *level-2-control* (make-array '(#x1000)
				      :element-type '(unsigned-byte 32)
				      :initial-element 0)
  "Level-2 Memory Map Control storage")

(declaim (type (simple-array (unsigned-byte 32) (*)) *level-2-address*))
(defvar *level-2-address* (make-array '(#x1000)
				      :element-type '(unsigned-byte 32)
				      :initial-element 0)
  "Level-2 Memory Map Address storage")

(declaim (type (simple-array (unsigned-byte 32) (*)) *d-memory*))
(defvar *d-memory* (make-array '(#x1000) :element-type '(unsigned-byte 32)
			       :initial-element 0)
  "Dispatch memory storage")


;; Internal registers

(declaim (type (unsigned-byte 32)
	       *machine-control-register*)
	 (type (unsigned-byte 26) *location-counter*)
	 (type (unsigned-byte 10) *dispatch-constant* *pdl-buffer-pointer*
	       *pdl-buffer-index*)
	 (type (unsigned-byte 6) *microstack-pointer*)
	 (type (simple-array (unsigned-byte 32) ())
	       *q-register* *virtual-memory-address*
	       *memory-data* *macroinstruction-buffer*))
(defvar *microstack-pointer* 0 "Micro-stack pointer")
(defvar *virtual-memory-address* (make-array () :element-type '(unsigned-byte 32) :initial-element 0))
(defvar *memory-data* (make-array () :element-type '(unsigned-byte 32) :initial-element 0))
(defvar *q-register* (make-array () :element-type '(unsigned-byte 32) :initial-element 0) "The Q register.")
(defvar *pdl-buffer-pointer* 0)
(defvar *pdl-buffer-index* 0)
(defvar *location-counter* 0)
(defvar *macroinstruction-buffer* (make-array () :element-type '(unsigned-byte 32) :initial-element 0) "Here come the Men In Black!")
(defvar *dispatch-constant* 0 "I-Arg?")
(defvar *machine-control-register* 0)
(defvar *need-fetch* nil "Instruction Stream Need Fetch signal")


;; Microengine state

(declaim (fixnum *micro-instruction-pointer*))
(defvar *micro-instruction-pointer* 0
  "Index of next microinstruction to read")

(declaim (fixnum *last-micro-instruction-pointer*))
(defvar *last-micro-instruction-pointer* 0
  "Index of last microinstruction read")

(defvar *inhibit-micro-execution* nil
  "Flag for if the next microinstruction should be ignored.")

(declaim (type (unsigned-byte 24) *micro-instruction-a*))
(defvar *micro-instruction-a* 0
  "Microinstruction currently executing")

(declaim (type (simple-array (unsigned-byte 32) ()) *micro-instruction-m*))
(defvar *micro-instruction-m* (make-array () :element-type '(unsigned-byte 32) :initial-element 0)
  "Microinstruction currently executing")

(declaim (type (unsigned-byte 24) *next-micro-instruction-a*))
(defvar *next-micro-instruction-a* 0
  "Next microinstruction to execute")

(declaim (type (simple-array (unsigned-byte 32) ()) *next-micro-instruction-m*))
(defvar *next-micro-instruction-m* (make-array () :element-type '(unsigned-byte 32) :initial-element 0)
  "Next microinstruction to execute")

(defvar *control-transfer-ring* (make-array '(512)))
(defvar *control-transfer-ring-pointer* 0)
;; Condition bits

(declaim (fixnum *memory-busy*))
(defvar *page-fault* nil "T if last memory/map access caused a page fault.")
(defvar *memory-busy* 0 "Number of cycles for memory-busy or 0.")
(defvar *interrupt-pending* nil "T if interrupts are unmasked and an interrupt occurs")
(defvar *loop-selftest* nil "T if boot prom should loop selftest")

;;; EOF
