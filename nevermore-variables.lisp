;;;
;;; nevermore-variables.lisp
;;;
;;; Assorted variables.
;;;

(in-package :nevermore)

;; Microengine control

(defvar *micro-instruction-trace* t
  "Flag to enable/disable disassembly of microinstructions during step")

(defvar *inhibit-nubus-trace* nil
  "Flag to enable/disable tracing of all NuBus accesses. This can be overridden by nubus errors and some memory areas.")


;; NuBus slot assignments

(declaim (fixnum *cpu-nubus-slot* *sib-nubus-slot* *mem-nubus-slot* *nupi-nubus-slot*))
(defvar *cpu-nubus-slot* 6 "NuBus slot the CPU is in (usually 6)")
(defvar *sib-nubus-slot* 5 "NuBus slot the SIB is in (usually 5)")
(defvar *mem-nubus-slot* 4 "NuBus slot the memory is in (usually 4)")
(defvar *nupi-nubus-slot* 2 "NuBus slot the NuPI is in (usually 2)")
; Ethernet in slot 1 when we add it.


;; Per-microcycle hooks

(defvar *microcycle-hooks* ())

(defun add-microcycle-hook (fn)
  (setf *microcycle-hooks* (cons fn *microcycle-hooks*)))

(defun run-microcycle-hooks ()
  (mapc #'funcall *microcycle-hooks*))

;;; EOF
