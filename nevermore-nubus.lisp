;;;
;;; nevermore-nubus.lisp
;;;
;;; NuBus interface emulation.
;;;

(in-package :nevermore)

;; FIXME: These functions are a mix between generic NuBus accessors, Raven
;; processor memory access emulation, and a poor implementation of the NuBus
;; memory map. In order to properly support Hummingbird the memory access
;; emulation needs to be pushed back to the CPU layer and the cpu access
;; handlers (cpu-nubus-read and cpu-nubus-write) need to be handled better.
;; Ideally the NuBus memory map handling would be rewritten as well.

;; FIXME: The bus-mastering protocol for non-CPU boards is needlessly complex.
;; And probably doesn't work under all circumstances.

(declaim (type (simple-array (unsigned-byte 32) ()) *memory-data*))
(defvar *memory-data* (make-array () :element-type '(unsigned-byte 32) :initial-element 0))
(defvar *nubus-error* nil "T if last NuBus access caused an error")


(defun nubus-read (slot address width)
  "Read from NuBus space at ADDRESS with WIDTH either :BYTE, :HALF or :WORD. Sets *nubus-error* to T if memory not mapped, otherwise sets (aref *memory-data*) to the requisite value."
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address))

  (setf *page-fault* nil)
  (setf *nubus-error* nil)
  (when (zerop (logand #x100 *machine-control-register*))
    ;; Memory cycle disabled.
    (setf *memory-busy* 0)
    (return-from nubus-read))
  ;; Memory busy should remain high for three cycles after this one.
  ;; Memory busy counter is decremented at the end of the cycle, so we
  ;; have to account for this one as well.
  ;; FIXME: Was 4, trying other values.
  (setf *memory-busy* 6)

  (let ((*inhibit-nubus-trace* *inhibit-nubus-trace*))
    (if (= (ldb (byte 4 4) slot) #xf)
        (let ((slot-id (ldb (byte 4 0) slot)))
          (cond ((= slot-id *cpu-nubus-slot*) (cpu-nubus-read slot address width))
                ((= slot-id *sib-nubus-slot*) (sib-nubus-read slot address width))
                ((= slot-id *mem-nubus-slot*) (memory-nubus-read slot address width))
                ((= slot-id *nupi-nubus-slot*) (nupi-nubus-read slot address width))
                (t (setf *nubus-error* t))))
        (setf *nubus-error* t))
    (when (or *nubus-error* (not *inhibit-nubus-trace*))
      (format t "~AReading NuBus Address ~X ~X~%"
              (if (eq width :byte) "Byte " (if (eq width :half) "Half " ""))
              slot address)))
  (values))

(defun nubus-write (slot address width)
  "Write (aref *memory-data*) to NuBus space at ADDRESS with WIDTH either :BYTE, :HALF or :WORD. Sets *nubus-error* to T if memory not mapped."
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address))

  (setf *page-fault* nil)
  (setf *nubus-error* nil)
  (when (zerop (logand #x100 *machine-control-register*))
    ;; Memory cycle disabled.
    (setf *memory-busy* 0)
    (return-from nubus-write))
  ;; Memory busy should remain high for three cycles after this one.
  ;; Memory busy counter is decremented at the end of the cycle, so we
  ;; have to account for this one as well.
  ;; FIXME: Was 4, trying other values.
  (setf *memory-busy* 6)

  (let ((*inhibit-nubus-trace* *inhibit-nubus-trace*))
    (if (= (ldb (byte 4 4) slot) #xf)
        (let ((slot-id (ldb (byte 4 0) slot)))
          (cond ((= slot-id *cpu-nubus-slot*) (cpu-nubus-write slot address width))
                ((= slot-id *sib-nubus-slot*) (sib-nubus-write slot address width))
                ((= slot-id *mem-nubus-slot*) (memory-nubus-write slot address width))
                ((= slot-id *nupi-nubus-slot*) (nupi-nubus-write slot address width))
                (t (setf *nubus-error* t))))
        (setf *nubus-error* t))
    (when (or *nubus-error* (not *inhibit-nubus-trace*))
      (format t "~AWriting NuBus Address ~X ~X: ~X~%"
              (if (eq width :byte) "Byte " (if (eq width :half) "Half " ""))
              slot address (aref *memory-data*))))
  (values))


(declaim (inline start-nubus-read))
(defun start-nubus-read (address)
  (if (zerop (logand 1 address))
      (nubus-read (ldb (byte 8 24) address) (ldb (byte 24 0) address) :word)
      (nubus-read (ldb (byte 8 24) address) (ldb (byte 24 0) (logand -2 address)) :half)))

(declaim (inline start-nubus-write))
(defun start-nubus-write (address)
  (if (zerop (logand 1 address))
      (nubus-write (ldb (byte 8 24) address) (ldb (byte 24 0) address) :word)
      (nubus-write (ldb (byte 8 24) address) (ldb (byte 24 0) (logand -2 address)) :half)))

(declaim (inline start-nubus-byte-read))
(defun start-nubus-byte-read (address)
  (nubus-read (ldb (byte 8 24) address) (ldb (byte 24 0) address) :byte))

(declaim (inline start-nubus-byte-write))
(defun start-nubus-byte-write (address)
  (nubus-write (ldb (byte 8 24) address) (ldb (byte 24 0) address) :byte))

;;; EOF
