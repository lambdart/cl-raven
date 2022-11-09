;;;
;;; sib-event.lisp
;;;
;;; SIB event generator emulation.
;;;

(in-package :nevermore)

(defun sib-post-event (event)
  (let ((address (aref *sib-event-generator-addresses* event))
	(old-memory-data (aref *memory-data*))
	(*page-fault* *page-fault*)
	(*memory-busy* *memory-busy*)
	(*nubus-error* *nubus-error*))
    (setf (aref *memory-data*) #xffffffff)
    (start-nubus-byte-write address)
    (setf (aref *memory-data*) old-memory-data)))

;;
;; Event sources by event number
;;
;;  0  RTC
;;  1  Interval timer (short)
;;  2  Interval timer (long)
;;  3  Serial port
;;  4  Parallel port
;;  5  Graphics controller (?)
;;  6  Keyboard
;;  7  Power supply (overtemp)
;;  8  Keyboard chord reset
;;  9  Mouse motion
;; 10  Mouse button change
;; 11  Voice interface (?)
;; 12  Sound parity error (audio data?)
;; 13  Fiber optic link failure?
;; 14  Power failure
;; 15  Power failure

(defun sib-check-events ()
  (when (logbitp 1 *sib-config-register*) ;; interrupts enabled
    (when (sib-keyboard-interrupt) (sib-post-event 6))))

(defparameter *sib-event-poll-interval* 4000)
(defvar *sib-event-poll-count* 42)

(defun sib-event-microcycle-handler ()
  (when (< (decf *sib-event-poll-count*) 0)
    (setf *sib-event-poll-count* *sib-event-poll-interval*)
    (sib-check-events)))

(add-microcycle-hook #'sib-event-microcycle-handler)

;;; EOF
