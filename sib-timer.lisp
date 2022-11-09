;;;
;;; sib-timer.lisp
;;;
;;; SIB board interval timers
;;;

(in-package :nevermore)

;; This looks like the same 8253/8254 PIT that the PC uses,
;; clocked at once every 7 microcycles (1 MHz).

;;
;;   counter 0 is driven by 1Mhz nubus clock, and appears to be always
;; set up for 'interrupt on terminal count'

;;   counter 1 is driven by 1Mhz nubus clock, and this output drives
;; counter 2.  Counter 1 seems to be set up as square wave.

;; the 'short term interval timer' is counter 0
;; the 'long term interval timer' is counter 2
;;
(defvar *sib-timer-0-count* 0)
(defvar *sib-timer-0-latch* 0)
(defvar *sib-timer-0-byte* 0)

(defvar *sib-timer-registers* nil)       ;; values for timer counters
(defvar *sib-timer-register-latches* nil)
(defvar *sib-timer-register-states* nil) ;; msb/lsb state tracking

(defvar *sib-timer-active-counter* nil)

;;  (setf (getf *rtc-registers* address) value))

(defun sib-timer-read (address)
;; (declare (ignorable address))
;;  (cond
;;    ((= address #xf90000)
;;     (format t "read ~x~%" address))
;;    ((= address #xf90004)
;;     (format t "read ~x~%" address))
;;    ((= address #xf90008)
;;     (format t "read ~x~%" address))
;;    ((= address #xf9000c)
;;     (progn
;;       (format t "shouldn't happen: read: ~x~%" address))))             ;; control register

  ;; counter 0 (short interval timer) is derived from the microcycle counter
  ;; counter 1 (long interval timer) is derived from sbcl's internal millisecond counter
  ;;
  ;; XXX should reverse this so the flip-flop is on the outside and the tiemr select
  ;; XXX is performed implicitly rather than with a wrapping (cond)
  ;; XXX but I'm not that smart.
  ;;
;;  (format t "timer read at #x~X counter ~A ~X ~X ~X~%"
;;	  raven::*micro-instruction-pointer*
;;	  *sib-timer-active-counter*
;;	  (getf *sib-timer-register-latches* 0)
;;	  (getf *sib-timer-register-latches* 1)
;;	  (getf *sib-timer-register-latches* 2))

  (cond
    ((= *sib-timer-active-counter* 0)
     (progn
       (cond
         ((= 0 (getf *sib-timer-register-states* 0))
          (progn
            (setf (getf *sib-timer-register-states* 0) 1)
            (return-from sib-timer-read
              (mod (getf *sib-timer-register-latches* 0) #xff))))
         ((= 1 (getf *sib-timer-register-states* 0))
          (progn
           (setf (getf *sib-timer-register-states* 0) 0)
            (return-from sib-timer-read
              (mod (ldb (byte 8 8) (getf *sib-timer-register-latches* 0)) #xff)))))))
     ((= *sib-timer-active-counter* 2)
      (progn
        (format t "counter 2~%")
        (mod (get-internal-real-time) #xff)))
     (t
      (format t "unimpl counter ~d~%" *sib-timer-active-counter*))))

(defun sib-timer-write (address data)
  (declare (ignorable address data))
  (cond
    ((= address #xf90000)
     (format t "write ~x at ~x~%" data address))
    ((= address #xf90004)
     (format t "write ~x at ~x~%" data address))
    ((= address #xf90008)
     (format t "write ~x at ~x~%" data address))
    ((= address #xf9000c)   ;; XX blindly assume we're latching the counter value
     (progn
       (setf *sib-timer-active-counter* (ldb (byte 2 6) data))

       (setf (getf *sib-timer-register-states* *sib-timer-active-counter*) 0)
       (cond
         ((= 0 *sib-timer-active-counter*)
          (setf (getf *sib-timer-register-latches* 0)
                (mod (get-internal-real-time) #xffff)))
         ((= 2 *sib-timer-active-counter*)
          (setf (getf *sib-timer-register-latches* 2)
                (mod (get-internal-real-time) #xffff)))
         (t
          (format t "unhandled timer counter value: ~x~%" *sib-timer-active-counter*)))))
    (t
     (format t "timer: unknown write address: ~x~%" address))) ;; control register

  (setf *sib-timer-0-latch* *sib-timer-0-count*)
  (setf *sib-timer-0-byte* 8)
  (setf *inhibit-nubus-trace* t))

(defun sib-timer-clock ()
  (setf *sib-timer-0-count* (logand #xffff (1- *sib-timer-0-count*))))

(declaim (fixnum *sib-timer-7cycle-count*))
(defvar *sib-timer-7cycle-count* 0)

(defun sib-timer-microcycle-handler ()
  (if (zerop *sib-timer-7cycle-count*)
      (progn
        (sib-timer-clock)
        (setf *sib-timer-7cycle-count* 100))
      (decf *sib-timer-7cycle-count*)))

(add-microcycle-hook #'sib-timer-microcycle-handler)

;;
;; The rtc
;;   The rtc contains values that are BCD.
;;
;;   If an rtc register value hasn't been set yet, then return 0.
;; Ingest the millisecond value directly from SBCL's internal clock.
;;
(defvar *rtc-registers* nil)

(defun sib-rtc-read (address)
  (format t "sib-rtc-read: ~x~%" address)
  (if (= address #xF80004)
     (let ((a (sb-impl::get-internal-real-time))
           (milliseconds-bcd 0))
       (format t "at ~x milliseconds ~d~%" address a)
       (setf milliseconds-bcd (dpb (mod a 10) (byte 4 0) milliseconds-bcd))
       (format t "imed value ~x~%" milliseconds-bcd)
       (setf milliseconds-bcd (dpb (mod (nth-value 0 (truncate a 10)) 10)
                                   (byte 4 4) milliseconds-bcd))
       (format t "msecbcd ~x~%" milliseconds-bcd)
       (values milliseconds-bcd))
     (progn
       (format t "sib-rtc-read: ~x~%" address)
       (getf *rtc-registers* address 0))))

(defun sib-rtc-write (address value)
  (format t "sib-rtc-write: ~x ~x~%" address value)
  (setf (getf *rtc-registers* address) value))
;;; EOF
