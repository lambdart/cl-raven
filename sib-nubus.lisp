;;;
;;; sib-nubus.lisp
;;;
;;; NuBus interface to the SIB board.
;;;

(in-package :nevermore)

(declaim (type (simple-array (unsigned-byte 32) (*)) *sib-event-generator-addresses*))
(defvar *sib-event-generator-addresses* (make-array '(#x10) :element-type '(unsigned-byte 32)
                                                    :initial-element 0))

(declaim (type (simple-array (unsigned-byte 8) (*)) *sib-nvram*))
(defvar *sib-nvram* (make-array '(#x800) :element-type '(unsigned-byte 8)
                                :initial-element 0))

(defvar *sib-config-register* 0)
(defvar *sib-lpt-data* 0)

;; RTC hack
(defvar *sib-hack-millisecond-timer* 0)
(defvar *sib-hack-millisecond-timer-subtimer* 0)
(defun sib-hack-update-millisecond-timer ()
  (setf *sib-hack-millisecond-timer*
        (mod (sb-impl::get-internal-real-time) 99)))

;;  (when (minusp (decf *sib-hack-millisecond-timer-subtimer*))
;;    (setf *sib-hack-millisecond-timer-subtimer* 4)
;;    (incf *sib-hack-millisecond-timer*)

;;    (when (= 10 (logand 15 *sib-hack-millisecond-timer*))
;;      (incf *sib-hack-millisecond-timer* 6))
;;    (when (= 160 (logand 240 *sib-hack-millisecond-timer*))
;;      (setf *sib-hack-millisecond-timer* 0))))
;; End RTC hack

(defun sib-nubus-read (slot address width)
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address)
           (ignorable slot))

;; XXX need this?
;;  (if (= address #xfa0030)
;;      (format t "slot ~X address ~X logand ~X~%"
;;              slot address (logand address #xff8000)))

  (cond
    ((= address #xfa0030)
     (setf (aref *memory-data*) 0)
     (format t "FA0030: stuff happens~%"))

    ((= (logand address #xff8000) #xff8000)
     (setf *inhibit-nubus-trace* t)
     (setf (aref *memory-data*)
           (dpb (aref *sib-config-rom*
                      (ldb (byte 13 2) address)) (byte 8 (* 8 (logand 3 address))) 0)))

    ((= address #xe00084)
     (setf (aref *memory-data*) (aref *sib-graphics-mask-register*)))
    ((= (logand address #xffffc0) #xf00000)
     (setf (aref *memory-data*) (aref *sib-event-generator-addresses* (ldb (byte 4 2) address))))
        ((= (logand address #xff7000) #xfa0000)
	 (setf (aref *memory-data*) (aref *sib-nvram* (ldb (byte 11 2) address))))

        ((= address #xf00040)
         ;; config register
         (setf (aref *memory-data*)
               (logand *sib-config-register* #x03fa)))

        ((= address #xf10000)
         (setf (aref *memory-data*) *sib-lpt-data*))

        ((= address #xf20000)
	 (progn
	   (format t "mouse y read~%")
	   (setf (aref *memory-data*) *sib-mouse-ypos*)))
        ((= address #xf20004)
         (setf (aref *memory-data*) *sib-mouse-xpos*))
        ((= address #xf20008)
         (setf (aref *memory-data*) #x0a))
        ((= address #xf2000c)
         (setf (aref *memory-data*) (dpb *sib-interrupt-enable* (byte 4 4) *sib-diagnostic-control*)))
        ((= address #xf2000d)
         (setf (aref *memory-data*) (dpb *sib-monitor-control* (byte 4 8) 0)))
        ((= address #xf20010)
         (setf (aref *memory-data*) *sib-diagnostic-data*))
        ((= address #xf20014)
         (setf (aref *memory-data*) *sib-sound-control*))
        ((= address #xf20018)
         (setf (aref *memory-data*) *sib-speech-register*))

        ((= address #xf2001c)
         (unless (zerop (logand 1 *sib-diagnostic-control*))
           (setf (aref *memory-data*) *sib-last-diagnostic-data*)))

        ((= (logand address #xfffff3) #xf90000)
         (progn
           (setf *inhibit-nubus-trace* t)
           (setf (aref *memory-data*) (sib-timer-read address))))

        ((= (logand address #xffff00) #xf80000)
         (setf *inhibit-nubus-trace* t)
         (setf (aref *memory-data*) (sib-rtc-read address)))

 ;;       ((= address #xf80004)
         ;; RTC hack
 ;;        (setf (aref *memory-data*) *sib-hack-millisecond-timer*)
 ;;        (sib-hack-update-millisecond-timer))
 ;;       ((= address #xf80005)
 ;;        (nevermore::microengine-force-halt))
 ;;       ((= address #xf80050)
         ;; RTC hack
 ;;        (setf (aref *memory-data*) 0))

        ((= address #xfc0000)
         (setf *inhibit-nubus-trace* t)
         (setf (aref *memory-data*) (sib-keyboard-read-status)))
        ((= address #xfc0004)
         (setf (aref *memory-data*) (sib-keyboard-read-data)))
        ((= (logand address #xfe0000) #xe80000)
         (sib-graphics-read (logand address #x1ffff) width))
        #+nil(t (setf *nubus-error* t)))
  ;; The following break is a hack to allow running extended diagnostics.
  ;; For all but the fourth break, continue. For the fourth break, first
  ;; do (setf (aref *memory-data*) #x15) before continuing. The extended
  ;; diagnostics will then run.
  #+nil (if (= address #xff8028) (break))
  (values))

(defun sib-nubus-write (slot address width)
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address)
           (ignorable slot))
  (cond ((= (logand address #xfe0000) #xe80000)
         (sib-graphics-write (logand address #x1ffff) width))
        ((= (logand address #xfe0000) #xec0000)
         (sib-graphics-read-modify-write (logand address #x1ffff) width))
        ((= address #xe00084)
         (setf (aref *sib-graphics-mask-register*) (aref *memory-data*)))
        ((= address #xe00088)
         (setf *sib-graphics-logical-operation* (logand #xff (aref *memory-data*))))
        ((= (logand address #xffffc0) #xf00000)
         (setf (aref *sib-event-generator-addresses* (ldb (byte 4 2) address)) (aref *memory-data*)))
        ((= (logand address #xff7000) #xfa0000)
         (setf (aref *sib-nvram* (ldb (byte 11 2) address)) (aref *memory-data*)))

        ((= address #xf10000)
         (setf *sib-lpt-data* (aref *memory-data*)))

        ((= address #xf20000)
         (setf *sib-mouse-ypos* (aref *memory-data*)))
        ((= address #xf20004)
         (setf *sib-mouse-xpos* (aref *memory-data*)))
        ((= address #xf2000c)
         (setf *sib-interrupt-enable* (ldb (byte 4 4) (aref *memory-data*)))
         (setf *sib-diagnostic-control* (ldb (byte 4 0) (aref *memory-data*))))
        ((= address #xf2000d)
         (setf *sib-monitor-control* (ldb (byte 4 8) (aref *memory-data*))))
        ((= address #xf20010)
         (setf *sib-last-diagnostic-data* *sib-diagnostic-data*)
         (setf *sib-diagnostic-data* (aref *memory-data*)))
        ((= address #xf20014)
         (setf *sib-sound-control* (aref *memory-data*)))
        ((= address #xf20018)
         (setf *sib-speech-register* (aref *memory-data*)))

        ((= (logand address #xfffff3) #xf90000)
         (sib-timer-write address (logand #xff (aref *memory-data*))))

        ((= (logand address #xffff00) #xf80000)
         (setf *inhibit-nubus-trace* t)
         (sib-rtc-write address (aref *memory-data*)))
        
        ((= address #xfc0000)
         (sib-keyboard-write-control (logand #xff (aref *memory-data*))))
        ((= address #xfc0004)
         (sib-keyboard-write-data (logand #xff (aref *memory-data*))))
        ((= address #xf00040)
         ;; config register
         (setf *sib-config-register* (logand #xffff (aref *memory-data*)))
         (when (not (zerop (logand 1 (aref *memory-data*))))
           (sib-keyboard-reset)))
        #+nil(t (setf *nubus-error* t)))
  (values))

;;; EOF
