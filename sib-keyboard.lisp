;;;
;;; sib-keyboard.lisp
;;;
;;; Keyboard emulation on the SIB board.
;;;

(in-package :nevermore)

(declaim (fixnum *sib-keyboard-mode* *sib-keyboard-command* *sib-keyboard-status*))
(defvar *sib-keyboard-mode* 0)
(defvar *sib-keyboard-command* 0)
(defvar *sib-keyboard-status* #x81)

(declaim (fixnum *sib-keyboard-poll-interval*))
(defvar *sib-keyboard-poll-interval* 0)

(declaim (fixnum *sib-keyboard-poll-count*))
(defvar *sib-keyboard-poll-count* 0)

(defun sib-keyboard-microcycle-handler ()
  (setf *sib-keyboard-poll-interval* (logand #xffffff (1+ *sib-keyboard-poll-interval*))))

(add-microcycle-hook #'sib-keyboard-microcycle-handler)

(defun sib-keyboard-read-status ()
  #+nil  (format t "Polling interval: ~D~%" *sib-keyboard-poll-interval*)
  (if (< *sib-keyboard-poll-interval* 2000)
      (when (< 4 (incf *sib-keyboard-poll-count*))
        (setf *sib-keyboard-poll-count* 0)
        (unless *display*
          (format t "Probable keyboard busy-loop.~%")
          (break)))
      (setf *sib-keyboard-poll-count* 0))
  (setf *sib-keyboard-poll-interval* 0)
  (logior (logand #x7d *sib-keyboard-status*)
          (if *sib-keyboard-recv-fifo* 2 0)
          (if (zerop (logand 8 *sib-keyboard-command*)) 0 #x80)))

(defun sib-keyboard-write-control (data)
  (if (not (zerop (logand #x40 *sib-keyboard-command*)))
      (progn
        (setf *sib-keyboard-recv-fifo* ())
        (setf *sib-keyboard-command* (logand #xbf *sib-keyboard-command*))
        (setf *sib-keyboard-mode* data))
      (setf *sib-keyboard-command* data)))

(defun sib-keyboard-read-data ()
  (if *sib-keyboard-recv-fifo*
      (pop *sib-keyboard-recv-fifo*)
      0))

(defun sib-keyboard-write-data (data)
  (if (/= 0 (logand 8 *sib-diagnostic-control*))
      (push data *sib-keyboard-recv-fifo*)
      (progn
        ;; NOTE: Hack to cover keyboard initialization only.
        ;; FIXME: Should probably add data bytes to the other end of the fifo.
        (push 0 *sib-keyboard-recv-fifo*)
        ;; NOTE: Should be #x70 in order to pass the keyboard tests.
        (push #x70 *sib-keyboard-recv-fifo*))))

(defun sib-keyboard-reset ()
  (setf *sib-keyboard-command* #x40))

(defun sib-keyboard-interrupt ()
  "Returns T if keyboard wants to interrupt, NIL otherwise."
  ;; receive only for now.
  (and *sib-keyboard-recv-fifo* t))

;;
;; Keyboard stuffer (for non-X use).
;;

(defvar *sib-keycode-translator* (make-hash-table))

(defun sib-keyboard-stuff-keypress (input-char)
  (let ((keycode (gethash input-char *sib-keycode-translator*)))
    (when keycode
      (setf *sib-keyboard-recv-fifo*
            (append *sib-keyboard-recv-fifo*
                    (list (logior #x80 keycode) keycode))))))

(defun sib-init-keypress-stuffer ()
  (let ((row '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
    (dotimes (i (length row))
      (setf (gethash (elt row i) *sib-keycode-translator*) (+ #x24 i))))
  (let ((row '(#\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P)))
    (dotimes (i (length row))
      (setf (gethash (elt row i) *sib-keycode-translator*) (+ #x38 i))))
  (let ((row '(#\A #\S #\D #\F #\G #\H #\J #\K #\L nil nil #\Return)))
    (dotimes (i (length row))
      (when (elt row i)
        (setf (gethash (elt row i) *sib-keycode-translator*) (+ #x50 i)))))
  (let ((row '(#\Z #\X #\C #\V #\B #\N #\M)))
    (dotimes (i (length row))
      (setf (gethash (elt row i) *sib-keycode-translator*) (+ #x68 i)))))

;;; EOF
