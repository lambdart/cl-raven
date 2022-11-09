;;;
;;; sib-mouse-diags.lisp
;;;
;;; Mouse and diagnostic register behavior.
;;;

(in-package :nevermore)

(defvar *sib-mouse-xpos* 0)
(defvar *sib-mouse-ypos* 0)
(defvar *sib-interrupt-enable* 0)
(defvar *sib-diagnostic-control* 0)
(defvar *sib-monitor-control* 0)
(defvar *sib-last-diagnostic-data* 0)
(defvar *sib-diagnostic-data* 0)
(defvar *sib-sound-control* 0)
(defvar *sib-speech-register* 0)

;;; EOF
