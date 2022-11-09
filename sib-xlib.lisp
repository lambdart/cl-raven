;;;
;;; sib-xlib.lisp
;;;
;;; CLX interface for SIB emulation (framebuffer and keyboard)
;;;
;;; Thanks to Paul Fuqua for the original version of this code.
;;;

(in-package :nevermore)

(defvar *display* nil)
(defvar *screen*  nil)
(defvar *window*  nil)
(defvar *gc-white* nil)
(defvar *gc-black* nil)

(defun start-x ()
  (setq *display* #+sbcl (xlib:open-default-display)
                  #-sbcl (xlib:open-display (machine-instance)))
  (setq *screen* (xlib:display-default-screen *display*))
  (let ((root (xlib:screen-root *screen*))
        (black-pixel (xlib:screen-black-pixel *screen*)))
    (setq *window* (xlib:create-window :parent root
                                       :x 50 :y 50
                                       :event-mask '(:exposure :key-press :key-release
                             :pointer-motion
                                                               :button-press :button-release)
                                       :width 1024 :height 808
                                       :background black-pixel
                                       :border black-pixel
                                       :border-width 1
                                       :backing-store :when-mapped))
    (xlib:set-wm-properties *window*
                            :name "Explorer Console"
                            :icon-name "Explorer Console"
                            :x 50 :y 50 :width 1024 :height 808
                            :min-width 1024 :min-height 808)))

(defun finish-x ()
  (when *gc-white*
    (xlib:free-gcontext *gc-white*)
    (setq *gc-white* nil))
  (when *gc-black*
    (xlib:free-gcontext *gc-black*)
    (setq *gc-black* nil))
  (when *window*
    (xlib:unmap-window *window*)
    (setq *window* nil))
  (when *display*
    (xlib:display-finish-output *display*)
    (xlib:close-display *display*)
    (setq *display* nil
          *screen*  nil)))

(defun init-window ()
  (xlib:map-window *window*)
  (when *gc-white*
    (xlib:free-gcontext *gc-white*)
    (setq *gc-white* nil))
  (when *gc-black*
    (xlib:free-gcontext *gc-black*)
    (setq *gc-black* nil))
  (setq *gc-white* (xlib:create-gcontext :drawable *window*
                                         :foreground (xlib:screen-white-pixel *screen*)
                                         :background (xlib:screen-black-pixel *screen*))
        *gc-black* (xlib:create-gcontext :drawable *window*
                                         :foreground (xlib:screen-black-pixel *screen*)
                                         :background (xlib:screen-white-pixel *screen*))))

(defun invert-video ()
  (psetq *gc-black* *gc-white*
         *gc-white* *gc-black*)
  (draw-framebuffer))

(defun draw-framebuffer ()
  (xlib:draw-rectangle *window* *gc-black* 0 0 1024 808 t)
  (loop for row from 0 below 808
        do (loop for col from 0 below (/ 1024 8)
                 as byte = (aref *sib-framebuffer* (+ (* row (/ 1024 8)) col))
                 do (loop for i from 0 below 8
                          doing (when (logbitp i byte)
                                  (xlib:draw-point *window* *gc-white* (+ (* col 8) i) row)))))
  (xlib:display-force-output *display*))

(declaim (inline draw-byte))
(defun draw-byte (byte address)
  (declare (type (unsigned-byte 8) byte)
           (type (unsigned-byte 17) address))
  ;;  Eventually be a smart lookup to do lines and such.
  (when *display*
    (let ((row (ldb (byte 10 7) address))
          (col (ldb (byte 7 0) address)))
      (when (< row 808)
        (dotimes (i 8)
          (if (logbitp i byte)
              (xlib:draw-point *window* *gc-white*
                               (+ (* col 8) i) row)
              (xlib:draw-point *window* *gc-black*
                               (+ (* col 8) i) row)))))))

(defun check-events ()
  (when *display*
    (xlib:display-force-output *display*)
    (loop while (xlib:event-listen *display*)
       do (xlib:event-case (*display*)
        ;;	   (:enter-notify (window)
        ;;			  (format t "~&enter notify~%") t)
               (:motion-notify (window x y code)
            ;;		   (format t "~&motion event ~d ~d~%" x y)
                       t)
               (:exposure (window count)
                          ;; CLX is stupid, so we can't put declares here.
                          ;; The following is evaluated for effect.
                      window
                          ;; CLX also doesn't declare the type of parameters
                          ;; in an event case. Lame.
                          ;; Ignore all but the last exposure event
                      (when (zerop count)
                    (draw-framebuffer))
                      t)
               (:key-press (code)
                           (let ((scancode (keycode->scancode code)))
                             (declare (type (or null (unsigned-byte 7)) scancode))
                             (when scancode
                               (setf *sib-keyboard-recv-fifo*
                                     (nconc *sib-keyboard-recv-fifo*
                                            (list (logior #x80 scancode))))))
                           t)
               (:key-release (code state)
                             state
                             (setf *sib-keyboard-recv-fifo*
                                   (nconc *sib-keyboard-recv-fifo*
                                          (list (keycode->scancode code))))
                             t)
               (:button-press (code x y state)
                                    (format t "~&buttonpress event:  code ~d, state ~d, x ~d, y ~d"
                                            code state x y)
                                    t)
               (:button-release (code x y state)
                                      (format t "~&buttonrelease event:  code ~d, state ~d, x ~d, y ~d"
                                              code state x y)
                                      t)))
    (xlib:display-force-output *display*)))

(defvar *keysym-scancode-table* (make-hash-table))

(defun keycode->scancode (code)
  (format t "X11 code is #x~x~%" code)
  (let ((keysym (xlib:keycode->keysym *display* code 0)))
    (let ((char (xlib:keysym->character *display* keysym)))
      (if (characterp char)
          (format t "~&  key is ~C " char)
          (format t "~&  key is ~A ~X " char char)))
    (format t "sym #x~x output o~o~%" keysym (gethash keysym *keysym-scancode-table* 0))
    (gethash keysym *keysym-scancode-table* 0)))

;;  Note that the alphabetic keysyms are given with their lowercase bit
;;  set, because that's the way they come in the event.
(defun init-keysym-scancode-table ()
  (clrhash *keysym-scancode-table*)
  (setf (gethash #xff6a *keysym-scancode-table*) #o001) ; SCAN-CODE-HELP
  (setf (gethash #xffe5 *keysym-scancode-table*) #o003) ; SCAN-CODE-CAPS-LOCK
  (setf (gethash #xffed *keysym-scancode-table*) #o007) ; SCAN-CODE-LEFT-HYPER
  (setf (gethash #xff0b *keysym-scancode-table*) #o016) ; SCAN-CODE-CLEAR-INPUT
  (setf (gethash #xff65 *keysym-scancode-table*) #o017) ; SCAN-CODE-UNDO
  (setf (gethash #xff57 *keysym-scancode-table*) #o020) ; SCAN-CODE-END
  (setf (gethash #xffbe *keysym-scancode-table*) #o024) ; SCAN-CODE-F1
  (setf (gethash #xffbf *keysym-scancode-table*) #o025) ; SCAN-CODE-F2
  (setf (gethash #xffc0 *keysym-scancode-table*) #o026) ; SCAN-CODE-F3
  (setf (gethash #xffc1 *keysym-scancode-table*) #o027) ; SCAN-CODE-F4
  (setf (gethash #xffeb *keysym-scancode-table*) #o032) ; SCAN-CODE-LEFT-SUPER
  (setf (gethash #xffe9 *keysym-scancode-table*) #o033) ; SCAN-CODE-LEFT-META
  (setf (gethash #xffe3 *keysym-scancode-table*) #o034) ; SCAN-CODE-LEFT-CONTROL
  (setf (gethash #xffe4 *keysym-scancode-table*) #o035) ; SCAN-CODE-RIGHT-CONTROL
  (setf (gethash #xffe8 *keysym-scancode-table*) #o036) ; SCAN-CODE-RIGHT-META
  (setf (gethash #xffec *keysym-scancode-table*) #o037) ; SCAN-CODE-RIGHT-SUPER
  (setf (gethash #xffee *keysym-scancode-table*) #o040) ; SCAN-CODE-RIGHT-HYPER
  (setf (gethash #xff1b *keysym-scancode-table*) #o043) ; SCAN-CODE-ALT
  (setf (gethash   #x31 *keysym-scancode-table*) #o044) ; SCAN-CODE-1
  (setf (gethash   #x32 *keysym-scancode-table*) #o045) ; SCAN-CODE-2
  (setf (gethash   #x33 *keysym-scancode-table*) #o046) ; SCAN-CODE-3
  (setf (gethash   #x34 *keysym-scancode-table*) #o047) ; SCAN-CODE-4
  (setf (gethash   #x35 *keysym-scancode-table*) #o050) ; SCAN-CODE-5
  (setf (gethash   #x36 *keysym-scancode-table*) #o051) ; SCAN-CODE-6
  (setf (gethash   #x37 *keysym-scancode-table*) #o052) ; SCAN-CODE-7
  (setf (gethash   #x38 *keysym-scancode-table*) #o053) ; SCAN-CODE-8
  (setf (gethash   #x39 *keysym-scancode-table*) #o054) ; SCAN-CODE-9
  (setf (gethash   #x30 *keysym-scancode-table*) #o055) ; SCAN-CODE-0
  (setf (gethash   #x2d *keysym-scancode-table*) #o056) ; SCAN-CODE-MINUS
  (setf (gethash   #x3d *keysym-scancode-table*) #o057) ; SCAN-CODE-EQUALS
  (setf (gethash   #x60 *keysym-scancode-table*) #o060) ; SCAN-CODE-BACK-QUOTE
  ;;  (setf (gethash   #x7e *keysym-scancode-table*) #o061) ; SCAN-CODE-TILDE
  (setf (gethash   #x3a *keysym-scancode-table*) #o061) ; SCAN-CODE-TILDE
  (setf (gethash #xffbd *keysym-scancode-table*) #o062) ; SCAN-CODE-KEYPAD-EQUAL
  (setf (gethash #xffab *keysym-scancode-table*) #o063) ; SCAN-CODE-KEYPAD-PLUS
  (setf (gethash #xff80 *keysym-scancode-table*) #o064) ; SCAN-CODE-KEYPAD-SPACE
  (setf (gethash #xff89 *keysym-scancode-table*) #o065) ; SCAN-CODE-KEYPAD-TAB
  (setf (gethash #xff09 *keysym-scancode-table*) #o070) ; SCAN-CODE-TAB
  (setf (gethash   #x71 *keysym-scancode-table*) #o071) ; SCAN-CODE-Q
  (setf (gethash   #x77 *keysym-scancode-table*) #o072) ; SCAN-CODE-W
  (setf (gethash   #x65 *keysym-scancode-table*) #o073) ; SCAN-CODE-E
  (setf (gethash   #x72 *keysym-scancode-table*) #o074) ; SCAN-CODE-R
  (setf (gethash   #x74 *keysym-scancode-table*) #o075) ; SCAN-CODE-T
  (setf (gethash   #x79 *keysym-scancode-table*) #o076) ; SCAN-CODE-Y
  (setf (gethash   #x75 *keysym-scancode-table*) #o077) ; SCAN-CODE-U
  (setf (gethash   #x69 *keysym-scancode-table*) #o100) ; SCAN-CODE-I
  (setf (gethash   #x6f *keysym-scancode-table*) #o101) ; SCAN-CODE-O
  (setf (gethash   #x70 *keysym-scancode-table*) #o102) ; SCAN-CODE-P
  (setf (gethash   #x28 *keysym-scancode-table*) #o103) ; SCAN-CODE-OPEN-PARENTHESIS
  (setf (gethash   #x29 *keysym-scancode-table*) #o104) ; SCAN-CODE-CLOSE-PARENTHESIS
  (setf (gethash   #x5c *keysym-scancode-table*) #o106) ; SCAN-CODE-BACKSLASH
  (setf (gethash #xff52 *keysym-scancode-table*) #o107) ; SCAN-CODE-UP-ARROW
  (setf (gethash #xffb7 *keysym-scancode-table*) #o110) ; SCAN-CODE-KEYPAD-7
  (setf (gethash #xffb8 *keysym-scancode-table*) #o111) ; SCAN-CODE-KEYPAD-8
  (setf (gethash #xffb9 *keysym-scancode-table*) #o112) ; SCAN-CODE-KEYPAD-9
  (setf (gethash #xffad *keysym-scancode-table*) #o113) ; SCAN-CODE-KEYPAD-MINUS
;;  (setf (gethash #xffff *keysym-scancode-table*) #o117) ; SCAN-CODE-RUBOUT
  (setf (gethash #xff08 *keysym-scancode-table*) #o117) ; SCAN-CODE-RUBOUT
  (setf (gethash   #x61 *keysym-scancode-table*) #o120) ; SCAN-CODE-A
  (setf (gethash   #x73 *keysym-scancode-table*) #o121) ; SCAN-CODE-S
  (setf (gethash   #x64 *keysym-scancode-table*) #o122) ; SCAN-CODE-D
  (setf (gethash   #x66 *keysym-scancode-table*) #o123) ; SCAN-CODE-F
  (setf (gethash   #x67 *keysym-scancode-table*) #o124) ; SCAN-CODE-G
  (setf (gethash   #x68 *keysym-scancode-table*) #o125) ; SCAN-CODE-H
  (setf (gethash   #x6a *keysym-scancode-table*) #o126) ; SCAN-CODE-J
  (setf (gethash   #x6b *keysym-scancode-table*) #o127) ; SCAN-CODE-K
  (setf (gethash   #x6c *keysym-scancode-table*) #o130) ; SCAN-CODE-L
  (setf (gethash   #x3B *keysym-scancode-table*) #o131) ; SCAN-CODE-SEMICOLON
  (setf (gethash   #x27 *keysym-scancode-table*) #o132) ; SCAN-CODE-APOSTROPHE
  (setf (gethash #xff0d *keysym-scancode-table*) #o133) ; SCAN-CODE-RETURN
  (setf (gethash #xff0a *keysym-scancode-table*) #o134) ; SCAN-CODE-LINE
  (setf (gethash #xff51 *keysym-scancode-table*) #o135) ; SCAN-CODE-LEFT-ARROW
  (setf (gethash #xff50 *keysym-scancode-table*) #o136) ; SCAN-CODE-HOME
  (setf (gethash #xff53 *keysym-scancode-table*) #o137) ; SCAN-CODE-RIGHT-ARROW
  (setf (gethash #xffb4 *keysym-scancode-table*) #o140) ; SCAN-CODE-KEYPAD-4
  (setf (gethash #xffb5 *keysym-scancode-table*) #o141) ; SCAN-CODE-KEYPAD-5
  (setf (gethash #xffb6 *keysym-scancode-table*) #o142) ; SCAN-CODE-KEYPAD-6
  (setf (gethash #xffac *keysym-scancode-table*) #o143) ; SCAN-CODE-KEYPAD-COMMA
  (setf (gethash #xffe1 *keysym-scancode-table*) #o147) ; SCAN-CODE-LEFT-SHIFT
  (setf (gethash   #x7a *keysym-scancode-table*) #o150) ; SCAN-CODE-Z
  (setf (gethash   #x78 *keysym-scancode-table*) #o151) ; SCAN-CODE-X
  (setf (gethash   #x63 *keysym-scancode-table*) #o152) ; SCAN-CODE-C
  (setf (gethash   #x76 *keysym-scancode-table*) #o153) ; SCAN-CODE-V
  (setf (gethash   #x62 *keysym-scancode-table*) #o154) ; SCAN-CODE-B
  (setf (gethash   #x6e *keysym-scancode-table*) #o155) ; SCAN-CODE-N
  (setf (gethash   #x6d *keysym-scancode-table*) #o156) ; SCAN-CODE-M
  (setf (gethash   #x2c *keysym-scancode-table*) #o157) ; SCAN-CODE-COMMA
  (setf (gethash   #x2e *keysym-scancode-table*) #o160) ; SCAN-CODE-PERIOD
  (setf (gethash   #x2f *keysym-scancode-table*) #o161) ; SCAN-CODE-QUESTION
  (setf (gethash #xffe2 *keysym-scancode-table*) #o162) ; SCAN-CODE-RIGHT-SHIFT
  (setf (gethash #xff54 *keysym-scancode-table*) #o165) ; SCAN-CODE-DOWN-ARROW
  (setf (gethash #xffb1 *keysym-scancode-table*) #o166) ; SCAN-CODE-KEYPAD-1
  (setf (gethash #xffb2 *keysym-scancode-table*) #o167) ; SCAN-CODE-KEYPAD-2
  (setf (gethash #xffb3 *keysym-scancode-table*) #o170) ; SCAN-CODE-KEYPAD-3
  (setf (gethash   #x20 *keysym-scancode-table*) #o173) ; SCAN-CODE-SPACE
  (setf (gethash #xffb0 *keysym-scancode-table*) #o175) ; SCAN-CODE-KEYPAD-0
  (setf (gethash #xffae *keysym-scancode-table*) #o176) ; SCAN-CODE-KEYPAD-PERIOD
  (setf (gethash #xff8d *keysym-scancode-table*) #o177) ; SCAN-CODE-KEYPAD-ENTER

  ;;  These have keysyms (break and cancel) that are close enough, but I already
  ;;  remapped Pause to Help so I don't have Break and I don't know how to generate
  ;;  Cancel, so I'll move them.
;  (setf (gethash #xff6b *keysym-scancode-table*) #o066) ; SCAN-CODE-BREAK
;  (setf (gethash #xff69 *keysym-scancode-table*) #o114) ; SCAN-CODE-ABORT (cancel)
  (setf (gethash #xffc6 *keysym-scancode-table*) #o066) ; SCAN-CODE-BREAK  (F9)
  (setf (gethash #xffc7 *keysym-scancode-table*) #o114) ; SCAN-CODE-ABORT  (F10)

  ;;  These have no obvious X keysym equivalent.  I'm assigning some of them
  ;;  arbitrarily to keys that seem useful to me.
  (setf (gethash #xffc2 *keysym-scancode-table*) #o010) ; SCAN-CODE-SYSTEM   (F5)
  (setf (gethash #xffc3 *keysym-scancode-table*) #o011) ; SCAN-CODE-NETWORK  (F6)
  (setf (gethash #xffc4 *keysym-scancode-table*) #o012) ; SCAN-CODE-STATUS   (F7)
  (setf (gethash #xffc5 *keysym-scancode-table*) #o013) ; SCAN-CODE-TERMINAL (F8)
  (setf (gethash #xffc8 *keysym-scancode-table*) #o041) ; SCAN-CODE-RESUME   (F11)
  (setf (gethash #xff55 *keysym-scancode-table*) #o015) ; SCAN-CODE-CLEAR-SCREEN  (PgUp)

  ;;  And these I just have to leave for now.
;  (DEFCONSTANT SCAN-CODE-BOLD-LOCK         #o004)
;  (DEFCONSTANT SCAN-CODE-ITAL-LOCK         #o005)
;  (DEFCONSTANT SCAN-CODE-MODE-LOCK         #o006)
;  (DEFCONSTANT SCAN-CODE-LEFT              #o021)
;  (DEFCONSTANT SCAN-CODE-MIDDLE            #o022)
;  (DEFCONSTANT SCAN-CODE-RIGHT             #o023)
;  (DEFCONSTANT SCAN-CODE-LEFT-SYMBOL       #o146)
;  (DEFCONSTANT SCAN-CODE-RIGHT-SYMBOL      #o164)
  )

(eval-when (:load-toplevel)
  (init-keysym-scancode-table))

;;; EOF
