;;;
;;; nupi-nubus.lisp
;;;
;;; NuBus interface to the NuPI board.
;;;

(in-package :nevermore)

(declaim (type (simple-array (unsigned-byte 16) (*)) *nupi-config-rom*))
(defvar *nupi-config-rom* (make-array '(#x2000) :element-type '(unsigned-byte 16)
                                      :initial-element 0))

(defvar *nupi-scsi0-0-disk-file*
  #p"/home/nyef/oldhome/src/lisp/lispm_stuff/aek/explorerI_diskDumps/explScsi0Dr0.dsk")
(defvar *nupi-scsi0-1-disk-file*
  #p"/home/nyef/oldhome/src/lisp/lispm_stuff/aek/explorerI_diskDumps/explScsi0Dr1.dsk")
(defvar *nupi-scsi2-0-disk-file*
  #p"/home/nyef/oldhome/src/lisp/lispm_stuff/aek/explorerI_diskDumps/explScsi2.dsk")
(defvar *nupi-scsi2-1-disk-file*
  #p"")

(defun nupi-dump-rqb-header ()
  (let ((old-memory-data (aref *memory-data*))
        (*page-fault* *page-fault*)
        (*memory-busy* *memory-busy*)
        (*nubus-error* *nubus-error*))
 ;;   (dotimes (i 8)
 ;;     (start-nubus-read (+ old-memory-data (ash i 2)))
 ;;     (format t "Word ~X: ~X~%" i (aref *memory-data*)))
    (setf (aref *memory-data*) old-memory-data)))

(defun nupi-busmaster-read (address)
  (let ((old-memory-data (aref *memory-data*))
        (*page-fault* *page-fault*)
        (*memory-busy* *memory-busy*)
        (*nubus-error* *nubus-error*))
    (start-nubus-read address)
    (let ((retval (aref *memory-data*)))
      (setf (aref *memory-data*) old-memory-data)
      retval)))

(defun nupi-busmaster-write (address data)
  (let ((old-memory-data (aref *memory-data*))
        (*page-fault* *page-fault*)
        (*memory-busy* *memory-busy*)
        (*nubus-error* *nubus-error*))
    (setf (aref *memory-data*) data)
    (start-nubus-write address)
    (setf (aref *memory-data*) old-memory-data))
  (values))

(defun nupi-complete-request (rqb-address rqb-command-word)
  (declare (ignorable rqb-command-word))
  ;; FIXME: Should handle event-posting.
  (nupi-busmaster-write (+ rqb-address 4) #x40000000)
  (when (logbitp 23 rqb-command-word)
    (format t "Post!~%")
    (nupi-busmaster-write (nupi-busmaster-read (+ rqb-address (ash 5 2))) #xffffffff)
    #+nil    (break)))

(defun nupi-handle-command-nupi-status (rqb-address rqb-command-word)
  (let ((buffer-address (nupi-busmaster-read (+ rqb-address 8)))
        (transfer-length (nupi-busmaster-read (+ rqb-address 12))))
    (declare (ignorable transfer-length))
    (nupi-busmaster-write buffer-address 0)
    (nupi-busmaster-write (+ buffer-address 4) 0)
    (dotimes (i 7)
      (nupi-busmaster-write (+ buffer-address 8 (ash i 2)) #x10000000))
    ;; Same hack as in exploiter: First formatter, first drive only.
    ;; Minor change: The drive is write-protected.
    (nupi-busmaster-write (+ buffer-address (ash (+ 2 0) 2)) 0)
    (nupi-busmaster-write (+ buffer-address (ash  9 2)) #x41000000)
    (nupi-busmaster-write (+ buffer-address (ash 10 2)) #x10000000)

    ;; Enhanced hack: Enable first drive of formatters 1 and 3.
    (nupi-busmaster-write (+ buffer-address (ash (+ 2 1) 2)) 0)
    (nupi-busmaster-write (+ buffer-address (ash 11 2)) #x41000000)
    (nupi-busmaster-write (+ buffer-address (ash 12 2)) #x10000000)
    (nupi-busmaster-write (+ buffer-address (ash (+ 2 3) 2)) 0)
    (nupi-busmaster-write (+ buffer-address (ash 15 2)) #x41000000)
    (nupi-busmaster-write (+ buffer-address (ash 16 2)) #x10000000))
  (nupi-complete-request rqb-address rqb-command-word)
#+nil  (break))

(defun nupi-handle-command-drive-status (rqb-address rqb-command-word)
  (let ((buffer-address (nupi-busmaster-read (+ rqb-address 8)))
        (transfer-length (nupi-busmaster-read (+ rqb-address 12))))
    (declare (ignorable transfer-length))
    (nupi-busmaster-write buffer-address #x41000000)
    #+nil(nupi-busmaster-write (+ buffer-address (ash 10 2)) #x1acf2)
    #+nil(nupi-busmaster-write (+ buffer-address (ash 11 2)) #x400))
  (nupi-complete-request rqb-address rqb-command-word)
#+nil  (break))

;;
;; XXX this doesn't really seem consistent with the NUPI documentation,
;; XXX which claims (byte 3 3) is the formatter number, and (byte 1 0)
;; XXX is the device on the formatter.
;;
(defun nupi-disk-file-for-unit (rqb-command-word)
  (let* ((unit-number (ldb (byte 8 0) rqb-command-word))
         (disk-number (ldb (byte 3 3) unit-number))
         (device-number (ldb (byte 1 0) unit-number)))
    (format t "disk unit ~d.~d~%" disk-number device-number)
    (cond
      ((= 0 disk-number) *nupi-scsi0-0-disk-file*)
      ((= 1 disk-number) *nupi-scsi0-1-disk-file*)
      ((= 2 disk-number) *nupi-scsi2-1-disk-file*)
      ((= 3 disk-number) *nupi-scsi2-0-disk-file*))))

(defun nupi-handle-command-drive-read (rqb-address rqb-command-word)
  (with-open-file (diskfile (nupi-disk-file-for-unit rqb-command-word)
                            :direction :input :element-type '(unsigned-byte 8))
    (labels ((block-transfer (buffer-address transfer-length block-address)
             (file-position diskfile (* #x400 block-address))
             (dotimes (i (ash transfer-length -2))
               (let* ((b0 (read-byte diskfile))
                      (b1 (read-byte diskfile))
                      (b2 (read-byte diskfile))
                      (b3 (read-byte diskfile))
                      (disk-word
                       (dpb b3 (byte 8 24)
                            (dpb b2 (byte 8 16)
                                 (dpb b1 (byte 8 8) b0)))))
                 (nupi-busmaster-write (+ buffer-address (* i 4))
                                       disk-word))))
           (scatter-transfer (scatter-buffer-address total-length start-block)
             (loop for scatter-address = scatter-buffer-address
                   then (+ scatter-address 8)
                   with buffer-address
                   with buffer-length
                   with length-remaining = total-length
                   with current-block = start-block
                   do (setf buffer-address
                            (nupi-busmaster-read scatter-address))
                   do (setf buffer-length
                            (nupi-busmaster-read (+ scatter-address 4)))
;;                   do (format t "scatter block at ~8,'0X for ~X.~%"
;;                             buffer-address buffer-length)
                   do (block-transfer buffer-address buffer-length
                                      current-block)
                   do (incf current-block (ash buffer-length -10))
                   do (decf length-remaining buffer-length)
                   until (zerop length-remaining))))
      (let ((buffer-address (nupi-busmaster-read (+ rqb-address 8)))
            (transfer-length (nupi-busmaster-read (+ rqb-address 12)))
            (block-address (nupi-busmaster-read (+ rqb-address 16))))
        (if (logbitp 22 rqb-command-word)
            (scatter-transfer buffer-address transfer-length block-address)
            (block-transfer buffer-address transfer-length block-address)))))
  (nupi-complete-request rqb-address rqb-command-word)
  #+nil  (break))

(defun nupi-handle-command-drive-write (rqb-address rqb-command-word)
  (with-open-file (diskfile (nupi-disk-file-for-unit rqb-command-word)
                            :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :overwrite)
    (labels ((block-transfer (buffer-address transfer-length block-address)
             (file-position diskfile (* #x400 block-address))
             (dotimes (i (ash transfer-length -2))
               (let ((data-word (nupi-busmaster-read (+ buffer-address
                                                        (* i 4)))))
                 (write-byte (ldb (byte 8 0) data-word) diskfile)
                 (write-byte (ldb (byte 8 8) data-word) diskfile)
                 (write-byte (ldb (byte 8 16) data-word) diskfile)
                 (write-byte (ldb (byte 8 24) data-word) diskfile))))
           (scatter-transfer (scatter-buffer-address total-length start-block)
             (loop for scatter-address = scatter-buffer-address
                   then (+ scatter-address 8)
                   with buffer-address
                   with buffer-length
                   with length-remaining = total-length
                   with current-block = start-block
                   do (setf buffer-address
                            (nupi-busmaster-read scatter-address))
                   do (setf buffer-length
                            (nupi-busmaster-read (+ scatter-address 4)))
                   do (format t "scatter block at ~8,'0X for ~X.~%"
                              buffer-address buffer-length)
                   do (block-transfer buffer-address buffer-length
                                      current-block)
                   do (incf current-block (ash buffer-length -10))
                   do (decf length-remaining buffer-length)
                   until (zerop length-remaining))))
      (let ((buffer-address (nupi-busmaster-read (+ rqb-address 8)))
            (transfer-length (nupi-busmaster-read (+ rqb-address 12)))
            (block-address (nupi-busmaster-read (+ rqb-address 16))))
        (if (logbitp 22 rqb-command-word)
            (scatter-transfer buffer-address transfer-length block-address)
            (block-transfer buffer-address transfer-length block-address)))))
  (nupi-complete-request rqb-address rqb-command-word)
  #+nil  (break))

(defun nupi-command-write ()
  (format t "NuPI: Command write, RQB: ~X~%" (aref *memory-data*))
  (nupi-dump-rqb-header)
  (let* ((rqb-address (aref *memory-data*))
         (rqb-command-word (nupi-busmaster-read rqb-address)))
    (cond ((= #x82 (ldb (byte 8 24) rqb-command-word))
           (nupi-handle-command-nupi-status rqb-address rqb-command-word))
          ((= #x02 (ldb (byte 8 24) rqb-command-word))
           (nupi-handle-command-drive-status rqb-address rqb-command-word))
          ((= #x10 (ldb (byte 8 24) rqb-command-word))
           (nupi-complete-request rqb-address rqb-command-word)
#+nil      (break))
          ((= #x81 (ldb (byte 8 24) rqb-command-word))
           (nupi-complete-request rqb-address rqb-command-word)
#+nil      (break))
          ((= #x12 (ldb (byte 8 24) rqb-command-word))
           (nupi-handle-command-drive-read rqb-address rqb-command-word))
          ((= #x13 (ldb (byte 8 24) rqb-command-word))
           (nupi-handle-command-drive-write rqb-address rqb-command-word))
          (t (break)))))

(defun nupi-nubus-read (slot address width)
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address)
           (ignorable slot width))
  (cond ((= (logand address #xffc000) #xffc000)
         (setf *inhibit-nubus-trace* t)
         (setf (aref *memory-data*)
               (dpb (ldb (byte 8 (ash (logand 1 address) 3))
                         (aref *nupi-config-rom*
                               (logxor 1 (logand #x1fff (ash address -1)))))
                    (byte 8 (* 8 (logand 3 address))) 0)))

        ;; "Flag register"? (upper half?)
        ((= address #xd40002)
         (setf (aref *memory-data*) 0))

        ;; Configuration register
        ((= address #xe0000b)
         (setf (aref *memory-data*) 0))

        ;; Configuration register, part II
        ((= address #xe0000a)
         )

        (t (setf *nubus-error* t)))
  (values))

(defun nupi-nubus-write (slot address width)
  (declare (type (unsigned-byte 8) slot)
           (type (unsigned-byte 24) address)
           (ignorable slot width))
  (cond ((= address #xe00004)
         (nupi-command-write))

        ;; Configuration register
        ((= address #xe0000b)
         )

        ;; Configuration register, part II
        ((= address #xe0000a)
         )

        (t (setf *nubus-error* t)))

  (values))

;;; EOF
