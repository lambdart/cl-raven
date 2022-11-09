;;
;; nevermore load
;;
;; load this file to execute the emulator standalone
;;

(load #p"/home/lambdart/.local/lisp/quicklisp/local-projects/nevermore/nevermore.asd")
(asdf:operate 'asdf:load-op :nevermore)

(in-package :nevermore)

(setf *rom-file-path* #p"/home/lambdart/.local/lisp/quicklisp/local-projects/nevermore/proms/")
(load-romfiles)
;;
;; XXX there should be a better way to map formatter:device to disk files
;;
(setf *nupi-scsi0-0-disk-file* #p"/home/lambdart/.local/lisp/quicklisp/local-projects/nevermore/disks/c0-d0-hacked.dsk")
(setf *nupi-scsi0-1-disk-file* #p"/home/lambdart/.local/lisp/quicklisp/local-projects/nevermore/disks/c0-d1.dsk")
(setf *nupi-scsi2-0-disk-file* #p"/home/lambdart/.local/lisp/quicklisp/local-projects/nevermore/disks/c2-d0.dsk")
;; (setf *nupi-scsi2-1-disk-file* #p"/home/bowb/Lisp/nevermore/nevermore/c2-d1.dsk")

(start-x)
(init-window)

(in-package :raven)
(microengine-initialize)

(microengine-run-to -1)

;; (nevermore::finish-x)
