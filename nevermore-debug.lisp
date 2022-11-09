;;
;; nevermore debugging facilities and codes.  not really for loading.
;;

(in-package :raven)

(boot)
(reboot)
(microengine-step-trace)

(dump-active-microstack)

(raven::microengine-run-to -1)
(microengine-step)
(nevermore::microengine-force-halt)

;;
;; now random stuff for initializing the emulator
;;
(load "/home/bowb/Lisp/nevermore/nevermore/nevermore.asd")
(asdf:operate 'asdf:load-op :nevermore)

(in-package :nevermore)

(setf *rom-file-path* #p"~/Lisp/meroko/svn/proms/")
(load-romfiles)

;;
;; XXX there should be a better way to map formatter:device to disk files
;;
(setf *nupi-scsi0-0-disk-file* #p"/home/bowb/Lisp/nevermore/nevermore/c0-d0.dsk")
(setf *nupi-scsi0-1-disk-file* #p"/home/bowb/Lisp/nevermore/nevermore/c0-d1.dsk")
(setf *nupi-scsi2-0-disk-file* #p"/home/bowb/Lisp/nevermore/nevermore/c2-d0.dsk")
(setf *nupi-scsi2-1-disk-file* #p"/home/bowb/Lisp/nevermore/nevermore/c2-d1.dsk")

(start-x)
(init-window)

(in-package :raven)
(microengine-initialize)

(microengine-run-to -1)

(nevermore::finish-x)
