;;; -*- lisp -*-
;;;
;;; nevermore.asd
;;;
;;; system definition for nevermore
;;;

(asdf:defsystem :nevermore
    :depends-on (:clx)
    :serial t
    :components
  ((:file "nevermore-package")
   (:file "nevermore-variables")
   (:file "raven-variables")
   (:file "nevermore-nubus")
   (:file "nupi-nubus")
   (:file "sib-variables")
   (:file "sib-mouse-diags")
   (:file "sib-graphics")
   (:file "sib-xlib")
   (:file "sib-graphics-nubus")
   (:file "sib-keyboard")
   (:file "sib-timer")
   (:file "sib-nubus")
   (:file "sib-event")
   (:file "memory-nubus")
   (:file "raven-cpu-nubus")
   (:file "raven-mmu")
   (:file "raven-functional")
   (:file "raven-memory")
   (:file "raven-disassembler")
   (:file "raven-microengine")
   (:file "romload")))

;;; EOF
