;;;
;;; An attempt at a disassembler for the Explorer I (Raven) microcode
;;;

(in-package :raven)


(defvar *functional-destinations* nil "A plist for functional destination names")
(defvar *functional-sources* nil "A plist for functional source names")
(defvar *abj-values* nil "A plist for abbreviated jump codes")
(defvar *alu-operations* nil "A plist for ALU opcodes")
(defvar *q-control-values* nil "A plist for ALU Q-Control values")
(defvar *output-bus-values* nil "A plist for Output-Bus-Control values")
(defvar *byte-operations* nil "A plist for BYTE opcodes")
(defvar *jump-operations* nil "A plist for JUMP opcodes")
(defvar *condition-values* nil "A plist for Condition field values")
(defvar *condition-sources* nil "A plist for M- and A- sources used in conditions")
(defvar *alu-opc-sources* nil "A plist for M- and A- sources used in ALU ops")
(defvar *dispatch-operations* nil "A plsit for DISPATCH opcodes")

(defvar *m-memory-symbols* nil "A plist for M-Memory names")
(defvar *a-memory-symbols* nil "A plist for A-Memory names")
(defvar *i-memory-symbols* nil "A plist for I-Memory names")


(defmacro defsymbol (symbol index value)
  `(setf (getf ,symbol ,index) ',value))


(defsymbol *functional-destinations* #o0  nop)
(defsymbol *functional-destinations* #o1  location-counter)
(defsymbol *functional-destinations* #o2  mcr)
(defsymbol *functional-destinations* #o3  micro-stack-pointer)
(defsymbol *functional-destinations* #o4  micro-stack-data)
(defsymbol *functional-destinations* #o5  micro-stack-data-push)
(defsymbol *functional-destinations* #o6  imod-low)
(defsymbol *functional-destinations* #o7  imod-high)
(defsymbol *functional-destinations* #o10 mib)
(defsymbol *functional-destinations* #o17 test-synch)
(defsymbol *functional-destinations* #o20 vma)
(defsymbol *functional-destinations* #o21 vma-write-map-level-1)
(defsymbol *functional-destinations* #o22 vma-write-map-level-2-control)
(defsymbol *functional-destinations* #o23 vma-write-map-level-2-address)
(defsymbol *functional-destinations* #o24 vma-start-read)
(defsymbol *functional-destinations* #o25 vma-start-write)
(defsymbol *functional-destinations* #o26 vma-start-unmapped-read)
(defsymbol *functional-destinations* #o27 vma-start-unmapped-write)
(defsymbol *functional-destinations* #o30 md)
(defsymbol *functional-destinations* #o31 md-write-map-level-1)
(defsymbol *functional-destinations* #o32 md-write-map-level-2-control)
(defsymbol *functional-destinations* #o33 md-write-map-level-2-address)
(defsymbol *functional-destinations* #o34 md-start-read)
(defsymbol *functional-destinations* #o35 md-start-write)
(defsymbol *functional-destinations* #o36 md-start-unmapped-read)
(defsymbol *functional-destinations* #o37 md-start-unmapped-write)
(defsymbol *functional-destinations* #o40 c-pdl-buffer-pointer)
(defsymbol *functional-destinations* #o41 c-pdl-buffer-index)
(defsymbol *functional-destinations* #o44 c-pdl-buffer-pointer-push)
(defsymbol *functional-destinations* #o45 c-pdl-buffer-index-increment)
(defsymbol *functional-destinations* #o50 pdl-buffer-pointer)
(defsymbol *functional-destinations* #o51 pdl-buffer-index)
(defsymbol *functional-destinations* #o66 vma-start-unmapped-byte-read)
(defsymbol *functional-destinations* #o67 vma-start-unmapped-byte-write)
(defsymbol *functional-destinations* #o76 md-start-unmapped-byte-read)
(defsymbol *functional-destinations* #o77 md-start-unmapped-byte-write)

(defsymbol *functional-sources* #o0  vma)
(defsymbol *functional-sources* #o1  q-r)
(defsymbol *functional-sources* #o2  mib-argument-offset-field)
(defsymbol *functional-sources* #o3  micro-stack-pointer)
(defsymbol *functional-sources* #o4  mcr)
(defsymbol *functional-sources* #o5  location-counter)
(defsymbol *functional-sources* #o6  memory-map-level-2-address)
(defsymbol *functional-sources* #o7  read-i-arg)
(defsymbol *functional-sources* #o10 memory-map-level-1)
(defsymbol *functional-sources* #o11 memory-map-level-2-control)
(defsymbol *functional-sources* #o12 mib)
(defsymbol *functional-sources* #o13 mib-branch-offset-field)
(defsymbol *functional-sources* #o20 micro-stack-data)
(defsymbol *functional-sources* #o21 micro-stack-data-pop)
(defsymbol *functional-sources* #o22 md)
(defsymbol *functional-sources* #o40 c-pdl-buffer-pointer)
(defsymbol *functional-sources* #o41 c-pdl-buffer-index)
(defsymbol *functional-sources* #o44 c-pdl-buffer-pointer-pop)
(defsymbol *functional-sources* #o45 c-pdl-buffer-index-decrement)
(defsymbol *functional-sources* #o50 pdl-buffer-pointer)
(defsymbol *functional-sources* #o51 pdl-buffer-index)
(defsymbol *functional-sources* #o54 pdl-buffer-pointer-pop)
(defsymbol *functional-sources* #o55 pdl-buffer-index-decrement)


(defsymbol *abj-values* #o0 nil) ;; shouldn't show up in output
(defsymbol *abj-values* #o1 and-skip)
(defsymbol *abj-values* #o2 and-call-illop)
(defsymbol *abj-values* #o3 and-call-trap)
(defsymbol *abj-values* #o4 and-call-buserr)
(defsymbol *abj-values* #o5 unused-abj-value)
(defsymbol *abj-values* #o6 and-popj)
(defsymbol *abj-values* #o7 and-popj-xct-next)


(defsymbol *alu-operations* #o0  setz)
(defsymbol *alu-operations* #o1  and)
(defsymbol *alu-operations* #o2  andca)
(defsymbol *alu-operations* #o3  setm)
(defsymbol *alu-operations* #o4  andcm)
(defsymbol *alu-operations* #o5  seta)
(defsymbol *alu-operations* #o6  xor)
(defsymbol *alu-operations* #o7  ior)
(defsymbol *alu-operations* #o10 andcb)
(defsymbol *alu-operations* #o11 eqv)
(defsymbol *alu-operations* #o12 setca)
(defsymbol *alu-operations* #o13 orca)
(defsymbol *alu-operations* #o14 setcm)
(defsymbol *alu-operations* #o15 orcm)
(defsymbol *alu-operations* #o16 orcb)
(defsymbol *alu-operations* #o17 seto)

;;; now we do the alu symbols which probably need post-processing
(defsymbol *alu-operations* #o31 add)
(defsymbol *alu-operations* #o36 m-a-1)
(defsymbol *alu-operations* #o37 m+m)
(defsymbol *alu-operations* #o34 m+)

(defsymbol *alu-operations* #o20 opc-mul)
(defsymbol *alu-operations* #o21 opc-mul-last)
(defsymbol *alu-operations* #o22 opc-div)
(defsymbol *alu-operations* #o23 opc-div-first)
(defsymbol *alu-operations* #o24 opc-div-corr)


(defsymbol *q-control-values* #o0 nil)
(defsymbol *q-control-values* #o1 shift-q-left)
(defsymbol *q-control-values* #o2 shift-q-right)
(defsymbol *q-control-values* #o3 load-q)

(defsymbol *output-bus-values* #o0 output-selector-a-bus)
(defsymbol *output-bus-values* #o1 output-selector-r-bus)
(defsymbol *output-bus-values* #o2 output-selector-a-bus2)
(defsymbol *output-bus-values* #o3 output-selector-normal)
(defsymbol *output-bus-values* #o4 output-selector-leftshift-1)
(defsymbol *output-bus-values* #o5 output-selector-rightshift-1)
(defsymbol *output-bus-values* #o6 output-selector-sign-extend)
(defsymbol *output-bus-values* #o7 output-selector-mirror)


(defsymbol *byte-operations* #o0 bogus-byte-op)
(defsymbol *byte-operations* #o1 ldb)
(defsymbol *byte-operations* #o2 selective-deposit)
(defsymbol *byte-operations* #o3 dpb)


(defsymbol *jump-operations* #o0 jump-xct-next)
(defsymbol *jump-operations* #o1 jump)
(defsymbol *jump-operations* #o2 call-xct-next)
(defsymbol *jump-operations* #o3 call)
(defsymbol *jump-operations* #o4 popj-xct-next)
(defsymbol *jump-operations* #o5 popj)
(defsymbol *jump-operations* #o6 jump2-xct-next)
(defsymbol *jump-operations* #o7 jump2)


(defsymbol *condition-values* #o00 if-bit-set)
(defsymbol *condition-values* #o40 if-bit-clear)
(defsymbol *condition-values* #o01 if-less)
(defsymbol *condition-values* #o41 if-greater-or-equal)
(defsymbol *condition-values* #o02 if-less-or-equal)
(defsymbol *condition-values* #o42 if-greater)
(defsymbol *condition-values* #o03 if-not-equal)
(defsymbol *condition-values* #o43 if-equal)
(defsymbol *condition-values* #o04 if-page-fault)
(defsymbol *condition-values* #o44 if-not-page-fault)
(defsymbol *condition-values* #o05 if-page-fault-or-interrupt-pending)
(defsymbol *condition-values* #o45 if-not-page-fault-or-interrupt-pending)
(defsymbol *condition-values* #o06 if-page-fault-or-interrupt-pending-or-sequence-break)
(defsymbol *condition-values* #o46 if-not-page-fault-or-interrupt-pending-or-sequence-break)
(defsymbol *condition-values* #o07 always)
(defsymbol *condition-values* #o47 never)
(defsymbol *condition-values* #o10 if-tag-not-equal)
(defsymbol *condition-values* #o50 if-tag-equal)
(defsymbol *condition-values* #o11 if-not-memory-busy)
(defsymbol *condition-values* #o51 if-memory-busy)
(defsymbol *condition-values* #o12 if-q0)
(defsymbol *condition-values* #o52 if-not-q0)
(defsymbol *condition-values* #o13 if-nubus-error)
(defsymbol *condition-values* #o53 if-not-nubus-error)
(defsymbol *condition-values* #o14 if-not-fixnum-overflow)
(defsymbol *condition-values* #o54 if-fixnum-overflow)
(defsymbol *condition-values* #o15 if-negative)
(defsymbol *condition-values* #o55 if-positive)
(defsymbol *condition-values* #o16 if-no-interrupt-pending)
(defsymbol *condition-values* #o56 if-interrupt-pending)
(defsymbol *condition-values* #o20 if-in-class)
(defsymbol *condition-values* #o60 if-not-in-class)


;;; source information is stored as a bitfield. Low bit is A source, 2 bit is M source.
(defsymbol *condition-sources* 'if-bit-set 2)
(defsymbol *condition-sources* 'if-bit-clear 2)
(defsymbol *condition-sources* 'if-less 3)
(defsymbol *condition-sources* 'if-greater-or-equal 3)
(defsymbol *condition-sources* 'if-less-or-equal 3)
(defsymbol *condition-sources* 'if-greater 3)
(defsymbol *condition-sources* 'if-not-equal 3)
(defsymbol *condition-sources* 'if-equal 3)
(defsymbol *condition-sources* 'if-page-fault 0)
(defsymbol *condition-sources* 'if-not-page-fault 0)
(defsymbol *condition-sources* 'if-page-fault-or-interrupt-pending 0)
(defsymbol *condition-sources* 'if-not-page-fault-or-interrupt-pending 0)
(defsymbol *condition-sources* 'if-page-fault-or-interrupt-pending-or-sequence-break 0)
(defsymbol *condition-sources* 'if-not-page-fault-or-interrupt-pending-or-sequence-break 0)
(defsymbol *condition-sources* 'always 0)
(defsymbol *condition-sources* 'never 0)
(defsymbol *condition-sources* 'if-tag-not-equal 3)
(defsymbol *condition-sources* 'if-tag-equal 3)
(defsymbol *condition-sources* 'if-not-memory-busy 0)
(defsymbol *condition-sources* 'if-memory-busy 0)
(defsymbol *condition-sources* 'if-q0 0)
(defsymbol *condition-sources* 'if-not-q0 0)
(defsymbol *condition-sources* 'if-nubus-error 0)
(defsymbol *condition-sources* 'if-not-nubus-error 0)
(defsymbol *condition-sources* 'if-not-fixnum-overflow 0)
(defsymbol *condition-sources* 'if-fixnum-overflow 0)
(defsymbol *condition-sources* 'if-negative 1) ;; FIXME: Double-check
(defsymbol *condition-sources* 'if-positive 1) ;; FIXME: double-check
(defsymbol *condition-sources* 'if-no-interrupt-pending 0)
(defsymbol *condition-sources* 'if-interrupt-pending 0)
(defsymbol *condition-sources* 'if-in-class 2)
(defsymbol *condition-sources* 'if-not-in-class 2)


(defsymbol *alu-opc-sources* 'setz 0)
(defsymbol *alu-opc-sources* 'and 3)
(defsymbol *alu-opc-sources* 'andca 3)
(defsymbol *alu-opc-sources* 'setm 2)
(defsymbol *alu-opc-sources* 'andcm 3)
(defsymbol *alu-opc-sources* 'seta 1)
(defsymbol *alu-opc-sources* 'xor 3)
(defsymbol *alu-opc-sources* 'ior 3)
(defsymbol *alu-opc-sources* 'andcb 3)
(defsymbol *alu-opc-sources* 'eqv 3)
(defsymbol *alu-opc-sources* 'setca 3)
(defsymbol *alu-opc-sources* 'orca 3)
(defsymbol *alu-opc-sources* 'setcm 2)
(defsymbol *alu-opc-sources* 'orcm 3)
(defsymbol *alu-opc-sources* 'orcb 3)
(defsymbol *alu-opc-sources* 'seto 0)
(defsymbol *alu-opc-sources* 'add 3)
(defsymbol *alu-opc-sources* 'm-a-1 3)
(defsymbol *alu-opc-sources* 'm+m 2)
(defsymbol *alu-opc-sources* 'm+ 2)
(defsymbol *alu-opc-sources* 'opc-mul 3)
(defsymbol *alu-opc-sources* 'opc-mul-last 3)
(defsymbol *alu-opc-sources* 'opc-div 3)
(defsymbol *alu-opc-sources* 'opc-div-first 3)
(defsymbol *alu-opc-sources* 'opc-div-corr 3)


(defsymbol *dispatch-operations* 0 dispatch)
(defsymbol *dispatch-operations* 1 read-dispatch-ram)
(defsymbol *dispatch-operations* 2 write-dispatch-ram)
(defsymbol *dispatch-operations* 3 dispatch-error)


(defun get-m-memory-name (address)
  (or (getf *m-memory-symbols* address)
      (read-from-string (format nil "M-#x~X" address))))

(defun get-a-memory-name (address)
  (if (> 64 address)
      (get-m-memory-name address)
    (or (getf *a-memory-symbols* address)
    (read-from-string (format nil "A-#x~X" address)))))

(defun get-i-memory-name (address)
  (or (getf *i-memory-symbols* address)
      (read-from-string (format nil "L-#x~X" address))))

(defun clear-symbol-table ()
  (setf *m-memory-symbols* nil)
  (setf *a-memory-symbols* nil)
  (setf *i-memory-symbols* nil))


(defun decode-destination (opword)
  (if (= 1 (ldb (byte 1 31) opword))
      `(,(get-a-memory-name (ldb (byte 10 19) opword)))
    (let ((mdest (get-m-memory-name (ldb (byte 6 19) opword)))
      (mfunc (getf *functional-destinations* (ldb (byte 6 25) opword))))
      (when (null mfunc)
    (format T "Bogus functional destination~%"))
      (if (eq 'nop mfunc)
      (list mdest)
    (list mdest mfunc)))))

(defun decode-jump-micro-pc (opword)
  (get-i-memory-name (ldb (byte 14 18) opword)))


(defmacro bitfield-decode-lookup (name plist field)
  `(defun ,name (opword)
     (getf ,plist (ldb ,field opword))))

(bitfield-decode-lookup decode-abj         *abj-values*          (byte 3 51))
(bitfield-decode-lookup decode-alu-op      *alu-operations*      (byte 5 3))
(bitfield-decode-lookup decode-q-control   *q-control-values*    (byte 2 0))
(bitfield-decode-lookup decode-output-bus  *output-bus-values*   (byte 3 16))
(bitfield-decode-lookup decode-byte-op     *byte-operations*     (byte 2 17))
(bitfield-decode-lookup decode-jump-op     *jump-operations*     (byte 3 5))
(bitfield-decode-lookup decode-dispatch-op *dispatch-operations* (byte 2 8))

;; FIXME: Something needs doing about tag-class conditions with addresses
(bitfield-decode-lookup decode-condition *condition-values* (byte 6 10))


(defun decode-byte-field (opword)
  (list 'byte-field
    (ldb (byte 5 5) opword)
    (ldb (byte 5 0) opword)))


(defun decode-m-source (opword)
  (if (= 1 (ldb (byte 1 48)
        opword))
      (getf *functional-sources* (ldb (byte 6 42)
                      opword))
    (get-m-memory-name (ldb (byte 6 42)
                opword))))


(defun decode-a-source (opword)
  (get-a-memory-name (ldb (byte 10 32) opword)))


(defun final-cleanup (code)
  (remove-if #'null code))

(defun disassemble-alu-instruction (opword)
  (let ((code (list (decode-destination opword)
            (if (= 1 (ldb (byte 1 8) opword)) 'typed-alu-op)
            (decode-alu-op opword)
            (if (= 1 (ldb (byte 1 2) opword)) 'alu-carry-in-one)
            (decode-condition opword)
            (decode-q-control opword)
            (decode-output-bus opword)
            (decode-m-source opword)
            (decode-a-source opword)
            (decode-abj opword)))
    (sources-used 3))

    (let ((opc-sources (getf *alu-opc-sources* (elt code 2))))
      (if opc-sources
      (setf sources-used opc-sources)))

    (let ((cond-sources (getf *condition-sources* (elt code 4))))
      (if cond-sources
      (setf sources-used (logior sources-used cond-sources))))

    (if (and (eq (elt code 2) 'opc-mul)
         (eq (elt code 5) 'shift-q-right)
         (eq (elt code 6) 'output-selector-rightshift-1))
    (progn
      (setf (elt code 2) 'multiply-step)
      (setf (elt code 5) nil)
      (setf (elt code 6) nil)))

    (if (eq (elt code 2) 'opc-mul-last)
    (setf (elt code 2) 'multiply-step-last))

    (if (and (eq (elt code 2) 'opc-div-first)
         (eq (elt code 5) 'shift-q-left)
         (eq (elt code 6) 'output-selector-leftshift-1))
    (progn
      (setf (elt code 2) 'divide-first-step)
      (setf (elt code 5) nil)
      (setf (elt code 6) nil)))

    (if (and (eq (elt code 2) 'opc-div)
         (eq (elt code 5) 'shift-q-left)
         (eq (elt code 6) 'output-selector-leftshift-1))
    (progn
      (setf (elt code 2) 'divide-step)
      (setf (elt code 5) nil)
      (setf (elt code 6) nil)))

    (if (and (eq (elt code 2) 'opc-div-last)
         (eq (elt code 5) 'shift-q-left))
    (progn
      (setf (elt code 2) 'divide-last-step)
      (setf (elt code 5) nil)))

    (if (eq (elt code 2) 'opc-div-corr)
    (setf (elt code 2) 'divide-remainder-correction-step))

    (if (and (eq (elt code 2) 'm-a-1)
         (eq (elt code 3) 'alu-carry-in-one))
    (progn
      (setf (elt code 2) 'sub)
      (setf (elt code 3) nil)))

    (if (and (eq (elt code 2) 'add)
         (eq (elt code 3) 'alu-carry-in-one))
    (progn
      (setf (elt code 2) 'm+a+1)
      (setf (elt code 3) nil)))

    (if (and (eq (elt code 2) 'm+m)
         (eq (elt code 3) 'alu-carry-in-one))
    (progn
      (setf (elt code 2) 'm+m+1)
      (setf (elt code 3) nil)))

    (if (and (eq (elt code 2) 'm+)
         (eq (elt code 3) 'alu-carry-in-one))
    (progn
      (setf (elt code 2) 'm+1)
      (setf (elt code 3) nil)))

    ;; FIXME: Strip out SETM and SETA ops?
    ;; FIXME: Move LOAD-Q to destinations as Q-R?

    (if (and (eq (elt code 4) 'if-in-class)
         (= 1 (ldb (byte 1 9) opword)))
    (setf (elt code 4) 'write-m-tag-classifier))

    (if (eq (elt code 4) 'always)
    (setf (elt code 4) nil))

    (if (eq (elt code 6) 'output-selector-normal)
    (setf (elt code 6) nil))

    (if (eq 0 (logand 2 sources-used))
    (setf (elt code 7) nil))

    (if (eq 0 (logand 1 sources-used))
    (setf (elt code 8) nil))

    (if (equal code '((m-0) nil setz nil nil nil nil nil nil nil))
    (setf code '(no-op)))

    (final-cleanup code)))

(defun disassemble-byte-instruction (opword)
  (let ((code (list (decode-destination opword)
            (decode-byte-op opword)
            (decode-condition opword)
            (if (= 1 (ldb (byte 1 16) opword)) 'rotate-right)
            (decode-byte-field opword)
            (decode-m-source opword)
            (decode-a-source opword)
            (decode-abj opword))))
    (if (eq (elt code 2) 'always)
    (setf (elt code 2) nil))
    (final-cleanup code)))

(defun disassemble-jump-instruction (opword)
  (let ((code (list (decode-jump-op opword)
            (if (= 1 (ldb (byte 1 8) opword)) 'read-control-store)
            (if (= 1 (ldb (byte 1 9) opword)) 'write-control-store)
            (decode-jump-micro-pc opword)
            (decode-condition opword)
            (if (= 1 (ldb (byte 1 16) opword)) 'rotate-right)
            (list 'byte-field 1 (ldb (byte 5 0) opword))
            (decode-m-source opword)
            (decode-a-source opword)
            (decode-abj opword)))
    (sources-used 0))

    (let ((jump-op (elt code 0)))
      (if (and (or (eq jump-op 'popj-xct-next)
           (eq jump-op 'popj))
           (eq (elt code 3) 'l-0))
      (setf (elt code 3) nil)))

    (let ((condition (elt code 4)))
      (setf sources-used (getf *condition-sources* condition))
      (if (null sources-used)
      (setf sources-used 3))
      (if (and (not (eq condition 'if-bit-set))
           (not (eq condition 'if-bit-clear)))
      (setf (elt code 6) nil)))

    (if (elt code 2)
    (setf sources-used 3))

    (if (eq (elt code 4) 'always)
    (setf (elt code 4) nil))

    (if (and (eq (elt code 0) 'call-xct-next)
         (eq (elt code 3) 'l-0))
    (progn
      (setf (elt code 3) nil)
      (setf (elt code 0) 'access-i-mem)))

    (if (and (eq (elt code 0) 'popj)
         (elt code 1))
    (progn
      (setf (elt code 0) 'read-i-mem)
      (setf (elt code 1) nil)
      (setf (elt code 3) (decode-destination opword))))

    (if (and (eq (elt code 0) 'popj)
         (elt code 2))
    (progn
      (setf (elt code 0) 'write-i-mem)
      (setf (elt code 2) nil)))

    (if (eq 0 (logand 2 sources-used))
    (setf (elt code 7) nil))

    (if (eq 0 (logand 1 sources-used))
    (setf (elt code 8) nil))

    (final-cleanup code)))

(defun disassemble-dispatch-instruction (opword)
  (let ((code (list (decode-dispatch-op opword)
            (ldb (byte 12 20) opword)
            (decode-m-source opword)
            (decode-a-source opword)
            (decode-abj opword))))
    (final-cleanup code)))

(defun disassemble-instruction (opword &aux code)
  (setf code (case (ldb (byte 2 54) opword)
               (0 (disassemble-alu-instruction opword))
               (1 (disassemble-byte-instruction opword))
               (2 (disassemble-jump-instruction opword))
               (3 (disassemble-dispatch-instruction opword))))
  (format t "~S~%" code))

(defun disassemble-split-instruction (opword-a opword-m)
  (disassemble-instruction (dpb opword-a (byte 24 32) opword-m)))

;; XXX should have some way to disassemble the prom?
;;
(defun disassemble-i-memory-range (start end)
  (dotimes (i (- end start))
    (format t "~A " (get-i-memory-name (+ start i)))
    (disassemble-split-instruction (aref *i-memory-a* (+ start i))
                                   (aref *i-memory-m* (+ start i)))))

(defun disassemble-mcr-partition (filename)
  (with-open-file (mcrfile filename :direction :input :element-type '(unsigned-byte 8))
    (flet ((next-word ()
         (let ((b0 (read-byte mcrfile))
           (b1 (read-byte mcrfile))
           (b2 (read-byte mcrfile))
           (b3 (read-byte mcrfile)))
           (logior b0 (ash b1 8) (ash b2 16) (ash b3 24)))))
      (loop
       (let ((block-type (next-word))
         (block-address (next-word))
         (block-length (next-word)))
     (case block-type
       (1 ;; I-Memory
        (loop until (zerop block-length)
          doing
          (decf block-length)
          (format t "L-~D " block-address)
          (let ((opword-a (next-word))
            (opword-m (next-word)))
            (disassemble-split-instruction opword-a opword-m))
          (incf block-address)))
#+nil	   (2 ;; D-Memory
        )
       (3 ;; Main-Memory
        (unless (zerop block-length)
          (format t "Unable to decode non-zero-length Main-Memory segment.~%")
          (return-from disassemble-mcr-partition))
        (format t "Ignored word: #x~X~%" (next-word))
        (format t "MCR Entry Point: L-~D~%" (next-word)))
#+nil	   (4 ;; A-Memory
        )
#+nil	   (5 ;; T-Memory
        )
       (t ;; Bogus
        (format t "Bogus MCR block type ~X.~%" block-type)
        (return-from disassemble-mcr-partition))))))))

#|
(defun read-one-instruction (stream)
  (read-byte stream)
  (let ((word 0))
    (dotimes (i 7)
      (setf word (dpb (read-byte stream)
              (byte 8 (* i 8))
              word)))
    word))

(defun disassemble-romfile (filename)
  (clear-symbol-table)
  (with-open-file (romfile-stream filename
                  :direction :input
                  :element-type '(unsigned-byte 8))
          (dotimes (i #x800) ; #x22
            (format t "~A " (get-i-memory-name i))
            (disassemble-instruction (read-one-instruction
                          romfile-stream)))
          )
  T)


;; Used to update an old disassembly with consistent new symbols for
;; M-, A-, and I- memory addresses and new instruction decodings
;; while preserving commentary (lines beginning with a semicolon).
;;
;; This could be expanded upon with, say, a line prefix to strip from
;; the output and changing eval-forms to be output before being
;; evaluated. That way, we could do (hexdump-region ...) and get a
;; hexdump inserted in the output.
(defun re-disassemble-romfile (romfile disfile)
  "Used to update an old disassembly with consistent new symbols for M-, A-, and I- memory addresses and new instruction decodings while preserving commentary (lines beginning with a semicolon)."
  (clear-symbol-table)
  (with-open-file (romfile-stream romfile
                  :direction :input
                  :element-type '(unsigned-byte 8))
          (with-open-file (disfile-stream disfile
                          :direction :input)
                  (do ((i 0))
                      ((null (peek-char nil disfile-stream nil nil)))
                    (let ((fchar (peek-char nil disfile-stream
                                nil nil)))
                      (if (equal #\; fchar)
                      (write-line (read-line disfile-stream))
                    (if (equal #\Newline fchar)
                        (progn
                          (read-char disfile-stream)
                          (terpri))
                      (if (equal #\( fchar)
                          (let ((form (read disfile-stream)))
                        (eval form)
                        (format t "~S~%" form))
                        (if fchar
                        (progn
                          (read disfile-stream)
                          (read disfile-stream)
                          (format t "~A " (get-i-memory-name i))
                          (incf i)
                          (disassemble-instruction
                           (read-one-instruction
                            romfile-stream))))))))))))
|#


#|
lisp:
(with-open-file (*standard-output* #p"/home/nyef/src/lisp/aek/E1_eproms/prom_disassem_2" :direction :output)
        (re-disassemble-romfile #p"/home/nyef/src/lisp/aek/E1_eproms/prom_combined"
                    #p"/home/nyef/src/lisp/aek/E1_eproms/prom_disassem"))

elisp:
(save-excursion
  (set-buffer "prom_disassem")
  (save-buffer)
  (slime-eval '(eval (read-from-string "(with-open-file (*standard-output* #p\"/home/nyef/src/lisp/aek/E1_eproms/prom_disassem_2\" :direction :output)
        (re-disassemble-romfile #p\"/home/nyef/src/lisp/aek/E1_eproms/prom_combined\"
                    #p\"/home/nyef/src/lisp/aek/E1_eproms/prom_disassem\"))")))
  (rename-file "/home/nyef/src/lisp/aek/E1_eproms/prom_disassem_2"
           "/home/nyef/src/lisp/aek/E1_eproms/prom_disassem" t)
  (revert-buffer t t t))
|#

;;; EOF
