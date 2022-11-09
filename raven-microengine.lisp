;;;
;;; raven-microengine.lisp
;;;
;;; CPU Microcode interpreter.
;;;

(in-package :raven)

;; This is deadly straightforward. We set just enough initial state so that the STBM PROM
;; code can start running and set everything else up.
(defun microengine-initialize ()
  (setf *machine-control-register* 0)
  (setf *micro-instruction-pointer* 0)
  (setf *inhibit-micro-execution* t))

;; This is the interpreter for branch conditions, either ABJ conditions (in the case of ALU
;; and BYTE instructions) or conditional control transfer for JUMP instructions. The manual
;; is somewhat unenlightening as to the actual (rather than intended) meanings for some of
;; the various condition codes. In one case (not-equal) we have an absolute reference point
;; for the correct behavior. For two other cases (less and less-or-equal) we are still in
;; the dark.
(declaim (inline interpret-condition))
(defun interpret-condition (opword-m m-source a-source alu-result fixnum-overflow alu-carry)
  (declare (type (unsigned-byte 32) opword-m m-source a-source alu-result)
           (fixnum fixnum-overflow))
  (eq (= 0 (ldb (byte 1 15) opword-m))
      (if (= 0 (ldb (byte 1 14) opword-m))
          (case (ldb (byte 4 10) opword-m)
            (0 ;; bit-set
             (let ((rotation (ldb (byte 5 0) opword-m)))
               (= 1 (if (= 1 (ldb (byte 1 16) opword-m))
                        (ldb (byte 1 rotation) m-source)
                      (if (= rotation 0)
                          (ldb (byte 1 0) m-source)
                        (ldb (byte 1 (- 32 rotation)) m-source))))))
            (1 ;; less
             ;; The manual is somewhat unspecific when covering the cases of less and not-equal.
             ;; Or, perhaps, just plain wrong. Unfortunately, I haven't figured this one out yet.
             #+(or)(and (not (zerop alu-carry))
                  (/= #xffffffff alu-result))
             (< (logxor #x80000000 m-source)
                (logxor #x80000000 a-source)))
            (2 ;; less-or-equal
             ;; The manual says ALU(32), which presumably means the carry output, which may be more
             ;; correct... except that we need the -sign- of the result. We can entertain the idea
             ;; that the ALU is a 33-bit system operating on sign-extended 32-bit data, but that's
             ;; a bit much, and would only affect one case (alu-result of #x7fffffff, thanks to
             ;; carry). If it is, it is, but we need a test case first.
             ;; (It -is- a 33-bit system. And Hummingbird is 34.)
             (not (zerop alu-carry #+(or)(logand #x80000000 alu-result))))
            (3 ;; not-equal
             ;; This interpretation, with the idea that byte and jump instructions force ALU M-A-1,
             ;; is consistent with the one strange test in the STBM selftest.
             (/= #xffffffff alu-result))
            (4 ;; page-fault
             *page-fault*)
            (5 ;; page-fault-or-interrupt
             (or *page-fault* *interrupt-pending*))
            (6 ;; page-fault-or-interrupt-or-sequence-break
             (or *page-fault* *interrupt-pending* (/= 0 (logand #x4000 *machine-control-register*))))
            (7 ;; true
             t)
            (8 ;; tag-not-equal
             ;; FIXME: May want to convert this to just logand the bitfields or (zerop (logand (logxor))).
             (/= (ldb (byte 5 25) m-source)
                 (ldb (byte 5 25) a-source)))
            (9 ;; not-memory-busy
             (zerop *memory-busy*))
            (10 ;; q0
             (oddp (aref *q-register*)))
            (11 ;; nu-bus-error
             *nubus-error*)
            (12 ;; not-fixnum-overflow
             (= 0 fixnum-overflow))
            (13 ;; boxed-sign-bit
             (= #x01000000 (logand #x01000000 alu-result)))
            (14 ;; no-interrupt
             (not *interrupt-pending*))
            (t ;; Unhandled or bogus
             (format t "Unhandled condition.~%")
             ))

        ;; Look up the tag-memory contents for this index.
        (= 1 (read-t-memory (ldb (byte 4 10) opword-m) m-source)))))

;; This function applies an abbreviated jump field. It modifies the interpreter
;; state to effect flow control changes.
(defun interpret-abj (opword-a)
  (declare (type (unsigned-byte 24) opword-a))
  (case (ldb (byte 3 19) opword-a)
    (0 ;; Nop
     )
    (1 ;; Skip
     (setf *inhibit-micro-execution* t))
    (2 ;; Call-Illop
     (write-functional-destination 5 (- *micro-instruction-pointer* 1))
     (setf *micro-instruction-pointer* #o10)
     (setf *inhibit-micro-execution* t))
    (3 ;; Call-Trap
     (write-functional-destination 5 (- *micro-instruction-pointer* 1))
     (setf *micro-instruction-pointer* #o12)
     (setf *inhibit-micro-execution* t))
    (4 ;; Call-Buserr
     (write-functional-destination 5 (- *micro-instruction-pointer* 1))
     (setf *micro-instruction-pointer* #o14)
     (setf *inhibit-micro-execution* t))
    (5 ;; "Unused"
     (write-functional-destination 5 (- *micro-instruction-pointer* 1))
     (setf *micro-instruction-pointer* #o16)
     (setf *inhibit-micro-execution* t))
    (6 ;; Popj
     (setf *micro-instruction-pointer* (read-functional-source #o21))
     (when (not (zerop (logand #x4000 *micro-instruction-pointer*)))
       #+nil(format t "abj-popj-14~%")
       (setf *micro-instruction-pointer*
             (logand #x3fff *micro-instruction-pointer*))
       (handle-popj-14))
     (setf *inhibit-micro-execution* t))
    (7 ;; Popj-after-next
     (setf *micro-instruction-pointer* (read-functional-source #o21))
     (when (not (zerop (logand #x4000 *micro-instruction-pointer*)))
       #+nil(format t "abj-popj-afternext-14~%")
       (setf *micro-instruction-pointer*
             (logand #x3fff *micro-instruction-pointer*))
       (handle-popj-14)))
    (t ;; Unimplemented
     (format t "Error dispatching ABJ instruction.~%"))))

;; This is a helper function to do a 32-bit modular add. There are two
;; versions here, one for systems without built-in 32-bit arithmetic
;; and one for SBCL. Presumably CMUCL and some other systems also have
;; modular arithmetic by now, but I don't use them. Feel free to add
;; them to the reader conditionals.
(declaim (inline +-32bit-3))
#-sbcl
(defun +-32bit-3 (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (let* ((result-low (+ (ldb (byte 16 0) a)
                        (ldb (byte 16 0) b)
                        (ldb (byte 16 0) c)))
         (result-high (+ (ldb (byte 16 16) a)
                         (ldb (byte 16 16) b)
                         (ldb (byte 16 16) c)
                         (ldb (byte 1 16) result-low))))
    (dpb result-high (byte 16 16) result-low)))
#+sbcl
(defun +-32bit-3 (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (logand #xffffffff (+ a b c)))

;; This is a helper function to do a 32-bit modular subtract. There are
;; two versions here, same as for +-32bit-3. The same comments apply.
(declaim (inline --32bit-3))
#-sbcl
(defun --32bit-3 (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (let* ((result-low (- (ldb (byte 16 0) a)
                        (ldb (byte 16 0) b)
                        (ldb (byte 16 0) c)))
         (result-high (- (ldb (byte 16 16) a)
                         (ldb (byte 16 16) b)
                         (ldb (byte 16 16) c)
                         (ldb (byte 1 16) result-low))))
    (dpb result-high (byte 16 16) (logand #xffff result-low))))
#+sbcl
(defun --32bit-3 (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (logand #xffffffff (- a b c)))

;; This function is the main ALU setup. The ALU is unfortunately very poorly
;; documented, to the point where the manual doesn't even list the various
;; operations the ALU can do (!). The manual makes a mention of a 33rd output
;; line from the ALU in the section on condition bits, and suggests that a
;; sign output exists separately from the 32-bit ALU result normally used in
;; the section on O bus control. The manual also implies that the sign output
;; is simply ALU(31) in the section on Q-register control. The CADR document
;; (memo 528), which is for an earlier CPU, states that the Q-register control
;; uses ALU(31) but that the O bus control uses 'the correct sign shifted in,
;; regardless of overflow'. The CADR document also suggests that the multiply
;; and divide instructions behave differently depending on the sign of the
;; inputs, which does not appear to be the case for Raven.
;; FIXME: Some cleanup to perform in here.
(declaim (inline perform-alu-operation))
(defun perform-alu-operation (opword-m m-source a-source &aux (fixnum-overflow 0) (alu-carry 0))
  (declare (type (unsigned-byte 32) opword-m m-source a-source)
           (fixnum fixnum-overflow alu-carry))
  (let ((alu-result
         (case (ldb (byte 5 3) opword-m)
           ;; NOTE: These are in ascending order by boole opcode, but we break
           ;; them out like this for two reasons. One is to make it explicit
           ;; that each instruction is handled. The other is that SBCL 0.8.5
           ;; doesn't inline boole without constant operations, and thus it
           ;; can't use mod32 ops (not so bad), and that it has to box the
           ;; values (intolerable).
           (#o0  (logand #xffffffff (boole boole-clr   a-source m-source))) ;; SETZ
           (#o1  (logand #xffffffff (boole boole-and   a-source m-source))) ;; AND
           (#o2  (logand #xffffffff (boole boole-andc1 a-source m-source))) ;; ANDCA
           (#o3  (logand #xffffffff (boole boole-2     a-source m-source))) ;; SETM
           (#o4  (logand #xffffffff (boole boole-andc2 a-source m-source))) ;; ANDCM
           (#o5  (logand #xffffffff (boole boole-1     a-source m-source))) ;; SETA
           (#o6  (logand #xffffffff (boole boole-xor   a-source m-source))) ;; XOR
           (#o7  (logand #xffffffff (boole boole-ior   a-source m-source))) ;; IOR
           (#o10 (logand #xffffffff (boole boole-nor   a-source m-source))) ;; ANDCB
           (#o11 (logand #xffffffff (boole boole-eqv   a-source m-source))) ;; EQV
           (#o12 (logand #xffffffff (boole boole-c1    a-source m-source))) ;; SETCA
           (#o13 (logand #xffffffff (boole boole-orc1  a-source m-source))) ;; ORCA
           (#o14 (logand #xffffffff (boole boole-c2    a-source m-source))) ;; SETCM
           (#o15 (logand #xffffffff (boole boole-orc2  a-source m-source))) ;; ORCM
           (#o16 (logand #xffffffff (boole boole-nand  a-source m-source))) ;; ORCB
           (#o17 (logand #xffffffff (boole boole-set   a-source m-source))) ;; SETO

           (#o20 ;; MUL
            (let ((result (if (oddp (aref *q-register*))
                              (+-32bit-3 m-source a-source 0)
                              m-source)))
              result))
           #+nil(#o21 ;; MUL-Last
                 )
           (#o22 ;; DIV
            (if (evenp (aref *q-register*))
                (+-32bit-3 m-source a-source 0)
                (--32bit-3 m-source a-source 0)))
           (#o23 ;; DIV-First
            (--32bit-3 m-source a-source 0))
           (#o24 ;; DIV-Corr
            (if (evenp (aref *q-register*))
                (+-32bit-3 m-source a-source 0)
                m-source))
           (#o31 ;; ADD and M+A+1
            (let ((result (+-32bit-3 m-source a-source (ldb (byte 1 2) opword-m))))
              (setf fixnum-overflow (logand #x01000000
                                            (logxor m-source result)
                                            (logxor a-source result)))
              (setf alu-carry (if (< result m-source) 1 0))
              (setf alu-carry (logand 1 (+ alu-carry (ldb (byte 1 31) m-source) (ldb (byte 1 31) a-source))))
              result))
           (#o34 ;; M+ and friends
            (let ((result (+-32bit-3 m-source (ldb (byte 1 2) opword-m) 0)))
              (setf alu-carry (if (< result m-source) 1 0))
              (setf alu-carry (logand 1 (+ alu-carry (ldb (byte 1 31) m-source) (ldb (byte 1 31) a-source))))
              result))
           (#o36 ;; M-A-1 and SUB
            (let* ((result (--32bit-3 m-source a-source (- 1 (ldb (byte 1 2) opword-m))))
                   (overflow-bits (logand (logxor m-source a-source) (logxor m-source result))))
              (setf fixnum-overflow (logand #x01000000 overflow-bits
                                            #+(or) (logxor m-source a-source)
                                            #+(or) (logxor m-source result)))
;             (setf alu-carry (if (> result m-source) 1 0))
;             (setf alu-carry (logxor alu-carry (ldb (byte 1 31) overflow-bits)))
;             (setf alu-carry (logand 1 (+ alu-carry (ldb (byte 1 31) m-source) (ldb (byte 1 31) a-source))))
              (setf alu-carry (ldb (byte 1 31) result))
              result))
           (#o37 ;; M+M and M+M+1
            (let ((result (+-32bit-3 m-source m-source (ldb (byte 1 2) opword-m))))
              (setf alu-carry (ldb (byte 1 31) m-source))
              result))
           (t ;; Unsupported or bogus.
            (format t "Unsupported ALU operation ~A.~%" (ldb (byte 5 3) opword-m))
            0))))
    (if (/= 3 (ldb (byte 2 6) opword-m))
        (setf alu-carry (ldb (byte 1 31) alu-result)))
    (values alu-result fixnum-overflow alu-carry)))

;; The BYTE and JUMP instructions still need the ALU in order to supply condition codes.
;; They force the ALU operation to be M-A-1, which we have reimplemented below for speed
;; and clarity of operation.
(declaim (inline perform-alu-for-non-alu-instruction))
(defun perform-alu-for-non-alu-instruction (m-source a-source)
  (declare (type (unsigned-byte 32) m-source a-source))
  (let* ((alu-result (--32bit-3 m-source a-source 1))
;        (alu-carry (if (> alu-result m-source) 1 0))
;        (alu-carry (logand 1 (+ alu-carry (ldb (byte 1 31) m-source) (ldb (byte 1 31) a-source))))
         (alu-carry (ldb (byte 1 31) alu-result))
         (fixnum-overflow (logand #x01000000
                                  (logxor m-source a-source)
                                  (logxor m-source alu-result))))
    (declare (fixnum fixnum-overflow))
    (values alu-result fixnum-overflow alu-carry)))

;; This is the condition interpreter for BYTE and JUMP instructions. This encapsulates the
;; ALU and condition interface so that it is easier to change (and it has been changing).
(declaim (inline interpret-non-alu-condition))
(defun interpret-non-alu-condition (opword-m m-source a-source)
  (multiple-value-bind (alu-result fixnum-overflow alu-carry)
      (perform-alu-for-non-alu-instruction m-source a-source)
    (declare (type (unsigned-byte 32) alu-result))
    (interpret-condition opword-m m-source a-source alu-result fixnum-overflow alu-carry)))

;; This is a condition interpreter for ALU operations. It is used for testing. It doesn't
;; have the appropriate controls for obus or q control, nor does it provide ALU outputs. It
;; takes five bits worth of alu instruction + one bit carry input + six bits of condition
;; code, and A and M inputs, runs the ALU on the inputs for the desired opcode and returns
;; the condition status as T or NIL.
(defun interpret-alu-condition (opword-m m-source a-source)
  (multiple-value-bind (alu-result fixnum-overflow alu-carry)
      (perform-alu-operation opword-m m-source a-source)
    (declare (type (unsigned-byte 32) alu-result))
    (interpret-condition opword-m m-source a-source alu-result fixnum-overflow alu-carry)))

;; Quick and dirty test setup for various condition options in the ALU, BYTE, and JUMP
;; instructions. Easier to do it this way than to run the full emulator, especially since
;; some of the places where the faults are take a while for the emulator to run through to.
;; Also easier to do it this way because it's easier to interpret the results. We're using
;; the assumption that BYTE and JUMP instructions force the ALU operation to M-A-1.
;; This is mostly to keep me from breaking stuff when I 'fix' the ALU or condition logic.
;; More tests should be added here as necessary.
;;
;; Running this is cheap enough, might consider adding assert-alu-result and assert-obus-
;; result functions so that we don't have to do much messing around with alu-op-names.
(defun test-conditions ()
  (let ((condition-names '(if-bit-set if-less if-less-or-equal if-not-equal if-page-fault
                           if-page-fault-or-interrupt if-page-fault-or-interrupt-or-sequence-break
                           always if-tag-not-equal if-not-memory-busy if-q0 if-nu-bus-error
                           if-not-fixnum-overflow if-negative if-no-interrupt bogus
                           x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
                           if-bit-clear if-greater-or-equal if-greater if-equal if-no-page-fault
                           if-no-page-fault-or-interrupt if-no-page-fault-or-interrupt-or-sequence-break
                           never if-tag-equal if-memory-busy if-not-q0 if-no-nu-bus-error
                           if-fixnum-overflow if-positive if-interrupt bogus2
                           x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31))
        (alu-op-names '(setz x0 and x1 andca x2 setm x3 andcm x4 seta x5 xor x6 ior x7
                        andcb x8 eqv x9 setca x10 orca x11 setcm x12 orcm x13 orcb x14 seto x15
                        mul x16 mul-last x17 div x18 div-first x19 div-corr x20 x21a x21 x22a x22 x23a x23
                        x24a x24 m+a m+a+1 x26a x26 x27a x27 m+ m+1 x29a x29 m-a-1 sub m+m m+m+1))
        (passed t))
    (flet ((assert-nil (alu-op condition m-src a-src)
             (when (interpret-alu-condition (logior (ash (position condition condition-names) 10)
                                                    (ash (position alu-op alu-op-names) 2)) m-src a-src)
               (setf passed nil)
               (format t "FAIL: ~(~A ~A #x~8,'0X #x~8,'0X~) should be NIL, is T~%"
                       alu-op condition m-src a-src)))
           (assert-t (alu-op condition m-src a-src)
             (unless (interpret-alu-condition (logior (ash (position condition condition-names) 10)
                                                      (ash (position alu-op alu-op-names) 2)) m-src a-src)
               (setf passed nil)
               (format t "FAIL: ~(~A ~A #x~8,'0X #x~8,'0X~) should be T, is NIL~%"
                       alu-op condition m-src a-src)))
           (assert-result (result alu-op m-src a-src)
             (let ((alu-result (perform-alu-operation (ash (position alu-op alu-op-names) 2) m-src a-src)))
               (unless (= result alu-result)
                 (setf passed nil)
                 (format t "FAIL: ~(~A #x~8,'0X #x~8,'0X should be #x~8,'0X, is #x~8,'0X~)~%"
                         alu-op m-src a-src result alu-result)))))
      
      (assert-t   'm-a-1 'if-equal 0 0)
      (assert-nil 'm-a-1 'if-equal 0 1)

      (assert-nil 'm-a-1 'if-greater-or-equal #x80000000 0)  ; L-117
      (assert-nil 'm-a-1 'if-less 0 #xffffffff)              ; L-125
      (assert-nil 'm-a-1 'if-less 0 0)                       ; L-124
      (assert-t   'm-a-1 'if-greater-or-equal 0 #xffffffff)  ; L-122
      (assert-t   'm-a-1 'if-greater-or-equal 0 0)           ; L-120
      (assert-t   'm-a-1 'if-greater-or-equal 1 0)           ; L-1564
      (assert-t   'm-a-1 'if-greater-or-equal #x7fffffff 0)
      (assert-t   'm-a-1 'if-less #x80000000 0)              ; L-118

      (assert-nil 'm-a-1 'if-greater 0 0)
      (assert-t   'm-a-1 'if-greater 7 2)
      (assert-nil 'm-a-1 'if-greater 2 7)
      (assert-t   'm-a-1 'if-greater #xffffff7f #xfffff7ff)
      (assert-nil 'm-a-1 'if-greater #xfffff7ff #xffffff7f)
      (assert-t   'm-a-1 'if-less-or-equal 0 0)
      (assert-nil 'm-a-1 'if-less-or-equal 7 #xffffffff)
      (assert-t   'm-a-1 'if-less-or-equal #xffffffff 7)
      (assert-nil 'm-a-1 'if-less-or-equal 0 #xffffffff)
      (assert-t   'm-a-1 'if-less-or-equal #xffffffff 0)     ; L-1566

      (assert-t   'seto  'if-equal 57 2999)                  ; L-198
      (assert-nil 'seto  'if-greater 0 0)                    ; L-150

      (assert-nil 'orca  'if-less-or-equal 0 #xffffffff)     ; L-134

      (assert-result #xffffffff 'seto 57 2999)

      passed)))

#|
;; Rotate value left rotation bits.
(declaim (inline rotate-value))
(defun rotate-value (value rotation)
  (declare (type (integer 0 31) rotation)
           (type (unsigned-byte 32) value))
  (dpb value (byte (- 32 rotation) rotation)
       (ldb (byte rotation (- 32 rotation)) value)))

;; Implementation of rotate-right by Raymond Toy.
(defun rotate-right (v n)
  (declare (type (integer 0 31) n)
           (type (unsigned-byte 32) v)
           (optimize (speed 3) (safety 0)))
  (let ((low (ldb (byte n 0) v)))
    (logior (ash v (- n)) (the (unsigned-byte 32) (ash low (- 32 n))))))
|#

;; Rotate an unsigned 32-bit value left n places. Used to simulate the
;; barrel shifter for the BYTE instruction.
(declaim (inline rotate-left))
(defun rotate-left (v n)
  (declare (type (integer 0 31) n)
           (type (unsigned-byte 32) v)
           (optimize (speed 3) (safety 0)))
  (let ((low (ldb (byte (- 32 n) 0) v)))
    (logior (ash v (- (- 32 n))) (the (unsigned-byte 32) (ash low n)))))

;; Create an integer containing a number of consecutive 1 bits in the
;; low-order positions. Used to simulate the left and right mask memories
;; for the BYTE instruction.
;; %make-ones by Raymond Toy.
(declaim (inline %make-ones))
(defun %make-ones (len)
  (declare (type (integer 1 32) len)
           (optimize (speed 3) (safety 0)))
  (if (< len 32)
      (1- (ash 1 len))
      #.(1- (ash 1 32))))

;; This is the guts of the implementation of the BYTE instruction.
;; There aren't really any suprises here for someone who has read
;; what the manual and memo 528 have to say about BYTE instructions.
(declaim (inline perform-byte-operation))
(defun perform-byte-operation (opword-m m-source a-source)
  (declare (type (unsigned-byte 32) opword-m m-source a-source))
  (let* ((rotation (ldb (byte 5 0) opword-m))
         (rotation (if (= 1 (ldb (byte 1 16) opword-m))
                       (logand 31 (- 32 rotation))
                     rotation))
         (width (ldb (byte 5 5) opword-m))
         (mask-rotate (= 1 (ldb (byte 1 18) opword-m)))
         (source-rotate (= 1 (ldb (byte 1 17) opword-m)))
         (right-mask-index (if mask-rotate rotation 0))
         (left-mask-index (logand 31 (+ right-mask-index (- width 1))))
         (right-mask (logxor #xffffffff
                             (1- (dpb 1 (byte 1 right-mask-index) 0))))
         (left-mask (%make-ones (1+ left-mask-index)))
         (final-mask (logand left-mask right-mask))
         (rotated-source (if source-rotate (rotate-left m-source rotation) m-source))
         (result (logior (logand rotated-source final-mask)
                         (logand a-source (logxor #xffffffff final-mask)))))
    (declare (type (unsigned-byte 32) right-mask left-mask final-mask rotated-source result)
             (type (integer 0 31) rotation width right-mask-index left-mask-index))
    result))

;; This is the final output handling for ALU and BYTE instructions.
;; It actually does the dirty work of writing the results to A-Memory
;; or to M-Memory and a functional destination.
(declaim (inline store-result))
(defun store-result (opword-m result)
  (declare (type (unsigned-byte 32) opword-m))
  (if (= 1 (ldb (byte 1 31) opword-m))
      (write-a-memory (ldb (byte 10 19) opword-m) result)
    (progn
      (write-m-memory (ldb (byte 6 19) opword-m) result)
      (write-functional-destination (ldb (byte 6 25) opword-m) result))))

;; This function handles the Q control portion of ALU instructions. It's
;; fairly straightforward. It performs destructive modification of the Q
;; register.
(declaim (inline handle-q-control))
(defun handle-q-control (opword-m result)
  (declare (type (unsigned-byte 32) opword-m result))
  (case (ldb (byte 2 0) opword-m)
    (0 ;; nop
     )
    (1 ;; shift-left
     (setf (aref *q-register*) (logior (dpb (aref *q-register*) (byte 31 1) 0)
                                (- 1 (ldb (byte 1 31) result)))))
    (2 ;; shift-right
     (setf (aref *q-register*) (logior (ldb (byte 31 1) (aref *q-register*))
                                (* #x80000000 (logand 1 result)))))
    (3 ;; load-q
     (setf (aref *q-register*) result)))
  (values))

;; Here we handle the O bus control for ALU instructions. There are a
;; couple interesting points here. The first is that the output from
;; the O bus feeds into the mask mechanism for the BYTE instruction.
;; Or, more accurately, the two high bits of the obus result select
;; two inputs to the mask mechanism and the low bit selects which
;; input to the mask mechanism is used. This becomes important in two
;; cases. First, the BYTE instruction forces the O bus selector to 0
;; and 1 and controls the masker itself. Second, tagged ALU operation
;; also uses the mask mechanism, which is why you're supposed to use
;; the 'normal' output control with tagged operation. The second
;; interesting point is that rightshift-1 supposedly uses a 'sign'
;; output from the ALU. The manual merely refers to it as 'sign', but
;; memo 528 says that the CADR manages to use 'correct sign, regardless
;; of overflow'. We're using alu-carry, but may need to rethink that.
;; It may be that the correct sign is obtained by taking the high bit
;; of the ALU result XOR the signed overflow from the result. Since the
;; overflow would be calculated on the carry lines directly, rather
;; than the usual emulator trick in terms of the inputs, it would be 0
;; for all logical operations.
(declaim (inline handle-output-selector))
(defun handle-output-selector (opword-m alu-result m-source a-source alu-carry)
  (declare (type (unsigned-byte 32) opword-m m-source a-source alu-result)
           (type (integer 0 1) alu-carry))
  (case (ldb (byte 3 16) opword-m)
    (0 ;; A-Bus
     a-source)
    (1 ;; R-Bus
     (let* ((rotation (ldb (byte 5 0) opword-m))
            (rotation (if (= 1 (ldb (byte 1 16) opword-m))
                          (logand 31 (- 32 rotation))
                          rotation)))
       (rotate-left m-source rotation)))
    (2 ;; A-Bus (again)
     a-source)
    (3 ;; Normal
     alu-result)
    (4 ;; Leftshift-1
     (dpb alu-result (byte 31 1)
          (ldb (byte 1 31) (aref *q-register*))))
    (5 ;; Rightshift-1
     (logior (ash alu-carry 31) (ldb (byte 31 1) alu-result)))
    (6 ;; Sign-Extend
     (if (zerop (logand #x01000000 alu-result))
         (logand #x00ffffff alu-result)
         (logior #xff000000 alu-result)))
    (7 ;; Mirror
     (let ((data alu-result))
       (setf data (logior (ash (logand data #x55555555) 1)
                          (ash (logand data #xaaaaaaaa) -1)))
       (setf data (logior (ash (logand data #x33333333) 2)
                          (ash (logand data #xcccccccc) -2)))
       (setf data (logior (ash (logand data #x0f0f0f0f) 4)
                          (ash (logand data #xf0f0f0f0) -4)))
       (setf data (logior (ash (logand data #x00ff00ff) 8)
                          (ash (logand data #xff00ff00) -8)))
       (setf data (logior (ash (logand data #x0000ffff) 16)
                          (ash (logand data #xffff0000) -16)))
       data))
    (t ;; Unimplemented
     (format t "Unimplemented output selector.~%")
     alu-result)))

;(declaim (notinline microengine-interpret-alu-instruction))
(defun microengine-interpret-alu-instruction (&aux (opword-m (aref *micro-instruction-m*)) (opword-a *micro-instruction-a*))
  (declare (type (unsigned-byte 24) opword-a)
           (type (unsigned-byte 32) opword-m))
  (let ((m-source (read-m-memory (ldb (byte 7 10) opword-a)))
        (a-source (read-a-memory (ldb (byte 10 0) opword-a))))
    (multiple-value-bind (alu-result fixnum-overflow alu-carry)
        (perform-alu-operation opword-m m-source a-source)
      (declare (type (unsigned-byte 32) alu-result))
      (let* ((obus-result (handle-output-selector opword-m alu-result m-source a-source alu-carry))
             (result (if (= (logand opword-m #x10100) #x10100)
                         (logior (logand #xfe000000
                                         (handle-output-selector (logand #xfffeffff opword-m)
                                                                 alu-result m-source a-source alu-carry))
                                 (logand #x01ffffff obus-result))
                         obus-result)))
        (declare (type (unsigned-byte 32) obus-result))

        (if (= 1 (ldb (byte 1 9) opword-m))
            (write-t-memory (ldb (byte 4 10) opword-m) alu-result))

        (store-result opword-m result)
        
        (if (interpret-condition opword-m m-source a-source alu-result fixnum-overflow alu-carry)
            (interpret-abj opword-a))
        
        (handle-q-control opword-m alu-result))))
  (values))

;(declaim (notinline microengine-interpret-byte-instruction))
(defun microengine-interpret-byte-instruction (&aux (opword-m (aref *micro-instruction-m*)) (opword-a *micro-instruction-a*))
  (declare (type (unsigned-byte 24) opword-a)
           (type (unsigned-byte 32) opword-m))
  (let* ((m-source (read-m-memory (ldb (byte 7 10) opword-a)))
         (a-source (read-a-memory (ldb (byte 10 0) opword-a)))
         (result (perform-byte-operation opword-m m-source a-source)))
    (store-result opword-m result)
    (if (interpret-non-alu-condition opword-m m-source a-source)
        (interpret-abj opword-a)))
  (values))

(defvar *pj14-need-fetch* nil)

(defun handle-popj-14 ()
  (let ((chain-enable (not (zerop (logand #x04000000
                                          *machine-control-register*)))))
    (if *micro-instruction-trace*
        (format t "POPJ-14: to ~A chain enable: ~A need fetch: ~A LC ~A~%"
                *micro-instruction-pointer* chain-enable
                *need-fetch* *location-counter*))

    ;; The POPJ-14 condition is almost completely undocumented. It is not
    ;; even mentioned in the TI CPU manual. The only reference I have found
    ;; thus far is in memo 528, which describes the CADR version, which
    ;; isn't suficient information when it comes to emulating it on Raven.

    ;; The behavior of POPJ-14 depends on the need-fetch signal (available
    ;; to microcode as part of the MCR) and on the chain-enable bit also
    ;; in the MCR (settable directly under microcode control).

    ;; If need-fetch is set, the top 25 bits of the location counter are
    ;; transferred to the VMA and a mapped read cycle is initiated. Next,
    ;; the location counter is incremented. If chain-enable is not set,
    ;; the microinstruction address being returned to is modified by oring
    ;; in the value #b10. If we initiated a read cycle and chain-enable is
    ;; set, the microinstruction address being returned to is modified by
    ;; oring in the value #b11. Finally, unless chain-enable was set or a
    ;; read cycle was not required, we update the need-fetch signal;
    ;; setting it if the updated location-counter is even, clearing it
    ;; otherwise.

    ;; There is a cleverness here (and in the corresponding ISTM case for
    ;; the DISPATCH instruction). Instructions are stored in memory with
    ;; the even-offsetted instructions in the low half of the memory word.
    ;; The need-fetch signal is set when the location-counter is -even-.
    ;; It turns out that when reading the MIB, the high-half of the word
    ;; is returned when the location counter is even. This means that the
    ;; location counter actually points to the instruction -after- the one
    ;; currently being executed. Presumably this also means that either the
    ;; macroinstruction handlers start off with a check to see if a memory
    ;; access failed or there is trap functionality to perform the check
    ;; in hardware (this is suggested by some of the fields in the MCR).

    ;; If *need-fetch* is set, start a read cycle.
    (when *need-fetch*
      (setf *pj14-need-fetch* t)
      #+nil (setf (aref *virtual-memory-address*)
                  (ldb (byte 25 1) *location-counter*))
      #+nil (start-read))

    (incf *location-counter*)

    (unless chain-enable
      (setf *micro-instruction-pointer*
            (logior *micro-instruction-pointer* 2)))

    (when (and chain-enable (not *need-fetch*))
      (setf *micro-instruction-pointer*
            (logior *micro-instruction-pointer* 2 #+nil 3)))

    (when (or chain-enable (not *need-fetch*))
      (setf *need-fetch* (evenp *location-counter*)))))

;;(defvar *jump-abj-table* 0)
#|
Jump-abj test data result from lisp run.
24000000000000FF2C000000000000FF

condition fail: 2C000000000000FF
condition pass: 24000000000000FF

Each byte contains jump op bits for a given abj.
Only abjs used are 0 and 7?

|#
;(declaim (notinline microengine-interpret-jump-instruction))
(defun microengine-interpret-jump-instruction (&aux (opword-m (aref *micro-instruction-m*)) (opword-a *micro-instruction-a*))
  (declare (type (unsigned-byte 24) opword-a)
           (type (unsigned-byte 32) opword-m))
  (let* ((m-source (read-m-memory (ldb (byte 7 10) opword-a)))
         (a-source (read-a-memory (ldb (byte 10 0) opword-a)))
         (dest-micro-pc (ldb (byte 14 18) opword-m))
         (condition-true (interpret-non-alu-condition
                          opword-m m-source a-source)))
    (when (= 1 (ldb (byte 1 17) opword-m))
      (format t "M-source select (?)~%")
      (break))

    ;; I-Mem write.
    (when (= 1 (ldb (byte 1 9) opword-m))
      (write-i-memory-m *last-micro-instruction-pointer* m-source)
      (write-i-memory-a *last-micro-instruction-pointer* (ldb (byte 24 0) a-source)))

    ;; I-Mem read.
    (when (= 1 (ldb (byte 1 8) opword-m))
      (if (= 0 (ldb (byte 1 31) opword-m))
          (write-m-memory (ldb (byte 6 19) opword-m)
                          (read-i-memory-m *last-micro-instruction-pointer*))
          (if (< 63 (ldb (byte 10 19) opword-m))
              (write-a-memory (ldb (byte 10 19) opword-m)
                              (read-i-memory-a *last-micro-instruction-pointer*))
              (write-m-memory (ldb (byte 6 19) opword-m)
                              (read-i-memory-a *last-micro-instruction-pointer*)))))

    ;; Jump/abj combination tracking
    #+nil (setf (ldb (byte 1 (+ (ldb (byte 3 5) opword-m)
                          (ash (ldb (byte 3 19) opword-a) 3)
                          (if condition-true #o100 0))) *jump-abj-table*) 1)

    (if condition-true
        (progn
          (setf *inhibit-micro-execution* (= (ldb (byte 1 5) opword-m) 1))
          (if (= 1 (ldb (byte 2 6) opword-m))
              (write-functional-destination 5
                                            (+ *micro-instruction-pointer*
                                               (- (ldb (byte 1 5) opword-m)))))
          (if (= 2 (ldb (byte 2 6) opword-m))
              (setf dest-micro-pc (read-functional-source #o21)))
				     
          (setf *micro-instruction-pointer* (logand #x3fff dest-micro-pc))

	  ;; keep track of control transfers so we can backtrace where the jump
	  ;; came from when debugging
	  ;;
          (setf (aref *control-transfer-ring* *control-transfer-ring-pointer*) *micro-instruction-pointer*)
	  (setf *control-transfer-ring-pointer* (mod (incf *control-transfer-ring-pointer*) 512))

          ;; When uPCS bit 14 set
          (when (not (zerop (logand #x4000 dest-micro-pc)))
            (handle-popj-14))

          ;; Special hack for CALL-XCT-NEXT AND-POPJ-XCT-NEXT
          (when (and (= 7 (ldb (byte 3 19) opword-a))
                     (= 1 (ldb (byte 2 6) opword-m)))
            (decf *microstack-pointer*)))
        (interpret-abj opword-a)))
  (values))

(declaim (inline dispatch-source-data))
(defun dispatch-source-data (opword-m m-source)
  (let ((dispatch-source (ldb (byte 2 12) opword-m)))
    (case dispatch-source
      (0 (let* ((rotation (ldb (byte 5 0) opword-m))
                (rotation (if (= 1 (ldb (byte 1 16) opword-m))
                              (logand 31 (- 32 rotation))
                              rotation))
                (rotated-data (rotate-left m-source rotation))
                (mask (1- (ash 1 (ldb (byte 3 5) opword-m))))
                (mask (if (zerop (logand #xc00 opword-m))
                          mask
                          (logand -2 mask))))
           (logand mask rotated-data)))
      (1 (ash (ldb (byte 5 25) m-source) 1))
      (t (let ((mib (read-m-memory #o112)))
           (if (and (not (zerop (logand #x01000000 *machine-control-register*)))
                    (zerop (logand (logxor 1 (ldb (byte 1 25) *machine-control-register*))
                                   (ldb (byte 1 13) mib)))
                    (= #xd (ldb (byte 4 9) mib)))
               (logior #x800 (dpb (logxor 1 (ldb (byte 1 13) mib))
                                  (byte 1 9)
                                  (ldb (byte 9 0) mib)))
               (logior #xc00 (ldb (byte 10 6) mib))))))))

(declaim (inline dispatch-check-gcvf))
(defun dispatch-check-gcvf (opword-m)
  (if (zerop (logand opword-m #x400))
      0
      (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))))
             (map-1-volatility (ldb (byte 3 7) map-1))
             (map-2-volatility (ldb (byte 2 11) (aref *cached-level-2-control*))))
        (if (> (+ 4 map-2-volatility) (logxor 7 map-1-volatility)) 0 1))))

(declaim (inline dispatch-check-oldspace))
(defun dispatch-check-oldspace (opword-m)
  (if (zerop (logand opword-m #x800))
      0
      (let ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*)))))
        (ldb (byte 1 10) map-1))))

;(declaim (notinline microengine-interpret-dispatch-instruction))

(defun microengine-interpret-dispatch-instruction (&aux (opword-m (aref *micro-instruction-m*)) (opword-a *micro-instruction-a*))
  (declare (type (unsigned-byte 24) opword-a)
           (type (unsigned-byte 32) opword-m))
  (let* ((m-source (read-m-memory (ldb (byte 7 10) opword-a)))
         (dispatch-constant (ldb (byte 10 0) opword-a))
         (a-source (read-a-memory dispatch-constant))

         (raw-dispatch-address (ldb (byte 12 20) opword-m))
         (mir-mask #xfff #+nil (if (zerop (logand #x2000 opword-m)) #xfff #xc7f))
         (source-data (dispatch-source-data opword-m m-source))
         (gc-volatility-flag (dispatch-check-gcvf opword-m))
         (oldspace-flag (dispatch-check-oldspace opword-m))
         (dispatch-address (logior (logand mir-mask raw-dispatch-address)
                                   source-data gc-volatility-flag
                                   oldspace-flag))
         (abj-live nil))
    
    #+nil (when (= 1 (ldb (byte 1 13) opword-m))
      (format t "macroop: LC ~X ~4,'0X~%"
              *location-counter* (read-m-memory #o112)))

    (setf *dispatch-constant* dispatch-constant)
    (case (ldb (byte 2 8) opword-m)
      (0 (let ((dispatch-word (aref *d-memory* dispatch-address)))
           ;; FIXME: logbitp the following.
           (setf *inhibit-micro-execution* (= (ldb (byte 1 14) dispatch-word) 1))
           (case (ldb (byte 2 15) dispatch-word)
             (1 (write-functional-destination
                 5 (- *micro-instruction-pointer*
                      (ash (ldb (byte 1 14) dispatch-word)
                           (ldb (byte 1 17) opword-m)))))
             (2 (setf dispatch-word (read-functional-source #o21))
                (unless (zerop (logand dispatch-word #x4000))
                  ;; FIXME: Shouldn't hack so mightily.
                  (setf *micro-instruction-pointer*
                        (logand #x3fff dispatch-word))
                  (handle-popj-14)
                  (setf dispatch-word *micro-instruction-pointer*)))
             (3 (setf dispatch-word *micro-instruction-pointer*)
                (setf abj-live t)))
           
           (setf *micro-instruction-pointer* (logand #x3fff dispatch-word)))

         (unless (zerop (logand #x8000 opword-m))
           (when *need-fetch*
             (setf (aref *virtual-memory-address*)
                   (ldb (byte 25 1) *location-counter*))
             (start-read))

           (incf *location-counter*)
           (setf *need-fetch* (evenp *location-counter*))))

      (1 (setf (aref *q-register*) (aref *d-memory* dispatch-address)))
      (2 (setf (aref *d-memory* dispatch-address) a-source))
      (3 (format t "DISPATCH operation in error.~%")
         (break)))

    (when (and abj-live (not (zerop (ldb (byte 3 19) opword-a))))
      (unless (eq *inhibit-micro-execution*
                  (= 6 (ldb (byte 3 19) opword-a)))
        (format t "Live ABJ mismatch vs. dispatch word.~%")
        (break))
      (interpret-abj opword-a)))

  (values))

;; This is the toplevel entry for the non-flow-control parts of instruction
;; emulation. It basically dispatches to a handler function for one of the
;; four different kinds of instructions after optionally calling for the
;; current instruction to be disassembled.
(declaim (inline microengine-interpret-instruction))
(defun microengine-interpret-instruction (opword-m opword-a)
  (declare (type (unsigned-byte 24) opword-a)
           (type (unsigned-byte 32) opword-m))
  (when *micro-instruction-trace*
    (disassemble-split-instruction opword-a opword-m))
  (case (ldb (byte 2 22) opword-a)
    (0 (microengine-interpret-alu-instruction))
    (1 (microengine-interpret-byte-instruction))
    (2 (microengine-interpret-jump-instruction))
    (3 (microengine-interpret-dispatch-instruction)))
  (values))

(defun microengine-step ()
  (let ((opword-a *next-micro-instruction-a*)
        (opword-m (aref *next-micro-instruction-m*))
        (need-fetch *pj14-need-fetch*))
    (setf *pj14-need-fetch* nil)

    ;; FIXME: HORRIBLE HACK
    ;; If the next instructions is not inhibited, and reads MD, and
    ;; memory-busy is between 1 and 4 inclusive, and there is a
    ;; nubus-error condition, -and- the memory trap enable bit is
    ;; set in the MCR, then we need to fault to L-16 pushing the
    ;; address of the next and current microinstructions to uPCS,
    ;; inhibiting the execution of both, killing the memory-busy.
    ;; Yes, all that. And we do it this way to avoid having to fix
    ;; the entire timing chain. We should do something similar when
    ;; memory-busy runs out and there's a nubus error and the trap
    ;; is enabled, but we haven't found a case where that matters
    ;; yet (the EXPT test is covered by the existing botch-job).
    ;; One more thing. We also need to do this if we're an ALU or
    ;; BYTE instruction that writes to VMA (or MD or the memory map
    ;; tables or tries to start another memory cycle, but we'll skip
    ;; those for now).
    (when (and *nubus-error*
               (< 0 *memory-busy* 5)
               (not *inhibit-micro-execution*)
               (or (= #o122 (ldb (byte 7 10) opword-a))
                   (and (not (logbitp 23 opword-a))
                        (not (logbitp 31 opword-m))
                        (= #o20 (ldb (byte 6 25) opword-m))))
               (logbitp 13 *machine-control-register*))
      ;; FIXME: Pushing these in backwards order (whoops!)
      (write-functional-destination 5 *micro-instruction-pointer*)
      (write-functional-destination 5 *last-micro-instruction-pointer*)
      (setf *inhibit-micro-execution* t)
      (setf *micro-instruction-pointer* 16)
      (setf *memory-busy* 0))

    (setf *micro-instruction-a* *next-micro-instruction-a*)
    (setf (aref *micro-instruction-m*) (aref *next-micro-instruction-m*))

    (setf *next-micro-instruction-a* (logand #xffffff (read-i-memory-a *micro-instruction-pointer*)))
    (setf (aref *next-micro-instruction-m*) (read-i-memory-m *micro-instruction-pointer*))

    (when *micro-instruction-trace*
      (format t "L-#x~X" *last-micro-instruction-pointer*))

    (setf *last-micro-instruction-pointer* *micro-instruction-pointer*)
    (incf *micro-instruction-pointer*)

    (if (not *inhibit-micro-execution*)
        (microengine-interpret-instruction opword-m opword-a)
      (progn
        (when *micro-instruction-trace* (format t " Inhibited.~%"))
        ;; Make sure that the HALT bit isn't set coming out.
        (setf *micro-instruction-a* (logand (logxor -1 #x20000) *micro-instruction-a*))
        (setf *inhibit-micro-execution* nil)))

    (when need-fetch
      (setf (aref *virtual-memory-address*)
            (ldb (byte 25 1) (1- *location-counter*)))
      (start-read)))

  (when *micro-instruction-trace*
    (if (not *inhibit-micro-execution*)
        (progn
          (format t "L-#x~X " *last-micro-instruction-pointer*)
          (disassemble-split-instruction *next-micro-instruction-a* (aref *next-micro-instruction-m*)))))

  (unless (zerop *memory-busy*)
    (decf *memory-busy*)
    ;; FIXME: was 2, trying other values.
    (when (= 4 *memory-busy*)
      (setf (aref *memory-data*)
            (aref nevermore::*memory-data*)))
    (when (zerop *memory-busy*)
      (check-memory-cycle-abort-2)))
  (run-microcycle-hooks)

  *micro-instruction-pointer*)

(defparameter *eventcheck-interval* 100000)
(defvar *eventcheck-countdown* 0)
(defvar *last-time* (get-universal-time))
(defvar *microcycles-executed* 0)

(defun microengine-run-to (addr)
  (declare (fixnum addr))
  (let ((*micro-instruction-trace* nil))
    (do ()
        ;; Not an imem-write or imem-read, and one of the target address, selftest-fail, or the common error exit.
        ((and (or (= *micro-instruction-pointer* addr)
                  ;; Take out whatever this is, causes problems with X.
                  #+nil (= *micro-instruction-pointer* 40)
                  ;; Take out selftest-fail halt, causes problems in lisp.
                  #+nil (= *micro-instruction-pointer* 18)
                  ;; Trap on LC value (debugging aid).
                  #+nil (= *location-counter* #x18534d)
                  #+nil (= *location-counter* #x18517a)
                  #+nil (= *location-counter* #x185a8d)
                  #+nil (= *location-counter* #x93b788)
                  #+nil (= *location-counter* #x182c4e)
                  #+nil (= *location-counter* #x1133872))
              (not
               (and
                (= (logand #xc00000 *next-micro-instruction-a*) #x800000)
                (not (zerop (logand #x00000300 (aref *next-micro-instruction-m*))))))))
      (microengine-step)

      (if (zerop *eventcheck-countdown*)
          (progn
            (setf *eventcheck-countdown* *eventcheck-interval*)
            (nevermore::check-events))
          (decf *eventcheck-countdown*))

      (incf *microcycles-executed*)
;;      (if (not (zerop (- *last-time* (get-universal-time))))
;;          (progn
;;            (setf *last-time* (get-universal-time))
;;            (format t "~d microcycles~%" *microcycles-executed*)
;;            (setf *microcycles-executed* 0)))

      (unless (zerop (logand #x20000 *micro-instruction-a*))
        (format t "HALT bit set in microinstruction at #x~x.~%" *micro-instruction-pointer*)
        (return *micro-instruction-pointer*)))
    *micro-instruction-pointer*))

#|
;; this version of microengine-run-to has all sorts of random
;; debugging code in of, some of which may still be active.
(defun microengine-run-to (addr)
  (declare (fixnum addr))
  (let ((*micro-instruction-trace* nil))
    (do ()
        ;; Not an imem-write or imem-read, and one of the target address, selftest-fail, or the common error exit.
        ((and (or (= *micro-instruction-pointer* addr)
                  (= *micro-instruction-pointer* 18)
                  (= *micro-instruction-pointer* 40))
              (not
               (and
                (= (logand #xc00000 *next-micro-instruction-a*) #x800000)
                (not (zerop (logand #x00000300 (aref *next-micro-instruction-m*))))))))
      (microengine-step)
#+nil      (when (= *micro-instruction-pointer* 12217)
        (format t "M-29: ~X  M-30: ~X  M-33: ~X  M-34: ~X  M-36: ~X  A-432: ~X  A-518: ~X  A-589: ~X  A-590: ~X~%"
                (read-m-memory 29) (read-m-memory 30) (read-m-memory 33)
                (read-m-memory 34) (read-m-memory 36) (read-a-memory 432)
                (read-a-memory 518) (read-a-memory 589) (read-a-memory 590)))
      (when (and (= *micro-instruction-pointer* 13234)
                 (< (logand *location-counter* #xffff) 64)
                 (not *inhibit-micro-execution*)
                 #+nil(or (= (read-a-memory 866) 15)
                     (= (read-a-memory 866) 16)))
#+nil   (format t "TRAP: 13234. ~A ~A ~A ~A ~A (~A)~%"
                (read-a-memory 866) (read-a-memory 820)
                (read-a-memory 861) (read-a-memory 860)
                (read-a-memory 859) *location-counter*))
#+nil      (when (and (= *last-micro-instruction-pointer* 13223)
                 (= *micro-instruction-pointer* 13321)
                 (= (read-a-memory 861) #xffffffff)
                 (= (read-a-memory 860) 0)
                 (= (read-a-memory 859) #xffffffff))
        (setf *micro-instruction-trace* t))
      (when (= *micro-instruction-pointer* 11692)
        (let ((o-m-d (aref *memory-data*)))
          (setf (aref *memory-data*) (aref *virtual-memory-address*))
        (format t "11692: expect ~A got ~A, l1 ~X, l2c ~X, l2a ~X, FAR ~A, uPCS ~A ~A~%"
                (read-m-memory 39) (read-m-memory 41)
                (read-m-memory #o110) (read-m-memory #o111)
                (read-m-memory #o106)
                (ldb (byte 1 9) *machine-control-register*)
                (aref *microstack* *microstack-pointer*)
                (aref *microstack* (logand #x3f (1- *microstack-pointer*))))
        (setf (aref *memory-data*) o-m-d))
#+nil   (write-m-memory 39 (read-m-memory 41)))
#+nil      (when (and (= *micro-instruction-pointer* 11752)
                 (= *last-micro-instruction-pointer* 11695))
        )
      (when (and (= *micro-instruction-pointer* 13445)
                 (not (= *last-micro-instruction-pointer* 13444))
                 (not *inhibit-micro-execution*))
        (setf *micro-instruction-trace* nil))
      (unless (zerop (logand #x20000 *micro-instruction-a*))
        (format t "HALT bit set in microinstruction.~%")
        (return *micro-instruction-pointer*)))
    *micro-instruction-pointer*))
|#

(defun dump-a-memory ()
  (dotimes (i 256)
    (let ((j (* i 4)))
      (format t "~(~3,'0X: ~8,'0X ~8,'0X ~8,'0X ~8,'0X~)~%"
              j
              (aref *a-memory* (+ j 0))
              (aref *a-memory* (+ j 1))
              (aref *a-memory* (+ j 2))
              (aref *a-memory* (+ j 3))))))

(defun dump-d-memory ()
  (dotimes (i 512)
    (let ((j (* i 8)))
      (format t "~(~3,'0X: ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X~)~%"
              j
              (aref *d-memory* (+ j 0))
              (aref *d-memory* (+ j 1))
              (aref *d-memory* (+ j 2))
              (aref *d-memory* (+ j 3))
              (aref *d-memory* (+ j 4))
              (aref *d-memory* (+ j 5))
              (aref *d-memory* (+ j 6))
              (aref *d-memory* (+ j 7))))))

(defun dump-d-memory-2 ()
  (dotimes (i 4096)
    (let ((dispatch-word (aref *d-memory* i)))
      (format t "~(~3,'0X~): ~A ~A~%" i
              (getf *jump-operations*
                    (ldb (byte 3 14) dispatch-word))
              (get-i-memory-name (ldb (byte 14 0) dispatch-word))))))

(defun dump-active-microstack ()
  (format t "~%")
  (dotimes (i (logand #x3f (1+ *microstack-pointer*)))
    (format t "~2,' X: ~X (~D)~%" i (aref *microstack* i)
            (logand #x3fff (aref *microstack* i)))))

(defun lc-offset ()
  (let ((fun (logand #x01ffffff (read-a-memory 58))))
    (- *location-counter* (ash fun 1))))

(defparameter *type-tags*
  '(dtp-trap             dtp-list            dtp-stack-list      dtp-symbol
    dtp-array            dtp-fix             dtp-character       dtp-single-float
    dtp-short-float      dtp-instance        dtp-extended-number dtp-locative
    dtp-function         dtp-closure         dtp-lexical-closure dtp-u-entry
    dtp-stack-group      dtp-gc-forward      dtp-evcp            dtp-one-q-forward
    dtp-header-forward   dtp-body-forward    dtp-symbol-header   dtp-header
    dtp-array-header     dtp-instance-header dtp-fef-header      dtp-self-ref-pointer
    dtp-gc-young-pointer dtp-free            dtp-null            dtp-ones-trap))

(defun format-data-word (data-word)
  (let ((type-tag (elt *type-tags* (ldb (byte 5 25) data-word))))
    (format t " ~S" type-tag)
    (case type-tag
      (dtp-fix
       (format t " ~A" (dpb data-word (byte 25 0)
                            (if (logbitp 24 data-word) -1 0))))
      (dtp-evcp
       )
      (dtp-symbol
       ))))

(defun dump-stack-frames ()
  (do* ((old-arg-ptr (1+ *pdl-buffer-pointer*) arg-ptr)
        (call-info (read-a-memory 55) (aref *pdl-buffer* (+ frame-ptr 0)))
        (arg-ptr   (read-a-memory 57) (aref *pdl-buffer* (+ frame-ptr 1)))
        (local-ptr (read-a-memory 56) (aref *pdl-buffer* (+ frame-ptr 2)))
        (function  (read-a-memory 58) (aref *pdl-buffer* (+ frame-ptr 3)))
        (lc-offset (lc-offset)
                   (logand #x01ffffff (aref *pdl-buffer* (+ frame-ptr 4))))
        (frame-ptr (logand #x3ff (- local-ptr 5))
                   (logand #x3ff (- local-ptr 5))))
       ((= function #x06000000))

    (format t "frame: callinfo ~8,'0X, fun ~8,'0X, lc ~4,'0X~%"
            call-info function lc-offset)

    (dotimes (i (logand #x01ffffff (- frame-ptr arg-ptr)))
      (let ((data-word (aref *pdl-buffer* (ldb (byte 25 0) (+ arg-ptr i)))))
        (format t "arg[~2,' D]: ~8,'0X" i data-word)
        (format-data-word data-word)
        (terpri)))

    (dotimes (i (logand #x01ffffff (- old-arg-ptr local-ptr)))
      (let ((data-word (aref *pdl-buffer* (ldb (byte 25 0) (+ local-ptr i)))))
        (format t "local[~2,' D]: ~8,'0X" i data-word)
        (format-data-word data-word)
        (terpri)))))

(defun dump-active-pdl ()
  (dotimes (i (logand #x3ff (1+ *pdl-buffer-pointer*)))
    (format t "~3,' X: ~8,'0X~%" i (aref *pdl-buffer* i))))

(defun microengine-force-halt ()
  (setf *micro-instruction-a* (logior #x20000 *micro-instruction-a*)))

(defun microengine-step-trace ()
  (let* ((opword-m (aref *next-micro-instruction-m*))
         (opword-a *next-micro-instruction-a*)
         (m-addr (ldb (byte 7 10) opword-a))
         (m-source (read-m-memory m-addr))
         (a-addr (ldb (byte 10 0) opword-a))
         (a-source (read-a-memory a-addr)))
    (format t "M-#x~X: ~(~8,'0X~)  A-#x~X: ~(~8,'0X~)~%" m-addr m-source a-addr a-source)
    (microengine-step)
    (format t "Result: ~(~8,'0X~)~%"
            (if (= 1 (ldb (byte 1 31) opword-m))
                (read-a-memory (ldb (byte 10 19) opword-m))
                (read-m-memory (ldb (byte 6 19) opword-m))))))

(defun microengine-run-istm-tests ()
  (microengine-run-to 13213)
  (let ((microstack-pointer *microstack-pointer*))
    (loop
     (microengine-run-to 13445)
     (when (/= *micro-instruction-pointer* 13445) (return))
     (if (and (= *last-micro-instruction-pointer* 13444)
              (not *inhibit-micro-execution*))
         (let ((*micro-instruction-trace* nil))
           (microengine-step))
         (let ((last-micro-pc *last-micro-instruction-pointer*))
           (setf *micro-instruction-pointer* 13240)
           (setf *microstack-pointer* microstack-pointer)
           (let ((*micro-instruction-trace* nil))
             (microengine-step))
           (format t "Fail: ~A (from L-~A) ~A ~A ~A ~A ~A -- ~A ~A~%"
                   (read-a-memory 401) last-micro-pc
                   (read-a-memory 866) (read-a-memory 820) (read-a-memory 861)
                   (read-a-memory 860) (read-a-memory 859)
                   (read-a-memory 30) (read-a-memory 31))
           (finish-output))))))

(defun boot ()
  (nevermore::start-x)
  (nevermore::init-window)
  (microengine-initialize)
  (microengine-run-to -1))

(defun reboot ()
  (nevermore::finish-x)
  (boot))

;;; EOF
