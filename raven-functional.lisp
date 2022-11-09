;;;
;;; raven-functional.lisp
;;;
;;; Functional sources and destinations.
;;;

(in-package :raven)

;;; (declaim (inline write-functional-destination))
(defun write-functional-destination (address data)
  (declare (type (unsigned-byte 32) data))
  (case address
    (0 ;; Nop
     )
    (1 ;; Location-counter
     (setf *need-fetch* t)
     (setf *location-counter* (logand #x03ffffff data)))
    (2 ;; MCR
     ;; FIXME: some parts of the MCR aren't supposed to be writable here.
     (setf *machine-control-register* data)
     (check-interrupt-status))
    (3 ;; Micro-stack-pointer
     (setf *microstack-pointer* (logand #x3f data)))
    (4 ;; Micro-stack-data
     (setf (aref *microstack* *microstack-pointer*) (logand #xfffff data)))
    (5 ;; Micro-stack-data-push
     (setf *microstack-pointer* (logand #x3f (1+ *microstack-pointer*)))
     (setf (aref *microstack* *microstack-pointer*) (logand #xfffff data)))
    (6 ;; OA-Reg-Low
     (setf (aref *next-micro-instruction-m*) (logior (aref *next-micro-instruction-m*) data)))
    (7 ;; OA-Reg-High
     (setf *next-micro-instruction-a* (logior *next-micro-instruction-a* (ldb (byte 24 0) data))))
    (#o10 ;; MIB
     (setf (aref *macroinstruction-buffer*) data)
#+nil     (setf *need-fetch* nil))
    (#o17 ;; Test-Synch
#+nil     (setf *machine-control-register* 0)
     (setf (aref *memory-data*) 0)
     (setf *nubus-error* nil)
     (check-interrupt-status))
    (#o20 ;; VMA
     (setf (aref *virtual-memory-address*) data))
    (#o21 ;; VMA-Write-Map-Level-1
     (setf (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))) data)
     (setf (aref *cached-level-1*) data)
     (setf (aref *virtual-memory-address*) data))
    (#o22 ;; VMA-Write-Map-Level-2-Control
     (setf (aref *virtual-memory-address*) data)
     (write-map-level-2-control))
    (#o23 ;; VMA-Write-Map-Level-2-Address
     (setf (aref *virtual-memory-address*) data)
     (write-map-level-2-address))
    (#o24 ;; VMA-Start-Read
     (setf (aref *virtual-memory-address*) data)
     (start-read))
    (#o25 ;; VMA-Start-Write
     (setf (aref *virtual-memory-address*) data)
     (start-write))
    (#o26 ;; VMA-Start-Unmapped-Read
     (setf (aref *virtual-memory-address*) data)
     (start-unmapped-read))
    (#o27 ;; VMA-Start-Unmapped-Write
     (setf (aref *virtual-memory-address*) data)
     (start-unmapped-write))
    (#o30 ;; MD
     (setf (aref *cached-level-1*)
	   (aref *level-1-map* (ldb (byte 12 13) data)))
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data))
    (#o31 ;; MD-Write-Map-Level-1
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (setf (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))) (aref *virtual-memory-address*))
     (setf (aref *cached-level-1*) (aref *virtual-memory-address*)))
    (#o32 ;; MD-Write-Map-Level-2-Control
     (setf (aref *cached-level-1*)
	   (aref *level-1-map* (ldb (byte 12 13) data)))
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (write-map-level-2-control))
    (#o33 ;; MD-Write-Map-Level-2-Address
     (setf (aref *cached-level-1*)
	   (aref *level-1-map* (ldb (byte 12 13) data)))
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (write-map-level-2-address))
    (#o34 ;; MD-Start-Read
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-read))
    (#o35 ;; MD-Start-Write
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-write))
    (#o36 ;; MD-Start-Unmapped-Read
     (setf (aref *cached-level-1*)
	   (aref *level-1-map* (ldb (byte 12 13) data)))
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-unmapped-read))
    (#o37 ;; MD-Start-Unmapped-Write
     (setf (aref *cached-level-1*)
	   (aref *level-1-map* (ldb (byte 12 13) data)))
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-unmapped-write))
    (#o40 ;; C-PDL-BUFFER-POINTER
     (setf (aref *pdl-buffer* *pdl-buffer-pointer*) data))
    (#o41 ;; C-PDL-BUFFER-INDEX
     (setf (aref *pdl-buffer* *pdl-buffer-index*) data))
    (#o44 ;; C-PDL-BUFFER-POINTER-PUSH
     (setf *pdl-buffer-pointer* (logand #x3ff (1+ *pdl-buffer-pointer*)))
     (setf (aref *pdl-buffer* *pdl-buffer-pointer*) data))
    (#o45 ;; C-PDL-BUFFER-INDEX-INCREMENT
     (setf *pdl-buffer-index* (logand #x3ff (1+ *pdl-buffer-index*)))
     (setf (aref *pdl-buffer* *pdl-buffer-index*) data))
    (#o50 ;; PDL-BUFFER-POINTER
     (setf *pdl-buffer-pointer* (logand #x3ff data)))
    (#o51 ;; PDL-BUFFER-INDEX
     (setf *pdl-buffer-index* (logand #x3ff data)))
    (#o66 ;; VMA-Start-Unmapped-Byte-Read
     (setf (aref *virtual-memory-address*) data)
     (start-unmapped-byte-read))
    (#o67 ;; VMA-Start-Unmapped-Byte-Write
     (setf (aref *virtual-memory-address*) data)
     (start-unmapped-byte-write))
    (#o76 ;; MD-Start-Unmapped-Byte-Read
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-unmapped-byte-read))
    (#o77 ;; MD-Start-Unmapped-Byte-Write
     (setf (aref nevermore::*memory-data*) data)
     (setf (aref *memory-data*) data)
     (start-unmapped-byte-write))
    (t ;; Unhandled or bogus
     (format t "Functional destination #o~o write #x~x.~%" address data)))
  (values))

(declaim (inline read-functional-source))
(defun read-functional-source (address)
  (case address
    (0 ;; VMA
     (aref *virtual-memory-address*))
    (1 ;; Q-R
     (aref *q-register*))
    (2 ;; MIB-argument-offset-field
     (ldb (byte 6 (* 16 (- 1 (ldb (byte 1 0) *location-counter*))))
	  (aref *macroinstruction-buffer*)))
    (3 ;; Micro-stack-pointer
     (logand #x3f *microstack-pointer*))
    (4 ;; MCR
     (dpb *current-interrupt-level* (byte 4 16)
	  (logior (ash (logand #xf (logxor *cpu-nubus-slot* #xf)) 28)
		  (if *need-fetch* #x00400000 0)
		  (if *loop-selftest* 0 #x00800000)
		  (logand #x0f3fffff *machine-control-register*))))
    (5 ;; Location-counter
     *location-counter*)
    (6 ;; Memory-map-level-2-address
     (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))))
	    (map-2-address (dpb map-1 (byte 7 5)
				(ldb (byte 5 8) (aref *memory-data*)))))
       (logand #x003fffff (aref *level-2-address* map-2-address))))
    (7 ;; I-Arg
     *dispatch-constant*)
    (#o10 ;; Memory-Map-Level-1
     (logand #xffff (aref *cached-level-1*) #+nil(aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*)))))
    (#o11 ;; Memory-map-level-2-control
     (let* ((map-1 (aref *level-1-map* (ldb (byte 12 13) (aref *memory-data*))))
	    (map-2-address (dpb map-1 (byte 7 5)
				(ldb (byte 5 8) (aref *memory-data*)))))
       (logand #xffff (aref *level-2-control* map-2-address))))
    (#o12 ;; MIB
     (if (= 1 (ldb (byte 1 0) *location-counter*))
	 (logand #xffff (aref *macroinstruction-buffer*))
	 (ldb (byte 16 16) (aref *macroinstruction-buffer*))
       #+nil (logior (ldb (byte 16 16) (aref *macroinstruction-buffer*))
	       (dpb (aref *macroinstruction-buffer*) (byte 16 16) 0))))
    (#o13 ;; MIB-branch-offset-field
     (ldb (byte 9 (* 16 (- 1 (ldb (byte 1 0) *location-counter*))))
	  (aref *macroinstruction-buffer*)))
    (#o20 ;; Micro-stack-data
     (aref *microstack* *microstack-pointer*))
    (#o21 ;; Micro-stack-data-pop
     (setf *microstack-pointer* (logand #x3f (1- *microstack-pointer*)))
     (aref *microstack* (logand (+ *microstack-pointer* 1) #x3f)))
    (#o22 ;; MD
     (when (and (not (zerop *memory-busy*))
		(< #+nil 3 5 *memory-busy*))
       (setf *memory-busy* 0)
       (check-memory-cycle-abort-2))
     (aref *memory-data*))
    (#o40 ;; C-PDL-BUFFER-POINTER
     (aref *pdl-buffer* *pdl-buffer-pointer*))
    (#o41 ;; C-PDL-BUFFER-INDEX
     (aref *pdl-buffer* *pdl-buffer-index*))
    (#o44 ;; C-PDL-BUFFER-POINTER-POP
     (let ((data (aref *pdl-buffer* *pdl-buffer-pointer*)))
       (setf *pdl-buffer-pointer* (logand #x3ff (1- *pdl-buffer-pointer*)))
       data))
    (#o45 ;; C-PDL-BUFFER-INDEX-DECREMENT
     (let ((data (aref *pdl-buffer* *pdl-buffer-index*)))
       (setf *pdl-buffer-index* (logand #x3ff (1- *pdl-buffer-index*)))
       data))
    (#o50 ;; PDL-BUFFER-POINTER
     *pdl-buffer-pointer*)
    (#o51 ;; PDL-BUFFER-INDEX
     *pdl-buffer-index*)
    (#o54 ;; PDL-BUFFER-POINTER-POP
     (prog1
	 *pdl-buffer-pointer*
       (setf *pdl-buffer-pointer* (logand #x3ff (1- *pdl-buffer-pointer*)))))
    (#o55 ;; PDL-BUFFER-INDEX-DECREMENT
     (prog1
	 *pdl-buffer-index*
       (setf *pdl-buffer-index* (logand #x3ff (1- *pdl-buffer-index*)))))
    (t ;; Unhandled or bogus
     (format t "Functional source #o~o read, returning 0.~%" address)
     0)))


;;; EOF
