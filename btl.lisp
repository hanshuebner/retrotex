;; -*- Lisp -*-

(defpackage :btl
  (:use :cl :alexandria))

(in-package :btl)

(defgeneric get-btl-field (buf type length offset bit-number))

(defmethod get-btl-field (buf (type (eql 'bd:bin)) length offset bit-number)
  (if (< length 1)
      (ldb (byte (floor (* 8 length)) bit-number) (aref buf offset))
      (let ((result 0))
        (dotimes (i length result)
          (setf result (logior result (ash (aref buf (+ offset i)) (* i 8))))))))

(defmethod get-btl-field (buf (type (eql 'bd:bit)) length offset bit-number)
  (assert (= length 1/8) () "bit field must have length 1/8")
  (plusp (ldb (byte 1 (- 7 bit-number)) (aref buf offset))))

(defmethod get-btl-field (buf (type (eql 'bd:bcd)) length offset bit-number)
  (with-output-to-string (*standard-output*)
    (dotimes (i length)
      (let* ((byte (aref buf (+ offset i)))
             (nibble-1 (ldb (byte 4 4) byte))
             (nibble-2 (ldb (byte 4 0) byte)))
        (write-char (code-char (+ #.(char-code #\0) nibble-1)))
        (write-char (code-char (+ #.(char-code #\0) nibble-2)))))))

(defmethod get-btl-field (buf (type (eql 'bd:bcd+)) length offset bit-number)
  (with-output-to-string (*standard-output*)
    (dotimes (i length)
      (let* ((byte (aref buf (+ offset i)))
             (nibble-1 (ldb (byte 4 4) byte))
             (nibble-2 (ldb (byte 4 0) byte)))
        (when (zerop nibble-1)
          (return))
        (write-char (code-char (+ #.(char-code #\0) (1- nibble-1))))
        (when (zerop nibble-2)
          (return))
        (write-char (code-char (+ #.(char-code #\0) (1- nibble-2))))))))

(defmethod get-btl-field (buf (type (eql 'bd:cept)) length offset bit-number)
  (flex:octets-to-string buf :start offset :end (+ offset length)))

(defmethod get-btl-field (buf (type (eql 'bd:bits)) length offset bit-number)
  ;; BITS wird nur verwendet, um Vorder- und Hintergrundfarben zu definieren (SKOFAZE1 und SKOFAZE4)
  (let ((byte (aref buf offset)))
    (list (ldb (byte 4 4) byte) (ldb (byte 4 0) byte))))

(defmacro define-btl-class ()
  (let ((field-names (mapcar (lambda (field)
                               (intern (string (bd:field-name field))))
                             bd:*btl-definitions*)))
    `(progn
       (defclass btl ()
         ((buffer :initarg :buffer :reader buffer)
          ,@(mapcar (lambda (name)
                      (list name :reader name))
                    field-names)))
       (defmethod initialize-instance ((btl btl) &key buffer)
         (call-next-method)
         (setf ,@(mapcan (lambda (name field)
                           `((slot-value btl ',name) (get-btl-field buffer
                                                                    ',(bd:field-type field)
                                                                    ,(bd:field-length field)
                                                                    ,(bd:field-offset field)
                                                                    ,(bd:field-bit-number field))))
                         field-names bd:*btl-definitions*))))))

(defmacro define-range-reader (name)
  (let ((ptr (find-symbol (format nil "~APTR" name)))
        (len (find-symbol (format nil "~ALEN" name))))
    `(defmethod ,name ((btl btl))
       (unless (zerop (,ptr btl))
         (subseq (buffer btl) (,ptr btl) (+ (,ptr btl) (,len btl)))))))

(define-btl-class)

(define-range-reader SKOAM)
(define-range-reader SKODR)
(define-range-reader SKOAC)
(define-range-reader SKOAS)
(define-range-reader SKOFB)
(define-range-reader SKOPM)
(define-range-reader SKOBV)
(define-range-reader SKOTD)

(defmethod btl-page-number ((btl btl))
  (format nil "~A~C" (SKOSNRBP btl) (code-char (+ #.(char-code #\a) (1- (SKOBLAKZ btl))))))

(defmethod btl-page-type ((btl btl))
  (cond
    ((SKOISEIT btl) "information")
    ((SKODSEIT btl) "dialog")
    ((SKOMSEIN btl) "message")
    ((SKOGWSEI btl) "transfer")
    ((SKOFSSER btl) "format service")
    ((SKOBBSEI btl) "billboard")
    (t (format nil "unknown ~2,'0X" (aref (buffer btl) 34)))))

(defmethod print-object ((btl btl) stream)
  (print-unreadable-object (btl stream :type t :identity t)
    (format stream "~A (~A)" (btl-page-number btl) (btl-page-type btl))))

(defmethod btl-choice-mapping ((btl btl))
  (append (list "#" "0")
          (if (SKOISEIT btl)
              (if (SKOWM2ST btl)
                  (loop for i from 10 upto 99 collect (princ-to-string i))
                  (loop for i from 1 upto 9 collect (princ-to-string i)))
              (list "2" "19"))))

(defmethod decode-bdhqsam2 ((btl btl))
  )

(defmethod decode-bdhqsam ((btl btl))
  (loop with buffer = (SKOAM btl)
        with mapping = (btl-choice-mapping btl)
        with SAMHWAMO = (aref buffer 0)
        with SAMAWAMO = (aref buffer 1)
        for i from 0 below SAMHWAMO
        unless (zerop (aref buffer i))
          collect (list (nth i mapping)
                        (let* ((slot (aref buffer (+ i 2)))
                               (base (+ 3 SAMAWAMO (* slot 8))))
                          (ignore-errors (get-btl-field (subseq buffer base (+ base 8))
                                                        'bd:bcd+ 8 0 0))))))

(defmethod btl-choices ((btl btl))
  (when (SKOAM btl)
    (if (SKOQSAM2 btl)
        (decode-bdhqsam2 btl)
        (decode-bdhqsam btl))))

(defun print-btl (btl)
  (format t "~&~A
SKOWM2ST 2-stellige Wahlmöglichkeit: ~A
SKOAWMDA Auswahlmöglichkeit: ~A
SKOQSAM2 Auswahlmöglichkeit mit BKZ und Bl.: ~A
SKOAM Auswahlmöglichkeit: ~A
SKODR Dekoder-Daten: ~A
SKOAC Aufbaucode: ~A
Auswahlmöglichkeiten: ~A
"
          btl
          (SKOWM2ST btl)
          (SKOAWMDA btl)
          (SKOQSAM2 btl)
          (SKOAM btl)
          (SKODR btl)
          (SKOAC btl)
          (btl-choices btl)))

(defun print-btl-directory (pathname &optional page)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with buffer = (make-array #x800)
          for i from #x4000 below (file-length f) by #x0800
          do (file-position f i)
             (read-sequence buffer f)
             (let ((btl (make-instance 'btl :buffer buffer)))
               (when (or (not page)
                         (equal page (btl-page-number btl)))
                 (print-btl btl))))))

