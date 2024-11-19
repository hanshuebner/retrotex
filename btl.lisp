;; -*- Lisp -*-

(defpackage :btl
  (:use :cl :alexandria))

(in-package :btl)

(defun decode-blatt-kennzeichen (n)
  (code-char (+ #.(char-code #\a) -1 n)))

(defgeneric decode-field (buf type length offset bit-number))

(defmethod decode-field (buf (type (eql 'bd:bin)) length offset bit-number)
  (if (< length 1)
      (ldb (byte (floor (* 8 length)) bit-number) (aref buf offset))
      (let ((result 0))
        (dotimes (i length result)
          (setf result (logior result (ash (aref buf (+ offset i)) (* i 8))))))))

(defmethod decode-field (buf (type (eql 'bd:bit)) length offset bit-number)
  (assert (= length 1/8) () "bit field must have length 1/8")
  (plusp (ldb (byte 1 (- 7 bit-number)) (aref buf offset))))

(defmethod decode-field (buf (type (eql 'bd:bcd)) length offset bit-number)
  (with-output-to-string (*standard-output*)
    (dotimes (i length)
      (let* ((byte (aref buf (+ offset i)))
             (nibble-1 (ldb (byte 4 4) byte))
             (nibble-2 (ldb (byte 4 0) byte)))
        (write-char (code-char (+ #.(char-code #\0) nibble-1)))
        (write-char (code-char (+ #.(char-code #\0) nibble-2)))))))

(defmethod decode-field (buf (type (eql 'bd:bcd+)) length offset bit-number)
  (let ((result (with-output-to-string (*standard-output*)
                  (dotimes (i (min length 8))
                    (let* ((byte (aref buf (+ offset i)))
                           (nibble-1 (ldb (byte 4 4) byte))
                           (nibble-2 (ldb (byte 4 0) byte)))
                      (when (zerop nibble-1)
                        (return))
                      (write-char (code-char (+ #.(char-code #\0) (1- nibble-1))))
                      (when (zerop nibble-2)
                        (return))
                      (write-char (code-char (+ #.(char-code #\0) (1- nibble-2))))))
                  (when (> length 8)
                    (write-char (decode-blatt-kennzeichen (aref buf (+ offset 8))))))))
    (unless (= 1 (length result))
      result)))

(defmethod decode-field (buf (type (eql 'bd:cept)) length offset bit-number)
  (flex:octets-to-string buf :start offset :end (+ offset length)))

(defmethod decode-field (buf (type (eql 'bd:bits)) length offset bit-number)
  ;; BITS wird nur verwendet, um Vorder- und Hintergrundfarben zu definieren (SKOFAZE1 und SKOFAZE4)
  (let ((byte (aref buf offset)))
    (list (ldb (byte 4 4) byte) (ldb (byte 4 0) byte))))

(defun decode-bcd+ (buffer &optional (offset 0))
  (decode-field buffer
                'bd:bcd+ 8 offset 0))

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
                           `((slot-value btl ',name) (decode-field buffer
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
    (format stream "~A (~A)" (SKOSNRBP btl) (btl-page-type btl))))

(defmethod btl-choice-mapping ((btl btl))
  (append (list "#" "0")
          (if (SKOISEIT btl)
              (if (SKOWM2ST btl)
                  (loop for i from 10 upto 99 collect (princ-to-string i))
                  (loop for i from 1 upto 9 collect (princ-to-string i)))
              (list "2" "19"))))

(defmethod btl-choices ((btl btl))
  (when (SKOAWMDA btl)
    (loop with buffer = (SKOAM btl)
          with mapping = (btl-choice-mapping btl)
          with SAMHWAMO = (aref buffer 0)
          with SAMAWAMO = (aref buffer 1)
          with slot-length = (if (SKOQSAM2 btl) 10 8)
          for i from 0 below SAMHWAMO
          for slot = (1- (aref buffer (+ i 2)))
          for base = (+ 2 SAMHWAMO (* slot slot-length))
          unless (minusp slot)
            collect (list (nth i mapping)
                          (if (SKOQSAM2 btl)
                              (unless (zerop (aref buffer (1+ base)))
                                (format nil "~A ~A ~A"
                                        (aref buffer base)
                                        (decode-bcd+ buffer (1+ base))
                                        (decode-blatt-kennzeichen (aref buffer (+ base 9)))))
                              (unless (zerop (aref buffer base))
                                (decode-bcd+ buffer base)))))))

(defun print-btl (btl)
  (format t "~&~A
~:[~;SKOWM2ST 2-stellige Wahlmöglichkeit
~]~:[~;SKOAWMDA Auswahlmöglichkeit
~]~:[~;SKOQSAM2 Auswahlmöglichkeit mit BKZ und Bl.
~]~@[SKOSNRMT Mutterseite ~A
~]~@[SKOSDRQ1 Decoder-Definition 1: ~A
~]~@[SKOSDRQ2 Decoder-Definition 2: ~A
~]~@[SKOSDRQ3 Decoder-Definition 3: ~A
~]~@[SKODR Dekoder-Daten: ~A
~]~@[SKOAC Aufbaucode: ~A
~]~@[Auswahlmöglichkeiten: ~A
~]"
          btl
          (SKOWM2ST btl)
          (SKOAWMDA btl)
          (SKOQSAM2 btl)
          (SKOSNRMT btl)
          (SKOSDRQ1 btl)
          (SKOSDRQ2 btl)
          (SKOSDRQ3 btl)
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
                         (equal page (SKOSNRBP btl)))
                 (print-btl btl))))))

