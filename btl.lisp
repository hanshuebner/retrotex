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
  (plusp (ldb (byte 1 bit-number) (aref buf offset))))

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

(defun print-btl (buffer)
  (let ((btl (make-instance 'btl :buffer buffer)))
    (format t "~&~A
SKOWM2ST 2-stellige Wahlmöglichkeit: ~A
SKOAWMDA Auswahlmöglichkeit: ~A
SKOQSAM2 Auswahlmöglichkeit mit BKZ und Bl.: ~A
SKOAM Auswahlmöglichkeit: ~A
SKODR Dekoder-Daten: ~A
SKOAC Aufbaucode: ~A
"
            (btl-page-number btl)
            (SKOWM2ST btl)
            (SKOAWMDA btl)
            (SKOQSAM2 btl)
            (SKOAM btl)
            (SKODR btl)
            (SKOAC btl))))

(defun print-btl-directory (pathname)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with buffer = (make-array #x800)
          for i from #x4000 below (file-length f) by #x0800
          do (file-position f i)
             (read-sequence buffer f)
             (print-btl buffer))))

