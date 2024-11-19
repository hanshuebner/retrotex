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
         (,@(mapcar (lambda (name)
                      (list name :reader name))
                    field-names)))
       (defmethod initialize-instance ((btl btl) &key buffer)
         (setf ,@(mapcan (lambda (name field)
                           `((slot-value btl ',name) (get-btl-field buffer
                                                    ',(bd:field-type field)
                                                    ,(bd:field-length field)
                                                    ,(bd:field-offset field)
                                                    ,(bd:field-bit-number field))))
                         field-names bd:*btl-definitions*))))))

(define-btl-class)

(defmethod btl-page-number ((btl btl))
  (format nil "~A~C" (SKOSNRBP btl) (code-char (+ #.(char-code #\a) (1- (SKOBLAKZ btl))))))

(defun print-btl (buffer)
  (let ((btl (make-instance 'btl :buffer buffer)))
    (format t "~&~A~%" (btl-page-number btl))))

(defun print-btl-directory (pathname)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with buffer = (make-array bd:+btl-size+)
          for i from #x2000 below (file-length f) by #x0800
          do (file-position f i)
             (read-sequence buffer f)
             (print-btl buffer))))

