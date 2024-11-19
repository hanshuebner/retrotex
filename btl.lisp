;; -*- Lisp -*-

(defpackage :btl
  (:use :cl :alexandria))

(in-package :btl)

(named-readtables:in-readtable :interpol-syntax)

(defconstant +btl-size+ 206)

(defclass field ()
  ((name :initarg :name :reader field-name)
   (level :initarg :level :reader field-level)
   (type :initarg :type :reader field-type)
   (length :initarg :length :reader field-length)
   (bit-number :initarg :bit-number :initform nil :reader field-bit-number)
   (offset :initarg :offset :reader field-offset)
   (description :initarg :description :reader field-description)
   (usage :initarg :usage :reader field-usage)))

(defmethod print-object ((field field) stream)
  (print-unreadable-object (field stream :type t)
    (with-slots (name level type length offset bit-number description usage) field
      (format stream "name ~S level ~D type ~S length ~D offset ~S bit-number ~D descripton ~S usage ~S"
              name level type length offset bit-number description usage))))

(defun parse-usage (s)
  (unless (equal s "")
    (ecase (intern (string-upcase s))
      (m 'mandatory)
      (o 'optional))))

(defun parse-btl-definitions (&optional (input-file "btl-header.txt"))
  (mapcar (lambda (line)
            (multiple-value-bind (match regs)
                (ppcre:scan-to-strings #?r"([A-Z][A-Z0-9]+) (\d) (|BIN|BIT|BCD|BCD\+|BITS|CEPT) ?(\d+|\d Bits?) (\d+(?:\(\d\)|)) (.*?) ?([mo]|)$"
                                       line)
              (unless match
                (error "not matched: ~A~%" line))
              (destructuring-bind (name level type length offset descripton usage) (coerce regs 'list)
                (let* ((bitsp (ppcre:scan #?r" Bit$" length))
                       (bit-number (when bitsp
                                     (multiple-value-bind (match regs) (ppcre:scan-to-strings #?r"^(\d+)\((\d)\)$" offset)
                                       (assert match)
                                       (setf offset (aref regs 0))
                                       (parse-integer (aref regs 1))))))
                  (multiple-value-bind (match regs) (ppcre:scan-to-strings #?r"(\d) Bit$" length)
                    (setf length (if match
                                     (/ (parse-integer (aref regs 0)) 8)
                                     (parse-integer length :junk-allowed t))))
                  (make-instance 'field
                                 :name (intern name)
                                 :level (parse-integer level)
                                 :type (unless (equal type "")
                                         (intern type))
                                 :length length
                                 :offset (parse-integer offset)
                                 :bit-number bit-number
                                 :description descripton
                                 :usage (parse-usage usage))))))
          (uiop:read-file-lines input-file)))

(defvar *btl-definitions* (remove-if 'null (parse-btl-definitions) :key #'field-type))

(defgeneric get-btl-field (buf type length offset bit-number))

(defmethod get-btl-field (buf (type (eql 'bin)) length offset bit-number)
  (if (< length 1)
      (ldb (byte (floor (* 8 length)) bit-number) (aref buf offset))
      (let ((result 0))
        (dotimes (i length result)
          (setf result (logior result (ash (aref buf (+ offset i)) (* i 8))))))))

(defmethod get-btl-field (buf (type (eql 'bit)) length offset bit-number)
  (assert (= length 1/8) () "bit field must have length 1/8")
  (plusp (ldb (byte 1 bit-number) (aref buf offset))))

(defmethod get-btl-field (buf (type (eql 'bcd)) length offset bit-number)
  (with-output-to-string (*standard-output*)
    (dotimes (i length)
      (let* ((byte (aref buf (+ offset i)))
             (nibble-1 (ldb (byte 4 4) byte))
             (nibble-2 (ldb (byte 4 0) byte)))
        (write-char (code-char (+ #.(char-code #\0) nibble-1)))
        (write-char (code-char (+ #.(char-code #\0) nibble-2)))))))

(defmethod get-btl-field (buf (type (eql 'bcd+)) length offset bit-number)
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

(defmethod get-btl-field (buf (type (eql 'cept)) length offset bit-number)
  (flex:octets-to-string buf :start offset :end (+ offset length)))

(defmethod get-btl-field (buf (type (eql 'bits)) length offset bit-number)
  ;; BITS wird nur verwendet, um Vorder- und Hintergrundfarben zu definieren (SKOFAZE1 und SKOFAZE4)
  (let ((byte (aref buf offset)))
    (list (ldb (byte 4 4) byte) (ldb (byte 4 0) byte))))

(defun make-btl-binding (buffer-var definition)
  (with-slots (name type length offset bit-number) definition
    `(,name (get-btl-field ,buffer-var ',type ,length ,offset ,bit-number))))

(defmacro with-btl ((buffer) &body body)
  (with-gensyms (b)
    `(let ((,b ,buffer))
      (assert (>= (length ,buffer) +btl-size+) () "BTL buffer too small, must be at least ~A bytes" +btl-size+)
      (let ,(mapcar (lambda (definition) (make-btl-binding b definition)) (remove-if 'null *btl-definitions* :key #'field-type))
        (declare (ignorable ,@(mapcar #'field-name *btl-definitions*)))
        ,@body))))

(defun print-btl (btl)
  (with-btl (btl)
    (format t "~&~A~C~%" SKOSNRBP (code-char (+ #.(char-code #\a) (1- SKOBLAKZ))))))

(defun print-btl-directory (pathname)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with btl = (make-array +btl-size+)
          for i from #x2000 below (file-length f) by #x0800
          do (file-position f i)
             (read-sequence btl f)
             (print-btl btl))))

