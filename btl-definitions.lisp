;; -*- Lisp -*-

(defpackage :btl-definitions
  (:nicknames :bd)
  (:use :cl :alexandria)
  (:export
   #:field
   #:field-name
   #:field-level
   #:field-type
   #:field-length
   #:field-bit-number
   #:field-offset
   #:field-description
   #:field-usage
   #:bin
   #:bit
   #:bcd
   #:bcd+
   #:bits
   #:cept
   #:parse-layout-file))

(in-package :btl-definitions)

(named-readtables:in-readtable :interpol-syntax)

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
    (ecase (intern (string-upcase s) :keyword)
      (:m 'mandatory)
      (:o 'optional))))

(defun parse-layout-file (input-file)
  (let* ((lines (uiop:read-file-lines input-file))
         (definitions (mapcar (lambda (line)
                                (multiple-value-bind (match regs)
                                    (ppcre:scan-to-strings #?r"([A-Z][A-Z0-9]+) (\d) (|BIN|BIT|BCD|BCD\+|BITS|CEPT) ?(\d+|\d Bits?) ?(\d+(?:\(\d\)|)) (.*?) ?([mo]|)$"
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
                                                     :name (intern name 'btl-definitions)
                                                     :level (parse-integer level)
                                                     :type (unless (equal type "")
                                                             (intern type 'btl-definitions))
                                                     :length length
                                                     :offset (parse-integer offset)
                                                     :bit-number bit-number
                                                     :description descripton
                                                     :usage (parse-usage usage))))))
                              lines)))
    (remove-if 'null definitions :key #'field-type)))
