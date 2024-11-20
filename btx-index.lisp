;; -*- Lisp -*-

(defpackage :btx-index
  (:use :cl :alexandria)
  (:export
   #:get-entry))

(in-package :btx-index)

(named-readtables:in-readtable :interpol-syntax)

(defun read-index (&optional (filename "schlagwortverzeichnis.txt"))
  (let ((map (make-hash-table)))
    (dolist (line (uiop:read-file-lines filename) map)
      (multiple-value-bind (match regs) (ppcre:scan-to-strings #?r"^(.*) (JA|NEIN) (\d+)$" line)
        (if match
            (destructuring-bind (keyword is-ad number) (coerce regs 'list)
              (setf (gethash (parse-integer number) map) (list keyword (equal is-ad "JA"))))
            (warn "unparseable: ~S" line))))))

(defparameter *index* (read-index))

(defun get-entry (n)
  (gethash n *index*))
