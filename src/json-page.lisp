;; -*- Lisp -*-

(defpackage :json-page
  (:use :cl :alexandria))

(in-package :json-page)

(defparameter *default-db-directory* "../bildschirmtext/data/")
(defvar *db*)

(defclass json-page (page:page)
  ((pathname :initarg :pathname :reader page-pathname)
   (metadata :initarg :metadata :reader page-metadata)
   (number :reader page-number)
   (sub-page :reader page-sub-page)
   (basename :reader page-basename)
   (next-page :reader page-next-page :initform nil)
   (page-choices :initform nil :reader page-choices)))

(defmethod print-object ((page json-page) stream)
  (with-slots (number sub-page basename next-page) page
    (print-unreadable-object (page stream :type t)
      (format stream "~A~A~:[~; (more)~]" number sub-page next-page))))

(defun parse-page-number (s)
  (multiple-value-bind (match regs) (ppcre:scan-to-strings "(.*)([a-z])$" s)
    (if match
        (coerce regs 'list)
        (list s "a"))))

(defmethod make-choices ((page json-page))
  (with-slots (next-page metadata page-choices) page
    (setf page-choices (make-hash-table :test #'equal))
    (doplist (choice full-page-number (getf metadata :links))
      (destructuring-bind (page-number sub-page) (parse-page-number full-page-number)
        (if-let (choice-page (find-page page-number sub-page))
          (if (string-equal choice "#")
              (setf next-page choice-page)
              (setf (gethash choice page-choices) choice-page))
          (format t "choice ~S from page ~A does not point to valid page ~S, ignored~%" choice page full-page-number))))))

(defmethod page-choices :before ((page json-page))
  (with-slots (choices) page
    (unless choices
      (make-choices page))))

(defun make-next-file (pathname)
  (ppcre:regex-replace "([a-z])(?=\\.meta$)"
                       (namestring pathname)
                       (lambda (match sub-page)
                         (declare (ignore match))
                         (make-string 1 :initial-element (code-char (1+ (char-code (aref sub-page 0))))))
                       :simple-calls t))

(defmethod initialize-instance :after ((page json-page) &key pathname base-directory db)
  (with-slots (basename number sub-page next-page) page
    (setf basename (ppcre:regex-replace "[a-z]\\.meta$" (enough-namestring pathname base-directory) "")
          number (ppcre:regex-replace-all "\\D" basename "")
          sub-page (ppcre:regex-replace-all ".*([a-z])\\.meta$" (namestring pathname) "\\1")))
  (let ((next-meta-file (make-next-file pathname)))
    (when (probe-file next-meta-file)
      (with-slots (next-page) page
        (setf next-page (make-page base-directory next-meta-file db))))))

(defmethod page-file ((page json-page) type)
  (probe-file (make-pathname :type type :defaults (page-pathname page))))

(defun make-meta-keyword (s)
  (or (find-symbol (string-upcase (ppcre:regex-replace-all "_" s "-")) :keyword)
      s))

(defun parse-json (string)
  (let ((yason:*parse-object-as* :plist))
    (loop for (key value) on (yason:parse string) by #'cddr
          collect (make-meta-keyword key)
          collect value)))

(defun parse-json-file (pathname)
  (let ((sanitized-json-string (ppcre:regex-replace-all ":\\s*\"(true|false)\"" (alexandria:read-file-into-string pathname)
                                                        (lambda (match true-or-false)
                                                          (declare (ignore match))
                                                          (format nil ":~:[false~;true~]" (string-equal true-or-false "true")))
                                                        :simple-calls t)))
    (ignore-errors (parse-json sanitized-json-string))))

(defmethod read-page-file ((page json-page) type &key error-if-not-found)
  (destructuring-bind (reader default)
      (ecase (find-symbol (string-upcase type) :keyword)
        ((:pal :meta)
         '(parse-json-file
           nil))
        ((:inc :cept)
         `(read-file-into-byte-vector
           ,(make-array 0 :element-type '(unsigned-byte 8)))))
    (if-let (pathname (page-file page type))
      (funcall reader pathname)
      (if error-if-not-found
          (error "page ~A has no file of type ~A" page type)
          default))))

(defun parse-12-bit-web-color (string)
  (multiple-value-bind (match digits) (ppcre:scan-to-strings "#(.)0(.)0(.)0" string)
    (if match
        (mapcar (lambda (s) (parse-integer s :radix 16)) (coerce digits 'list))
        (list 0 0 0))))

(defmethod make-cept ((page json-page))
  (with-slots (metadata) page
    (flex:with-output-to-sequence (s)
      (cept:with-cept-stream (s)
        (cept:reset-colors)
        (destructuring-bind (&key clear-screen &allow-other-keys) metadata
          (when (not clear-screen)
            (cept:clear-page)))
        (destructuring-bind (&key palette (start-color 16)) (read-page-file page "pal")
          (when palette
            (cept:define-colors start-color (mapcar #'parse-12-bit-web-color palette))))
        (write-sequence (read-page-file page "inc") s)
        (write-sequence (read-page-file page "cept") s)))))

(defun make-page (base-directory meta-file db)
  (when-let ((metadata (parse-json-file meta-file)))
    (make-instance 'json-page :db db
                              :base-directory base-directory
                              :metadata metadata
                              :pathname meta-file)))

(defun load-database (directory db)
  (let ((page-database (make-hash-table :test #'equal)))
    (dolist (meta-file (directory (merge-pathnames #p"**/*a.meta" directory)))
      (when-let ((page (make-page directory meta-file db)))
        (setf (gethash (page-number page) page-database) page)
        (dolist (keyword (getf (page-metadata page) :keywords))
          (setf (gethash (string-upcase keyword) page-database) page))))
    page-database))

(defun find-page (page-or-page-number &optional (sub-page "a"))
  (when-let (page (gethash page-or-page-number *db*))
    (loop
      (when (or (not page)
                (string-equal sub-page (page-sub-page page)))
        (return page))
      (setf page (page-next-page page)))))

(defmethod page:display ((page json-page) stream)
  (format t "; showing page ~A~%" page)
  (cept:with-cept-stream (stream)
    (cept:write-cept (make-cept page))))

