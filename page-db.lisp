;; -*- Lisp -*-

(defpackage :page-db
  (:use :cl :alexandria))

(in-package :page-db)

(defclass page ()
  ((meta-file :initarg :meta-file :reader page-meta-file)
   (number :initarg :number :reader page-number)
   (sub-page :initarg :sub-page :reader page-sub-page)
   (basename :initarg :basename :reader page-basename)
   (next-page :initarg :next-page :reader page-next-page :initform nil)))

(defmethod following-page-count ((page page))
  (with-slots (next-page) page
    (if next-page
        (1+ (following-page-count next-page))
        0)))

(defmethod print-object ((page page) stream)
  (with-slots (number sub-page basename) page
    (print-unreadable-object (page stream :type t)
      (format stream "~A~A [+~A]" number sub-page (following-page-count page)))))

(defmethod page-file ((page page) type)
  (probe-file (make-pathname :type type :defaults (page-meta-file page))))

(defun make-meta-keyword (s)
  (intern (string-upcase (ppcre:regex-replace-all "_" s "-")) :keyword))

(defun parse-json (string)
  (let ((yason:*parse-object-as* :plist))
    (yason:parse string :object-key-fn 'make-meta-keyword)))

(defun parse-json-file (pathname)
  (let ((sanitized-json-string (ppcre:regex-replace-all ":\\s*\"(true|false)\"" (alexandria:read-file-into-string pathname)
                                                        (lambda (match true-or-false)
                                                          (declare (ignore match))
                                                          (format nil ":~:[false~;true~]" (string-equal true-or-false "true")))
                                                        :simple-calls t)))
    (ignore-errors (parse-json sanitized-json-string))))

(defmethod read-page-file ((page page) type &key error-if-not-found)
  (destructuring-bind (reader default)
      (ecase (find-symbol (string-upcase type) :keyword)
        ((:pal :meta)
         '(parse-json-file nil))
        ((:inc :cept)
         `(read-file-into-byte-vector ,(make-array 0 :element-type '(unsigned-byte 8)))))
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

(defmethod make-cept ((page page))
  (with-slots (meta-file) page
    (flex:with-output-to-sequence (s)
      (cept:with-cept-stream (s)
        (cept:reset-colors)
        (destructuring-bind (&key clear-screen include &allow-other-keys) (read-page-file page "meta")
          (format t "include ~A~%" include)
          (when clear-screen
            (cept:clear-page)))
        (destructuring-bind (&key palette (start-color 16)) (read-page-file page "pal")
          (when palette
            (cept:define-colors start-color (mapcar #'parse-12-bit-web-color palette))))
        (write-sequence (read-page-file page "inc") s)
        (write-sequence (read-page-file page "cept") s)))))

(defmethod make-links ((page page))
  (with-slots (meta-file next-page) page
    (destructuring-bind (&key links &allow-other-keys) (read-page-file page "meta")
      (if (and (not (getf links :|#|)) next-page)
          `(:|#| ,(format nil "~A~A" (page-number next-page) (page-sub-page next-page)) ,@links)
          links))))

(defun make-next-file (meta-file)
  (ppcre:regex-replace "([a-z])(?=\\.meta$)"
                       (namestring meta-file)
                       (lambda (match sub-page)
                         (declare (ignore match))
                         (make-string 1 :initial-element (code-char (1+ (char-code (aref sub-page 0))))))
                       :simple-calls t))

(defun make-page (base-directory meta-file)
  (when-let ((metadata (parse-json-file meta-file)))
    (let* ((basename (ppcre:regex-replace "[a-z]\\.meta$" (enough-namestring meta-file base-directory) ""))
           (page-number (ppcre:regex-replace-all "\\D" basename ""))
           (sub-page (ppcre:regex-replace-all ".*([a-z])\\.meta$" (namestring meta-file) "\\1"))
           (page (make-instance 'page
                                :meta-file meta-file
                                :number page-number
                                :basename basename
                                :sub-page sub-page))
           (next-meta-file (make-next-file meta-file)))
      (when (probe-file next-meta-file)
        (with-slots (next-page) page
          (setf next-page (make-page base-directory next-meta-file))))
      page)))

(defun load-database (directory)
  (let ((page-database (make-hash-table :test #'equal)))
    (dolist (meta-file (directory (merge-pathnames #p"**/*a.meta" directory)))
      (when-let ((page (make-page directory meta-file)))
        (setf (gethash (page-number page) page-database) page)))
    page-database))
