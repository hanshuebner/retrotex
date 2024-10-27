;; -*- Lisp -*-

(defpackage :page-db
  (:use :cl :alexandria))

(in-package :page-db)

(defparameter *default-db-directory* "../bildschirmtext/data/")
(defvar *db*)

(defclass page ()
  ((pathname :initarg :pathname :reader page-pathname)
   (metadata :initarg :metadata :reader page-metadata)
   (number :reader page-number)
   (sub-page :reader page-sub-page)
   (basename :reader page-basename)
   (next-page :reader page-next-page :initform nil)
   (shortcuts :initform (make-hash-table :test #'equal) :reader page-shortcuts)))

(defmethod following-page-count ((page page))
  (with-slots (next-page) page
    (if next-page
        (1+ (following-page-count next-page))
        0)))

(defmethod print-object ((page page) stream)
  (with-slots (number sub-page basename) page
    (print-unreadable-object (page stream :type t)
      (format stream "~A~A [+~A]" number sub-page (following-page-count page)))))

(defun parse-page-number (s)
  (multiple-value-bind (match regs) (ppcre:scan-to-strings "(.*)(.)$" s)
    (when match
      (coerce regs 'list))))

(defmethod make-shortcuts ((page page))
  (with-slots (next-page metadata shortcuts) page
    (doplist (shortcut full-page-number (getf metadata :links))
      (destructuring-bind (page-number sub-page) (parse-page-number full-page-number)
        (if-let (shortcut-page (find-page page-number sub-page))
          (if (string-equal shortcut "#")
              (setf next-page shortcut-page)
              (setf (gethash shortcut shortcuts) shortcut-page))
          (format t "shortcut ~S from page ~A does not point to valid page ~S, ignored~%" shortcut page full-page-number))))))

(defun make-next-file (pathname)
  (ppcre:regex-replace "([a-z])(?=\\.meta$)"
                       (namestring pathname)
                       (lambda (match sub-page)
                         (declare (ignore match))
                         (make-string 1 :initial-element (code-char (1+ (char-code (aref sub-page 0))))))
                       :simple-calls t))

(defmethod initialize-instance :after ((page page) &key pathname base-directory)
  (with-slots (basename number sub-page next-page) page
    (setf basename (ppcre:regex-replace "[a-z]\\.meta$" (enough-namestring pathname base-directory) "")
          number (ppcre:regex-replace-all "\\D" basename "")
          sub-page (ppcre:regex-replace-all ".*([a-z])\\.meta$" (namestring pathname) "\\1")))
  (let ((next-meta-file (make-next-file pathname)))
    (when (probe-file next-meta-file)
      (with-slots (next-page) page
        (setf next-page (make-page base-directory next-meta-file)))))
  (make-shortcuts page))

(defmethod page-file ((page page) type)
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

(defmethod read-page-file ((page page) type &key error-if-not-found)
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

(defmethod make-cept ((page page))
  (with-slots (metadata) page
    (flex:with-output-to-sequence (s)
      (cept:with-cept-stream (s)
        (cept:reset-colors)
        (destructuring-bind (&key clear-screen &allow-other-keys) metadata
          (when clear-screen
            (cept:clear-page)))
        (destructuring-bind (&key palette (start-color 16)) (read-page-file page "pal")
          (when palette
            (cept:define-colors start-color (mapcar #'parse-12-bit-web-color palette))))
        (write-sequence (read-page-file page "inc") s)
        (write-sequence (read-page-file page "cept") s)))))

(defun make-page (base-directory meta-file)
  (when-let ((metadata (parse-json-file meta-file)))
    (make-instance 'page :base-directory base-directory :metadata metadata :pathname meta-file)))

(defun load-database (directory)
  (let ((page-database (make-hash-table :test #'equal)))
    (dolist (meta-file (directory (merge-pathnames #p"**/*a.meta" directory)))
      (when-let ((page (make-page directory meta-file)))
        (setf (gethash (page-number page) page-database) page)
        (dolist (keyword (getf (page-metadata page) :keywords))
          (setf (gethash (string-upcase keyword) page-database) page))))
    page-database))

(defun show-page (page)
  (cept:write-cept (make-cept page)))

(defun find-page (page-or-page-number &optional (sub-page "a"))
  (when-let (page (gethash page-or-page-number *db*))
    (loop
      (when (or (not page)
                (string-equal sub-page (page-sub-page page)))
        (return page))
      (setf page (page-next-page page)))))

(defun handle-input (page)
  (cept:service-jump)
  (cept:delete-to-end-of-line)
  (unwind-protect
       (let ((input (make-array 40 :fill-pointer 0 :element-type 'character))
             first-char)
         (loop
           (let* ((input-byte (read-byte cept:*cept-stream*))
                  (input-char (char-upcase (code-char input-byte))))
             (when (= 1 (length input))
               (setf first-char (aref input 0)))
             (cond
               ;; * => clear input
               ((= input-byte #x13)
                (cept:goto 23 0)
                (cept:delete-to-end-of-line)
                (setf (fill-pointer input) 0)
                (vector-push #\* input)
                (cept:write-cept #\*))
               ;; # => end of input
               ((= input-byte #x1c)
                (cept:write-cept #\#)
                (return (cond
                          ;; just # -> next page
                          ((zerop (length input))
                           (page-next-page page))
                          ;; *# -> this page
                          ((equal input "*")
                           page)
                          ;; *<digit...># -> find page
                          ((eql (aref input 0) #\*)
                           (gethash (subseq input 1) *db*))
                          ;; stay on page in all other cases
                          (t
                           page))))
               ;; delete => delete character
               ((member input-char '(#\backspace #\delete))
                (when (zerop (length input))
                  (return))
                (vector-pop input)
                (cept:write-cept #\backspace #\space #\backspace))
               ;; digits => stuff into buffer
               ((digit-char-p input-char)
                (vector-push input-char input)
                (cept:write-cept input-char))
               ;; * was typed => collect alphanumeric characters
               ((and (eql first-char #\*)
                     (alphanumericp input-char))
                (vector-push input-char input)
                (cept:write-cept input-char)))
             ;; check shortcut
             (when (ppcre:scan "^[0-9]" input)
               (when-let (page-number (gethash input (page-shortcuts page)))
                 (return (find-page page-number)))
               (when (= (length input) 2)
                 ;; two digits entered, no shortcut found
                 (return page))))))
    (cept:service-jump-return)))

(defun handle-client ()
  (let ((page (find-page "0")))
    (loop
      (show-page page)
      (when-let ((new-page (handle-input page)))
        (setf page new-page)))))

(retrotex:define-cept-client-handler ()
  (handle-client))

(setf *db* (load-database *default-db-directory*))
