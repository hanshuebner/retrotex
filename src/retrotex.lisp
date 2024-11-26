; -*- Lisp -*-

(cl-interpol:enable-interpol-syntax)

(defpackage :retrotex
  (:use :cl :alexandria)
  (:export #:slideshow
           #:editor
           #:start
           #:show-article-file
           #:handle-client))

(in-package :retrotex)

(defparameter *bind-last-client-p* t)

(defvar *client-handler* 'standard-btx-handler)

(defun handle-client (stream)

  (funcall *client-handler* stream))

(defmacro define-cept-client-handler (() &body body)
  `(setf *client-handler* (lambda ()
                            ,@body)))

(defun slideshow (pages &key sleep)
  (let ((i 0))
    (loop
      (cept:set-screen-format)
      (page:display (nth i pages) cept:*cept-stream*)
      (cond
        (sleep
         (sleep sleep)
         (incf i))
        (t
         (case (code-char (read-byte cept:*cept-stream*))
           ((#\backspace #\delete)
            (when (zerop i)
              (setf i (length pages)))
            (decf i))
           ((#\space #\return)
            (incf i))
           (#\q
            (return-from slideshow)))))
      (when (= i (length pages))
        (setf i 0)))))

(defun read-text-to-articles (filename)
  (let* ((text (alexandria:read-file-into-string filename))
         (paragraphs (ppcre:split #?"\n(?=#)" text)))
    (mapcar (lambda (paragraph)
              (multiple-value-bind (match regs) (ppcre:scan-to-strings #?"#[ \t]*(.*)\n+([\\s\\S]*)" paragraph)
                (declare (ignore match))
                (coerce regs 'list)))
            paragraphs)))

(defun make-text-chunks (text chunk-size)
  (loop with lines = (ppcre:split #?"\n" text)
        while lines
        for this-chunk-size = (if (and (equal (nth (- chunk-size 2) lines) "")
                                       (not (equal (nth (- chunk-size 1) lines) "")))
                                  (- chunk-size 1)
                                  (min chunk-size (length lines)))
        collect (prog1
                    (subseq lines 0 this-chunk-size)
                  (setf lines (nthcdr this-chunk-size lines)))))

(defun chunk-to-page (chunk chunk-size)
  (with-output-to-string (s)
    (loop for i below chunk-size
          collect (format s "~40A" (or (nth i chunk) "")))))

(defun show-article (title text &key (title-row 5) (text-start-row 8) (text-chunk-lines 14) (sleep 10))
  (cept:hide-cursor)
  (cept:goto title-row 0)
  (cept:double-height)
  (cept:goto title-row 0)
  (cept:write-cept (format nil "~40A" title))
  (cept:set-scroll-region text-start-row (+ text-start-row text-chunk-lines))
  (cept:enable-scrolling)
  (cept:goto text-start-row 0)
  (loop for chunk in (make-text-chunks text text-chunk-lines)
        do (cept:write-cept (chunk-to-page chunk text-chunk-lines))
           (finish-output cept:*cept-stream*)
           (if sleep
               (sleep sleep)
               (loop until (eql (code-char (read-byte cept:*cept-stream*)) #\space)))))

#+(or)
(defun show-article-file (&key (text-file "cc-exponate.md") (frame "pages/ccframe.cept") (sleep 15))
  (cept:clear-page)
  (load-page frame)
  (loop
    (dolist (article (read-text-to-articles text-file))
      (destructuring-bind (title text) article
        (show-article title text :sleep sleep)))))

(defun hello-world-handler (stream)
  (cept:with-cept-stream (stream)
    (cept:write-cept "Hello world! "))
  (loop until (= (read-byte stream) (char-code #\q))))

(defun make-page-directory (pages)
  (let ((directory (make-hash-table :test #'equal)))
    (dolist (page pages directory)
      (setf (gethash (page:nummer page) directory) page))))

(defparameter *default-btl-pathname* #P"ccc.btl")

(defun confirm-payment (preis)
  (page:display-system-line (format nil "Anzeigen für DM ~D,~2,'0D? Ja: #" (floor preis 100) (mod preis 100)))
  (equal (read-byte cept:*cept-stream*) #x1c))

(defun standard-btx-handler (stream)
  (when *bind-last-client-p*
    (setf cept:*cept-stream* stream)
    (format t "; bound *cept-stream* to ~A~%" stream))
  (let* ((pages (make-page-directory (btl-page:load-btl *default-btl-pathname*)))
         (page (gethash "655321a" pages))
         (session (page:make-session stream)))
    (loop
      (page:display page session)
      (let* ((next-page-nummer (page:handle-input page session))
             (next-page (gethash next-page-nummer pages)))
        (cond
          (next-page
           (when (or (zerop (page:preis next-page))
                     (confirm-payment (page:preis next-page)))
             (setf page next-page)))
          (t
           (page:display-system-line "Seite nicht vorhanden")
           (sleep 1)))))))

#+(or)
(defun instance-diff (a b)
  (dolist (slot (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of a))))
    (unless (equalp (slot-value a slot) (slot-value b slot))
      (format t "~A ~S <=> ~S~%" slot (slot-value a slot) (slot-value b slot)))))

#+(or)
(defun show-instance (i)
  (dolist (slot (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of i))))
    (let ((value (slot-value i slot)))
      (unless (or (equal 0 value) (not value))
        (format t "~A => ~S~%" slot (slot-value i slot))))))

(defun start ()
  (format t "; starting serial server~%")
  (rafi:start-serial-server *client-handler*)
  (format t "; starting web server~%")
  (webserver:start *client-handler*)
  (format t "; starting tcp server~%")
  (tcp-server:start *client-handler*))
