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

(defvar *client-handler* 'hello-world-handler)

(defun handle-client (stream)
  (when *bind-last-client-p*
    (setf cept:*cept-stream* stream)
    (format t "; bound *cept-stream* to ~A~%" stream))
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
                          ;; just # -> lookup "#" choice, otherwise stay on this page
                          ((zerop (length input))
                           (gethash "#" (page:page-choices page) page))
                          ;; *# -> this page (?)
                          ((equal input "*")
                           page)
                          ;; *<digit...># -> find page
                          ((eql (aref input 0) #\*)
                           (gethash (subseq input 1) (page:page-db page)))
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
             ;; check choice
             (when (ppcre:scan "^[0-9]" input)
               (when-let (page (gethash input (page:page-choices page)))
                 (format t "; shortcut ~S => ~A~%" input page)
                 (return page))
               (when (= (length input) 2)
                 ;; two digits entered, no shortcut found
                 (format t "; no shortcut ~S found, returning ~A~%" input page)
                 (return page))))))
    (cept:service-jump-return)))

(defun hello-world-handler (stream)
  (cept:with-cept-stream (stream)
    (cept:write-cept "Hello world! "))
  (loop until (= (read-byte stream) (char-code #\q))))

(defun start ()
  (format t "; starting serial server~%")
  (rafi:start-serial-server *client-handler*)
  (format t "; starting web server~%")
  (webserver:start *client-handler*)
  (format t "; starting tcp server~%")
  (tcp-server:start *client-handler*))
