;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:page
           #:display
           #:load-pages
           #:choices
           #:nummer
           #:handle-input))

(in-package :page)

(defclass page ()
  ((nummer :initarg :nummer
           :reader nummer
           :documentation "Seitennummer inklusive Blattkennzeichen")
   (choices :initarg :choices
            :reader choices
            :documentation "Hashtable Auswahlstring => Seitennummer")))

(defgeneric display (page stream)
  (:documentation "Ausgeben der angegebenen Seite"))

(defgeneric handle-input (page stream)
  (:documentation "Verabeiten von Benutzereingaben auf der Seite"))

(defmethod handle-input ((page page) stream)
  (cept:service-jump)
  (cept:delete-to-end-of-line)
  (format t "; handle input, defined choices ~A~%" (sort (hash-table-keys (choices page)) #'string-lessp))
  (cept:with-cept-stream (stream)
    (unwind-protect
         (let ((input (make-array 40 :fill-pointer 0 :element-type 'character))
               first-char)
           (loop
             (let* ((input-byte (read-byte stream))
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
                             (gethash "#" (choices page) (nummer page)))
                            ;; *# -> this page (?)
                            ((equal input "*")
                             (nummer page))
                            ;; *<digit...># -> find page
                            ((eql (aref input 0) #\*)
                             (format nil "~Aa" (subseq input 1)))
                            ;; stay on page in all other cases
                            (t
                             (nummer page)))))
                 ;; delete => delete character
                 ((member input-char '(#\backspace #\delete))
                  (unless (zerop (length input))
                    (vector-pop input)
                    (cept:write-cept #\backspace #\space #\backspace)))
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
                 (when-let (nummer (gethash input (choices page)))
                   (format t "; shortcut ~S => ~A~%" input nummer)
                   (return nummer))
                 (when (= (length input) 2)
                   ;; two digits entered, no shortcut found
                   (format t "; no shortcut ~S found~%" input)
                   (dotimes (i (length input))
                     (cept:write-cept #\backspace #\space #\backspace))
                   (setf (fill-pointer input) 0))))))
      (cept:service-jump-return))))
