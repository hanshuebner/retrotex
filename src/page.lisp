;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:page
           #:display
           #:load-pages
           #:choices
           #:nummer
           #:handle-input
           #:make-session
           #:decoder-pages
           #:session
           #:impressum
           #:preis
           #:display-system-line))

(in-package :page)

(defclass session ()
  ((cept-stream :initarg :cept-stream :reader cept-stream)
   (decoder-pages :initform nil :reader decoder-pages)))

(defmacro with-session-stream ((session) &body body)
  `(cept:with-cept-stream ((cept-stream ,session))
     ,@body))

(defun make-session (stream)
  (make-instance 'session :cept-stream stream))

(defclass page ()
  ((nummer :initarg :nummer
           :reader nummer
           :documentation "Seitennummer inklusive Blattkennzeichen")
   (choices :initarg :choices
            :reader choices
            :documentation "Hashtable Auswahlstring => Seitennummer")))

(defgeneric display (page session)
  (:documentation "Ausgeben der angegebenen Seite")
  (:method :around (page session)
    (with-session-stream (session)
      (call-next-method))))

(defgeneric handle-input (page session)
  (:documentation "Verabeiten von Benutzereingaben auf der Seite")
  (:method :around (page session)
    (with-session-stream (session)
      (call-next-method))))

(defgeneric impressum (page)
  (:documentation "Gibt den Impressums-String für die erste Zeile zurück")
  (:method (page)
    ""))

(defgeneric preis (page)
  (:documentation "Gibt den Seitenpreis in Pfennig zurück")
  (:method (page) 0))

(defmethod display :before ((page page) session)
  (cept:service-break)
  (let ((nummer (nummer page)))
    (cept:goto 23 (- 40 (length nummer)))
    (cept:write-cept nummer))
  (cept:service-break-return))

(defmethod display :after ((page page) session)
  (cept:service-break)
  (cept:goto 0 0)
  (cept:write-cept (impressum page))
  (cept:goto 0 32)
  (cept:write-cept (format nil " ~D,~2,'0D DM"  (floor (preis page) 100) (mod (preis page) 100)))
  (cept:service-break-return)
  (cept:goto 23 0))

(defun display-system-line (&rest things)
  (cept:service-break)
  (cept:goto 23 0)
  (apply 'cept:write-cept things)
  (cept:goto 23 0)
  (cept:service-break-return))

(defmethod clear-input-area ()
  (cept:goto 23 0)
  (cept:delete-to-end-of-line))

(defmethod handle-input ((page page) session)
  (format t "; handle input, defined choices ~A~%" (sort (hash-table-keys (choices page)) #'string-lessp))
  (let ((input (make-array 24 :fill-pointer 0 :element-type 'character))
        first-char)
    (loop
      (let* ((input-byte (read-byte cept:*cept-stream*))
             (input-char (char-upcase (code-char input-byte))))
        (when (= 1 (length input))
          (setf first-char (aref input 0)))
        (cond
          ;; * => clear input
          ((= input-byte #x13)
           (clear-input-area)
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
            (setf (fill-pointer input) 0)))))))
