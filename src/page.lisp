;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:page
           #:display
           #:choices
           #:nummer
           #:handle-input
           #:decoder-pages
           #:session
           #:impressum
           #:preis
           #:display-system-line
           #:handle-client
           #:display-page
           #:make-page-directory
           #:pages
           #:current-page
           #:goto-page
           #:*session*)
  (:local-nicknames
   (:ee :event-emitter)))

(in-package :page)

(defvar *session*)

(defun make-page-directory (pages)
  (let ((directory (make-hash-table :test #'equal)))
    (dolist (page pages directory)
      (setf (gethash (page:nummer page) directory) page))))

(defconstant +max-page-stack+ 50
  "Anzahl der Seiten, die pro Session für *# gespeichert werden")

(defclass session (ee:event-emitter)
  ((cept-stream :initarg :cept-stream
                :reader cept-stream)
   (decoder-pages :initform nil
                  :reader decoder-pages)
   (stack :initform nil
          :accessor stack
          :documentation "Enthält die aktuelle und bis zu +max-page-stack+ vorher besuchte Seiten")
   (pages :initargs :pages
          :accessor pages)
   (current-page :accessor current-page)
   (input-buffer :initform (make-array 24 :element-type 'character :fill-pointer 0))))

(defmethod session-push-page ((session session) nummer)
  (with-slots (stack) session
    (unless (equal (car stack) nummer)
      (when (> (length stack) +max-page-stack+)
        (setf stack (subseq stack +max-page-stack+)))
      (push nummer stack))))

(defmethod session-pop-page ((session session) nummer)
  (with-slots (stack) session
    (when stack
      (pop stack)
      (car stack))))

(defmacro with-session-stream ((session) &body body)
  `(cept:with-cept-stream ((cept-stream ,session))
     ,@body))

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
    (handler-case
        (with-session-stream (session)
          (call-next-method))
      (end-of-file (e)
        (declare (ignore e))
        (throw 'stream-closed nil)))))

(defgeneric line-1-colors (page)
  (:method (page) (list 7 0)))

(defgeneric line-24-colors (page)
  (:method (page) (list 7 0)))

(defgeneric impressum (page)
  (:documentation "Gibt den Impressumstext für die erste Zeile zurück")
  (:method (page)
    #())
  (:method :around (page)
    (subseq (flex:with-output-to-sequence (s)
              (write-sequence (call-next-method) s)
              (write-sequence #.(flex:string-to-octets (make-string 32 :initial-element #\space)) s))
            0 32)))

(defgeneric preis (page)
  (:documentation "Gibt den Seitenpreis in Pfennig zurück")
  (:method (page) 0))

(defmethod display :before ((page page) session)
  (cept:service-break)
  (cept:goto 23 0)
  (set-foreground-and-background-colors (line-24-colors page))
  (cept:write-cept #.(make-string 40 :initial-element #\space))
  (cept:service-break-return))

(defun set-foreground-and-background-colors (colors)
  (destructuring-bind (foreground-color background-color) colors
    (cept:foreground-color foreground-color)
    (cept:background-color background-color)))

(defmethod display :after ((page page) session)
  (cept:service-break)
  (cept:goto 0 0)
  (set-foreground-and-background-colors (line-24-colors page))
  (cept:write-cept (impressum page))
  (cept:goto 0 32)
  (cept:write-cept (format nil " ~D,~2,'0D DM"  (floor (preis page) 100) (mod (preis page) 100)))
  (let ((nummer (nummer page)))
  (set-foreground-and-background-colors (line-24-colors page))
    (cept:goto 23 (- 39 (length nummer)))
    (cept:write-cept #\space nummer)
    (session-push-page session nummer))
  (cept:service-break-return))

(defun display-system-line (&rest things)
  (cept:service-break)
  (cept:goto 23 0)
  (cept:write-cept (make-string 32 :initial-element #\space))
  (cept:goto 23 0)
  (apply 'cept:write-cept things)
  (cept:goto 23 0)
  (cept:service-break-return))

(defmethod clear-input-area ()
  (cept:goto 23 0)
  (cept:delete-to-end-of-line))

(defmethod handle-input-byte (page session input-byte)
  (with-slots (input-buffer) session
    (when (zerop (length input-buffer))
      (cept:goto 23 0))
    (let ((input-char (char-upcase (code-char input-byte))))
      (cond
        ;; * => clear input
        ((= input-byte #x13)
         (clear-input-area)
         (setf (fill-pointer input-buffer) 0)
         (vector-push #\* input-buffer)
         (cept:write-cept #\*)
         nil)
        ;; # => end of input
        ((= input-byte #x1c)
         (cept:write-cept #\#)
         (cond
           ;; just # -> lookup "#" choice, otherwise stay on this page
           ((zerop (length input-buffer))
            (gethash "#" (choices page) (nummer page)))
           ;; *# -> this page (?)
           ((equal input-buffer "*")
            (or (session-pop-page session (nummer page))
                (nummer page)))
           ;; *<digit...># -> find page
           ((eql (aref input-buffer 0) #\*)
            (format nil "~Aa" (subseq input-buffer 1)))
           ;; stay on page in all other cases
           (t
            (nummer page))))
        ;; delete => delete character
        ((member input-char '(#\backspace #\delete))
         (unless (zerop (length input-buffer))
           (vector-pop input-buffer)
           (cept:write-cept #\backspace #\space #\backspace))
         nil)
        ;; digits => stuff into buffer
        ((digit-char-p input-char)
         (vector-push input-char input-buffer)
         (cept:write-cept input-char)
         nil)
        ;; * was typed => collect alphanumeric characters
        ((and (plusp (length input-buffer))
              (eql (aref input-buffer 0) #\*)
              (alphanumericp input-char))
         (vector-push input-char input-buffer)
         (cept:write-cept input-char)
         nil))
      ;; check choice
      (when (ppcre:scan "^[0-9]" input-buffer)
        (let ((nummer (gethash input-buffer (choices page))))
          (cond
            (nummer
             (format t "; shortcut ~S => ~A~%" input-buffer nummer)
             nummer)
            (t
             (when (= (length input-buffer) 2)
               ;; two digits entered, no shortcut found
               (format t "; no shortcut ~S found~%" input-buffer)
               (dotimes (i (length input-buffer))
                 (cept:write-cept #\backspace #\space #\backspace))
               (setf (fill-pointer input-buffer) 0))
             nil)))))))

(defun confirm-payment (preis)
  (page:display-system-line (format nil "Anzeigen für DM ~D,~2,'0D? Ja: #" (floor preis 100) (mod preis 100)))
  (equal (read-byte cept:*cept-stream*) #x1c))

(defun process-current-page (session)
  (let ((page (current-page session)))
    (format t "process-page - ~A~%" page)
    (sleep 1)
    (unless (or (zerop (page:preis page))
                (confirm-payment (page:preis page)))
      (format t "; free page, display~%"))))

(defun goto-page (page-number)
  (format t "; goto-page ~A~%" page-number)
  (if-let (page (gethash page-number (pages *session*)))
    (page:display page *session*)
    (page:display-system-line "Seite nicht vorhanden")))

(defun handle-client (session)
  (let ((*session* session))
    (ee:on :goto-page session 'goto-page)
    (loop
      (when-let (nummer (handle-input-byte (current-page *session*) session (read-byte cept:*cept-stream*)))
        (ee:emit :goto-page session nummer)))))
