;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:page
           #:display
           #:load-pages
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
           #:goto-page))

(in-package :page)

(defun make-page-directory (pages)
  (let ((directory (make-hash-table :test #'equal)))
    (dolist (page pages directory)
      (setf (gethash (page:nummer page) directory) page))))

(defconstant +max-page-stack+ 50
  "Anzahl der Seiten, die pro Session für *# gespeichert werden")

(defclass session ()
  ((cept-stream :initarg :cept-stream
                :reader cept-stream)
   (decoder-pages :initform nil
                  :reader decoder-pages)
   (stack :initform nil
          :accessor stack
          :documentation "Enthält die aktuelle und bis zu +max-page-stack+ vorher besuchte Seiten")
   (pages :initargs :pages
          :accessor pages)
   (current-page :accessor current-page)))

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

(defmethod handle-input ((page page) session)
  (format t "; handle input, defined choices ~A~%" (sort (hash-table-keys (choices page)) #'string-lessp))
  (cept:goto 23 0)
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
                      (or (session-pop-page session (nummer page))
                          (nummer page)))
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

(defun confirm-payment (preis)
  (page:display-system-line (format nil "Anzeigen für DM ~D,~2,'0D? Ja: #" (floor preis 100) (mod preis 100)))
  (equal (read-byte cept:*cept-stream*) #x1c))

(defun handle-client (session)
  (catch 'stream-closed
    (let (next-page-nummer)
      (loop
        (setf next-page-nummer
              (catch 'display-page
                (format t "display-page - next-page-nummer ~A~%" next-page-nummer)
                (page:display (current-page session) session)
                (let* ((next-page-nummer (or next-page-nummer (page:handle-input (current-page session) session)))
                       (next-page (gethash next-page-nummer (pages session))))
                  (cond
                    (next-page
                     (when (or (zerop (page:preis next-page))
                               (confirm-payment (page:preis next-page)))
                       (setf (current-page session) next-page)))
                    (t
                     (page:display-system-line "Seite nicht vorhanden")
                     (sleep 1))))))))))

(defun goto-page (page-nummer)
  (format t "; goto-page ~A~%" page-nummer)
  (throw 'display-page page-nummer))
