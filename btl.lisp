;; -*- Lisp -*-

(defpackage :btl
  (:use :cl :alexandria))

(in-package :btl)

(defun decode-blatt-kennzeichen (n)
  (code-char (+ #.(char-code #\a) -1 n)))

(defgeneric decode-field (buf type length offset bit-number))

(defmethod decode-field (buf (type (eql 'bd:bin)) length offset bit-number)
  (if (< length 1)
      (ldb (byte (floor (* 8 length)) bit-number) (aref buf offset))
      (let ((result 0))
        (dotimes (i length result)
          (setf result (logior result (ash (aref buf (+ offset i)) (* i 8))))))))

(defmethod decode-field (buf (type (eql 'bd:bit)) length offset bit-number)
  (assert (= length 1/8) () "bit field must have length 1/8")
  (plusp (ldb (byte 1 (- 7 bit-number)) (aref buf offset))))

(defmethod decode-field (buf (type (eql 'bd:bcd)) length offset bit-number)
  (parse-integer (with-output-to-string (*standard-output*)
                   (dotimes (i length)
                     (let* ((byte (aref buf (+ offset i)))
                            (nibble-1 (ldb (byte 4 4) byte))
                            (nibble-2 (ldb (byte 4 0) byte)))
                       (write-char (code-char (+ #.(char-code #\0) nibble-1)))
                       (write-char (code-char (+ #.(char-code #\0) nibble-2))))))))

(defmethod decode-field (buf (type (eql 'bd:bcd+)) length offset bit-number)
  (let ((result (with-output-to-string (*standard-output*)
                  (dotimes (i (min length 8))
                    (let* ((byte (aref buf (+ offset i)))
                           (nibble-1 (ldb (byte 4 4) byte))
                           (nibble-2 (ldb (byte 4 0) byte)))
                      (when (zerop nibble-1)
                        (return))
                      (write-char (code-char (+ #.(char-code #\0) (1- nibble-1))))
                      (when (zerop nibble-2)
                        (return))
                      (write-char (code-char (+ #.(char-code #\0) (1- nibble-2)))))))))
    (unless (zerop (length result))
      (if (> length 8)
          (format nil "~A~C" result (decode-blatt-kennzeichen (aref buf (+ offset 8))))
          result))))

(defmethod decode-field (buf (type (eql 'bd:cept)) length offset bit-number)
  (flex:octets-to-string buf :start offset :end (+ offset length)))

(defmethod decode-field (buf (type (eql 'bd:bits)) length offset bit-number)
  ;; BITS wird nur verwendet, um Vorder- und Hintergrundfarben zu definieren (SKOFAZE1 und SKOFAZE4)
  (let ((byte (aref buf offset)))
    (list (ldb (byte 4 4) byte) (ldb (byte 4 0) byte))))

(defun decode-bcd+ (buffer &optional (offset 0) (length 8))
  (decode-field buffer
                'bd:bcd+ length offset 0))

(defmacro define-btl-class ()
  (let ((field-names (mapcar (lambda (field)
                               (intern (string (bd:field-name field))))
                             bd:*btl-definitions*)))
    `(progn
       (defclass btl ()
         ((buffer :initarg :buffer :reader buffer)
          ,@(mapcar (lambda (name)
                      (list name :reader name))
                    field-names)))
       (defmethod initialize-instance ((btl btl) &key buffer)
         (call-next-method)
         (setf ,@(mapcan (lambda (name field)
                           `((slot-value btl ',name) (decode-field buffer
                                                                    ',(bd:field-type field)
                                                                    ,(bd:field-length field)
                                                                    ,(bd:field-offset field)
                                                                    ,(bd:field-bit-number field))))
                         field-names bd:*btl-definitions*))))))

(eval-when (:compile-toplevel)
  (defun last-char (x)
    (let ((x (string x)))
      (aref x (1- (length x))))))

(defmacro define-range-reader (name)
  (let* ((long-name-p (> (length (string name)) 5))
         (ptr (intern (format nil "~APT~C" (subseq (string name) 0 5) (if long-name-p (last-char name) #\R))))
         (len (intern (format nil "~ALE~C" (subseq (string name) 0 5) (if long-name-p (last-char name) #\N)))))
    `(defmethod ,name ((btl btl))
       (unless (zerop (,ptr btl))
         (subseq (buffer btl) (,ptr btl) (+ (,ptr btl) (,len btl)))))))

(define-btl-class)

(define-range-reader SKOAM)
(define-range-reader SKODR)
(define-range-reader SKOAC)
(define-range-reader SKOAS)
(define-range-reader SKOFB)
(define-range-reader SKOPM)
(define-range-reader SKOBV)
(define-range-reader SKOTD)
(define-range-reader SKOHQ1)
(define-range-reader SKOHQ4)

(defmethod btl-page-type ((btl btl))
  (cond
    ((SKOISEIT btl) "information")
    ((SKODSEIT btl) "dialog")
    ((SKOMSEIN btl) "message")
    ((SKOGWSEI btl) "transfer")
    ((SKOFSSER btl) "format service")
    ((SKOBBSEI btl) "billboard")
    (t (format nil "unknown ~2,'0X" (aref (buffer btl) 34)))))

(defmethod print-object ((btl btl) stream)
  (print-unreadable-object (btl stream :type t :identity t)
    (format stream "~A (~A)" (SKOSNRBP btl) (btl-page-type btl))))

(defmethod btl-choice-mapping ((btl btl))
  (append (list "#" "0")
          (if (SKOISEIT btl)
              (if (SKOWM2ST btl)
                  (loop for i from 10 upto 99 collect (princ-to-string i))
                  (loop for i from 1 upto 9 collect (princ-to-string i)))
              (list "2" "19"))))

(defmethod btl-auswahlmöglichkeiten ((btl btl))
  (when (SKOAWMDA btl)
    (loop with buffer = (SKOAM btl)
          with mapping = (btl-choice-mapping btl)
          with SAMHWAMO = (aref buffer 0)
          with SAMAWAMO = (aref buffer 1)
          with slot-length = (if (SKOQSAM2 btl) 10 8)
          for i from 0 below SAMHWAMO
          for slot = (1- (aref buffer (+ i 2)))
          for base = (+ 2 SAMHWAMO (* slot slot-length))
          unless (minusp slot)
            collect (list (nth i mapping)
                          (if (SKOQSAM2 btl)
                              (unless (zerop (aref buffer (1+ base)))
                                ;; regionalbereich wird ignoriert
                                (decode-bcd+ buffer (1+ base) 9))
                              (unless (zerop (aref buffer base))
                                (format nil "~Aa" (decode-bcd+ buffer base))))))))

(defmethod btl-schlagworte ((btl btl))
  (when (SKOBV btl)
    (loop with count = (aref (SKOBV btl) 0)
          for offset from 1 below (1+ (* count 3)) by 3
          collect (decode-field (SKOBV btl) 'bd:bcd 3 offset 0))))

(defun print-if-defined (format value)
  (when value
    (format t format value)
    (terpri)))

(defun print-length (format value)
  (unless (zerop (length value)) 
    (format t format (length value))
    (terpri)))

(defun print-flag (format value)
  (when value
    (format t format)
    (terpri)))

(defun print-string (format value)
  (let ((string (string-trim '(#\space) (flex:octets-to-string value))))
    (unless (zerop (length string))
      (format t format string)
      (terpri))))

(defun print-btl (btl)
  (format t "~&~A~%" btl)
  (when (/= (SKOZEILA btl) (SKOZEILE btl))
    (format t "Fenster: ~A-~A~%" (SKOZEILA btl) (SKOZEILE btl)))
  (print-flag "SKOFSBGH Hintergrund halten" (SKOFSBGH btl))
  #+(or)
  (print-flag "SKOAWMDA Auswahlmöglichkeit" (SKOAWMDA btl))
  #+(or)
  (print-flag "SKOWM2ST 2-stellige Wahlmöglichkeit" (SKOWM2ST btl))
  #+(or)
  (print-flag "SKOQSAM2 Auswahlmöglichkeit mit BKZ und Bl." (SKOQSAM2 btl))
  #+(or)
  (print-flag "SKOSVANG Schlagwortbereich vorhanden" (SKOSVANG btl))
  (print-if-defined "SKOSNRMT Mutterseite ~A" (SKOSNRMT btl))
  (print-if-defined "SKOSDRQ1 Decoder-Definition 1: ~A" (SKOSDRQ1 btl))
  (print-if-defined "SKOSDRQ2 Decoder-Definition 2: ~A" (SKOSDRQ2 btl))
  (print-if-defined "SKOSDRQ3 Decoder-Definition 3: ~A" (SKOSDRQ3 btl))
  (print-string "SKOHQ1 Zeile 1: ~S" (SKOHQ1 btl))
  (print-string "SKOHQ4 Zeile 20/24: ~S" (SKOHQ4 btl))
  (print-length "SKODR Dekoder-Daten: ~A bytes" (SKODR btl))
  (print-length "SKOAC Aufbaucode: ~A bytes" (SKOAC btl))
  (print-if-defined "SKOBV Schlagworte: ~A" (mapcar 'btx-index:get-entry (btl-schlagworte btl)))
  (print-if-defined "Auswahlmöglichkeiten: ~S" (btl-auswahlmöglichkeiten btl)))

(defun print-btl-directory (pathname &optional page)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with buffer = (make-array #x800)
          for i from #x4000 below (file-length f) by #x0800
          do (file-position f i)
             (read-sequence buffer f)
             (let ((btl (make-instance 'btl :buffer buffer)))
               (when (or (not page)
                         (equal page (SKOSNRBP btl)))
                 (print-btl btl)))
          finally (format t "~A Seiten~%" (/ i #x0800)))))

