;; -*- Lisp -*-

(defpackage :btl-page
  (:use :cl :alexandria)
  (:export #:load-btl-file
           #:load-btl
           #:print-btl-directory
           #:btl-session))

(in-package :btl-page)

(defconstant +sk-size+ 206)

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
  (unless (every #'zerop (subseq buf offset (+ offset length)))
    (flex:octets-to-string buf :start offset :end (+ offset length))))

(defmethod decode-field (buf (type (eql 'bd:bits)) length offset bit-number)
  ;; BITS wird nur verwendet, um Vorder- und Hintergrundfarben zu definieren (SKOFAZE1 und SKOFAZE4)
  (let ((byte (aref buf offset)))
    (list (ldb (byte 4 4) byte) (ldb (byte 4 0) byte))))

(defun decode-bcd+ (buffer &optional (offset 0) (length 8))
  (decode-field buffer 'bd:bcd+ length offset 0))

(defun decode-bcd (buffer &optional (offset 0) (length (- (length buffer) offset)))
  (decode-field buffer 'bd:bcd length offset 0))

(defmacro define-btl-class (name superclasses extra-slots (&key layout-file))
  (let* ((definitions (bd:parse-layout-file layout-file))
         (field-names (mapcar (lambda (field)
                                (intern (string (bd:field-name field))))
                              definitions)))
    `(progn
       (defclass ,name ,superclasses
         ((buffer :initarg :buffer :reader buffer)
          ,@(mapcar (lambda (name)
                      (list name :reader name))
                    field-names)
          ,@extra-slots))
       (defmethod initialize-instance ((,name ,name) &key buffer)
         (call-next-method)
         (setf ,@(mapcan (lambda (slot-name field)
                           `((slot-value ,name ',slot-name) (decode-field buffer
                                                                          ',(bd:field-type field)
                                                                          ,(bd:field-length field)
                                                                          ,(bd:field-offset field)
                                                                          ,(bd:field-bit-number field))))
                         field-names definitions))))))

(eval-when (:compile-toplevel)
  (defun last-char (x)
    (let ((x (string x)))
      (aref x (1- (length x))))))

(defmacro define-range-reader (name)
  (let* ((long-name-p (> (length (string name)) 5))
         (ptr (intern (format nil "~APT~C" (subseq (string name) 0 5) (if long-name-p (last-char name) #\R))))
         (len (intern (format nil "~ALE~C" (subseq (string name) 0 5) (if long-name-p (last-char name) #\N)))))
    `(defmethod ,name ((sk sk))
       (unless (zerop (,ptr sk))
         (subseq (buffer sk) (,ptr sk) (+ (,ptr sk) (,len sk)))))))

(define-btl-class sk (page:page)
  ((file :initarg :file :reader file)
   (db :initarg :db :reader db))
  (:layout-file "sk-layout.txt"))

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

(defmethod sk-page-type ((sk sk))
  (cond
    ((SKOISEIT sk) "information")
    ((SKODSEIT sk) "dialog")
    ((SKOMSEIN sk) "message")
    ((SKOGWSEI sk) "transfer")
    ((SKOFSSER sk) "format service")
    ((SKOBBSEI sk) "billboard")
    (t (format nil "unknown ~2,'0X" (aref (buffer sk) 34)))))

(defmethod print-object ((sk sk) stream)
  (print-unreadable-object (sk stream :type t :identity t)
    (format stream "~A ~A (~A)" (SKOSNRBP sk) (file sk) (sk-page-type sk))))

(defmethod sk-choice-mapping ((sk sk))
  (append (list "#" "0")
          (if (SKOISEIT sk)
              (if (SKOWM2ST sk)
                  (loop for i from 10 upto 99 collect (princ-to-string i))
                  (loop for i from 1 upto 9 collect (princ-to-string i)))
              (list "2" "19"))))

(defmethod sk-auswahlmöglichkeiten ((sk sk))
  (when (SKOAWMDA sk)
    (loop with buffer = (SKOAM sk)
          with mapping = (sk-choice-mapping sk)
          with SAMHWAMO = (aref buffer 0)
          with SAMAWAMO = (aref buffer 1)
          with slot-length = (if (SKOQSAM2 sk) 10 8)
          for i from 0 below SAMHWAMO
          for slot = (1- (aref buffer (+ i 2)))
          for base = (+ 2 SAMHWAMO (* slot slot-length))
          unless (minusp slot)
            collect (list (nth i mapping)
                          (if (SKOQSAM2 sk)
                              (unless (zerop (aref buffer (1+ base)))
                                ;; regionalbereich wird ignoriert
                                (decode-bcd+ buffer (1+ base) 9))
                              (unless (zerop (aref buffer base))
                                (format nil "~Aa" (decode-bcd+ buffer base))))))))

(defmethod sk-schlagworte ((sk sk))
  (when (SKOBV sk)
    (loop with count = (aref (SKOBV sk) 0)
          for offset from 1 below (1+ (* count 3)) by 3
          collect (decode-field (SKOBV sk) 'bd:bcd 3 offset 0))))

(define-btl-class sf ()
  ((BDHQSFE :initarg :BDHQSFE :reader BDHQSFE)
   (BDHQSTD :initarg :BDHQSTD :reader BDHQSTD)
   (BDHQSPM :initarg :BDHQSPM :reader BDHQSPM))
  (:layout-file "sf-layout.txt"))

(defmethod feld-attributes ((sf sf))
  `(,@(when (SFBATTR1 sf) '(:numeric))
    ,@(when (SFBATTR2 sf) '(:alphabetic))
    ,@(when (SFBATTR3 sf) '(:no-echo))
    ,@(when (SFBATTR4 sf) '(:no-cursor))
    ,@(when (SFBATTR5 sf) '(:protected))))

(defparameter *sysvar-mappings* '("&S001" :tln-name             "Name des Teilnehmers/Mitbenutzers"
                                  "&S002" :tln-name-zusatz      "Zusatz zum Namen des Teilnehmers /Mitbenutzers"
                                  "&S003" :tln-straße           "Straße des Teilnehmers/Mitbenutzers"
                                  "&S004" :tln-plz              "Postleitzahl des Teilnehmers /Mitbenutzers"
                                  "&S005" :tln-ort              "Ort des Teilnehmers /Mitbenutzers"
                                  "&S006" :tln-btx-nr           "Btxnummer (Mitbenutzernummer) des Teilnehmers bzw. Mitbenutzers"
                                  "&S007" :tln-tln-nr           "Teilnehmernummer des Teilnehmers/Mitbenutzers"
                                  "&S008" :tln-mb-suf           "Mitbenutzersuffix des Teilnehmers/Mitbenutzers"
                                  "&S009" :tln-anrede           "Anrede des Teilnehmers /Mitbenutzers"
                                  "&S021" :datum-abruf          "Datum des Seitenabrufs"
                                  "&S022" :uhrzeit-abruf        "Uhrzeit des Seitenabrufs"
                                  "&S023" :datum-uhrzeit-abruf  "Datum und Uhrzeit des Seitenabrufs"
                                  "&S010" :tfi                  "Terminal Facility Identifier"
                                  "&S031" :empf-name            "Name des Empfängers"
                                  "&S032" :empf-name-zusatz     "Zusatz zum Namen des Empfängers"
                                  "&S033" :empf-tln-nr          "Teilnehmernummer des Empfängers"
                                  "&S034" :empf-mb-suf          "Mitbenutzersuffix des Empfängers"
                                  "&S036" :enthält-werbung      "Mitteilung enthält Werbung"))

(defparameter *sysvar-names* (let ((map (make-hash-table :test #'equal)))
                               (loop for (code name) on *sysvar-mappings* by #'cdddr
                                     do (setf (gethash code map) name))
                               map))

(defmethod feld-definition-list ((sf sf))
  `(:data-len ,(SFBFDLTH sf)
    :row ,(SFBBROWS sf)
    :column ,(SFBBCOLM sf)
    :attributes ,(feld-attributes sf)
    :SFBFOFFS ,(SFBFOFFS sf)
    :SFPPOFFS ,(SFBPOFFS sf)
    :SFBLPRPT ,(SFBLPRPT sf)
    :SFBSYSVA ,(SFBSYSVA sf)
    :SFBBLEER ,(SFBBLEER sf)
    ,@(unless (zerop (SFBPOFFS sf))
        (list :prompting-message (subseq (BDHQSPM sf) (SFBPOFFS sf) (+ (SFBPOFFS sf) (SFBLPRPT sf)))))
    ,@(unless (zerop (SFBFOFFS sf))
        (let ((transparentp (and (not (SFBATTR1 sf)) (not (SFBATTR2 sf)) (= (SFBBLEER sf) 8))))
          (if transparentp
              (list :transparent-data (subseq (BDHQSTD sf) (SFBFOFFS sf) (+ (SFBFOFFS sf) (SFBFDLTH sf))))
              (list :default (subseq (BDHQSFE sf) (SFBFOFFS sf) (+ (SFBFOFFS sf) (SFBFDLTH sf)))))))))

(defmethod print-object ((sf sf) stream)
  (print-unreadable-object (sf stream :type t :identity t)
    (format stream "~A/~A (LEN ~D ~@[SYSVAR ~A ~]~{~A~^ ~})" (SFBBROWS sf) (SFBBCOLM sf) (SFBFDLTH sf) (gethash (SFBSYSVA sf) *sysvar-names* (SFBSYSVA sf)) (feld-attributes sf))))

(defmethod sk-feld-definitionen ((sk sk))
  (when (SKOFB sk)
    (loop for feld from 0 below (SKOFBANZ sk)
          for start = (+ (SKOFBOF1 sk) (* feld 20))
          collect (make-instance 'sf
                                 :buffer (subseq (SKOFB sk) start (+ start 20))
                                 :BDHQSFE (SKOFB sk)
                                 :BDHQSTD (SKOTD sk)
                                 :BDHQSPM (SKOPM sk)))))

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

(defun print-sk (sk)
  (format t "~&~A~%" sk)
  (when (/= (SKOZEILA sk) (SKOZEILE sk))
    (format t "Fenster: ~A-~A~%" (SKOZEILA sk) (SKOZEILE sk)))
  (print-flag "SKOFSBGH Hintergrund halten" (SKOFSBGH sk))
  #+(or)
  (print-flag "SKOAWMDA Auswahlmöglichkeit" (SKOAWMDA sk))
  #+(or)
  (print-flag "SKOWM2ST 2-stellige Wahlmöglichkeit" (SKOWM2ST sk))
  #+(or)
  (print-flag "SKOQSAM2 Auswahlmöglichkeit mit BKZ und Bl." (SKOQSAM2 sk))
  #+(or)
  (print-flag "SKOSVANG Schlagwortbereich vorhanden" (SKOSVANG sk))
  (print-if-defined "SKOSNRMT Mutterseite ~A" (SKOSNRMT sk))
  (print-if-defined "SKOSDRQ1 Decoder-Definition 1: ~A" (SKOSDRQ1 sk))
  (print-if-defined "SKOSDRQ2 Decoder-Definition 2: ~A" (SKOSDRQ2 sk))
  (print-if-defined "SKOSDRQ3 Decoder-Definition 3: ~A" (SKOSDRQ3 sk))
  (print-if-defined "SKOFB TV-Defaultdaten: ~A" (SKOFB sk))
  (print-if-defined "SKOPM Prompting Messages: ~A" (SKOPM sk))
  (print-string "SKOHQ1 Zeile 1: ~S" (SKOHQ1 sk))
  (print-string "SKOHQ4 Zeile 20/24: ~S" (SKOHQ4 sk))
  (print-length "SKODR Dekoder-Daten: ~A bytes" (SKODR sk))
  (print-length "SKOAC Aufbaucode: ~A bytes" (SKOAC sk))
  (print-if-defined "SKOBV Schlagworte: ~A" (mapcar 'btx-index:get-entry (sk-schlagworte sk)))
  (print-if-defined "Feld-Definitionen: ~A " (sk-feld-definitionen sk))
  (print-if-defined "Auswahlmöglichkeiten: ~S" (sk-auswahlmöglichkeiten sk)))

(defun nächstes-blatt (nummer)
  (ppcre:regex-replace ".$"
                       nummer
                       (lambda (match)
                         (string (code-char (1+ (char-code (aref match 0))))))
                       :simple-calls t))

(defmethod initialize-instance :after ((sk sk) &key)
  (setf (gethash (SKOSNRBP sk) (db sk)) sk
        (slot-value sk 'page:nummer) (SKOSNRBP sk)
        (slot-value sk 'page:choices) (loop with map = (make-hash-table :test #'equal)
                                            for (choice nummer) in (sk-auswahlmöglichkeiten sk)
                                            do (setf (gethash choice map) nummer)
                                            finally (return map)))
  (unless (gethash "#" (page:choices sk))
    (setf (gethash "#" (page:choices sk)) (nächstes-blatt (page:nummer sk)))))

(defun load-btl-file (pathname)
  (with-input-from-file (f pathname :element-type '(unsigned-byte 8))
    (loop with db = (make-hash-table :test #'equal)
          for i from #x4000 below (file-length f) by #x0800
          for buffer = (make-array #x800 :element-type '(unsigned-byte 8))
          do (file-position f i)
             (read-sequence buffer f)
          collect (make-instance 'sk :file (pathname-name pathname):buffer buffer :db db)
          finally (format t "~&; ~A Seiten aus ~A gelesen~%" (/ i #x0800) pathname))))

(defparameter *archive-directory* #P"archive/")

(defun btl-files-in-directory (pathname)
  (remove-if-not (curry #'string-equal "btl")
                 (directory (merge-pathnames #p"*.*" pathname)) :key #'pathname-type))

(defclass btl-directory-page (page:page)
  ((btl-files :reader btl-files)
   (base-directory :initarg :base-directory :reader base-directory))
  (:default-initargs :base-directory *archive-directory*))

(defmethod initialize-instance :after ((page btl-directory-page) &key)
  (with-slots (base-directory btl-files) page
    (setf base-directory (merge-pathnames base-directory)
          btl-files (btl-files-in-directory base-directory))))

(defun btl-file-date (btl-pathname)
  (let ((bcd-date (make-array 3 :element-type '(unsigned-byte 8))))
    (with-open-file (f btl-pathname :element-type '(unsigned-byte 8))
      (file-position f #x30)
      (read-sequence bcd-date f))
    (let ((year (decode-bcd bcd-date 0 1))
          (month (decode-bcd bcd-date 1 1))
          (date (decode-bcd bcd-date 2 1)))
      (format nil "~2D.~2,'0D.~4D" date month (+ 1900 year)))))

(defmethod page:nummer ((page btl-directory-page))
  "0a")

(defun format-date (universal-time)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~2D.~2,'0D.~4,'0D" date month year)))

(defmethod page:impressum ((page btl-directory-page))
  (flex:string-to-octets "Verzeichnis der BTL-Dateien"))

(defmethod page:display ((page btl-directory-page) session)
  (cept:clear-page)
  (cept:define-colors 0 '((0 0 0) (5 5 5) (7 7 7)))
  (cept:screen-color 0)
  (loop with files = (btl-files-in-directory (merge-pathnames #p"**/" (base-directory page)))
        for i below 10
        for btl-file-pathname = (nth i files)
        for row = (+ (* 2 i) 2)
        do (cept:goto row 0)
           (cept:background-color (1+ (mod i 2)))
           (cept:format-cept "~2D ~10A~27A"
                             (1+ i)
                             (btl-file-date btl-file-pathname)
                             "")
           (cept:goto (1+ row) 0)
           (cept:background-color (1+ (mod i 2)))
           (cept:format-cept "   ~37A" (enough-namestring btl-file-pathname (base-directory page)))))

(defun load-btl (pathname)
  (if (pathname-name pathname)
      (load-btl-file pathname)
      (let ((btl-pathnames (btl-files-in-directory pathname)))
        (mappend 'load-btl-file btl-pathnames))))

(defun print-btl-directory (pathname)
  (let ((pages (load-btl-file pathname)))
    (dolist (page pages)
      (print-sk page))
    (format t "~A Seiten~%" (length pages))))

(defparameter *btl-directory* (or (uiop:getenv "BTL_DIRECTORY") #p"BTL/"))

(defclass btl-session (page:session)
  ((loaded-decoder-pages :initform (make-list 3) :accessor loaded-decoder-pages)))

(defmethod page:impressum ((sk sk))
  (SKOHQ1 sk))

(defmethod page:preis ((sk sk))
  (SKOEBETR sk))

(defun load-decoder-page (sk nummer)
  (when nummer
    (if-let (decoder-page (gethash nummer (db sk)))
      (cept:write-cept (SKODR decoder-page))
      (warn "; decoder page ~S not found" nummer))
    (finish-output cept:*cept-stream*)
    (sleep 0.5)))

(defmethod load-decoder-pages ((sk sk) session)
  (with-slots (SKOSDRQ1 SKOSDRQ2 SKOSDRQ3) sk
    (with-slots (loaded-decoder-pages) session
      (unless (equal (first loaded-decoder-pages) SKOSDRQ1)
        (load-decoder-page sk SKOSDRQ1))
      (unless (member SKOSDRQ2 (rest loaded-decoder-pages) :test #'equal)
        (load-decoder-page sk SKOSDRQ2))
      (unless (member SKOSDRQ3 (rest loaded-decoder-pages) :test #'equal)
        (load-decoder-page sk SKOSDRQ3))
      (setf loaded-decoder-pages (list SKOSDRQ1 SKOSDRQ2 SKOSDRQ3)))))

(defmethod page:display ((sk sk) session)
  (change-class session 'btl-session)
  (format t "; showing page ~A (Hintergrund ~:[nicht ~;~]halten)~%" sk (SKOFSBGH sk))
  (when (SKOFSBGH sk)
    (cept:reset)
    (cept:clear-page))
  (load-decoder-pages sk session)
  (when (plusp (SKOZEILA sk))
    (loop for row from (SKOZEILA sk) upto (SKOZEILE sk)
          do (cept:goto row 0)
             (cept:delete-to-end-of-line))
    (cept:goto (SKOZEILA sk) 0))
  (cept:write-cept (SKOAC sk)))

(defun check-btl-cross-references (pathname)
  (let ((pages (load-btl-file pathname))
        (error-count 0))
    (dolist (page (sort pages #'string-lessp :key #'page:nummer))
      (let (has-error-p)
        (dolist (input (sort (hash-table-keys (page:choices page)) #'string-lessp))
          (let ((target-page (gethash input (page:choices page)))) 
            (unless (gethash target-page (db page))
              (unless has-error-p
                (format t "~A~%" page)
                (setf has-error-p t))
              (format t "  ~A => ~A~%" input target-page)
              (incf error-count))))))
    (format t "~D error~:P~%" error-count)))

(defparameter *default-btl-pathname* #P"ccc.btl")

(defmethod initialize-instance :after ((session btl-session) &key)
  (let ((pages (page:make-page-directory (btl-page:load-btl *default-btl-pathname*))))
    (setf (page:pages session) pages
          (page:current-page session) (gethash "655321a" pages))))
