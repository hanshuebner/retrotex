;; -*- Lisp -*-

(cl-interpol:enable-interpol-syntax)

(defpackage :retrotex
  (:use :cl :alexandria)
  (:export
   #:slideshow
   #:editor
   #:start-serial-server
   #:start-tcp-server
   #:show-article-file
   #:stop-tcp-server))

(in-package :retrotex)

;; Port must be opened with crtscts enabled ("stty -aF /dev/ttyUSB0 crtscts" before opening)

(defvar *default-port* "/dev/ttyUSB0")

(defvar *current-filename* #p"pages/page.cept")

(defun read-cept-byte ()
  (let ((byte (read-byte cept:*cept-stream*)))
    (format t "; read ~2,'0X (~A)~%" byte (code-char byte))
    byte))

(defmacro with-cept-stream ((stream) &body body)
  `(let ((cept:*cept-stream* ,stream))
     ,@body))

(defmacro with-cept-port ((&optional (port *default-port*)) &body body)
  `(let ((cept:*cept-stream* (open-port ,port)))
     (unwind-protect
          (progn
            ,@body)
       (close cept:*cept-stream*))))

(defun open-port (&optional (port *default-port*) (baud-rate 9600))
  (uiop:run-program (format nil "stty < ~A crtscts" port)
                    :output *standard-output*
                    :error-output *standard-output*)
  (let ((stream (cserial-port:make-serial-stream (cserial-port:open-serial port :baud-rate baud-rate))))
    (with-cept-stream (stream)
      (set-pc-mode)
      (cept:hide-cursor)
      (cept:disable-system-line))
    stream))

(defun set-pc-mode ()
  (cept:write-cept 1 #x75 "pacdfx"))          ; Datenverteiler setzen

(defun send-current-page ()
  (cept:write-cept 1 #x4c))

(defun switch-to-page (page-no)
  (cept:write-cept 1 (ecase page-no
                  (0 #x6c)
                  (1 #x6d)
                  (2 #x6e)
                  (3 #x6f))))

(defvar *page-no* 0)

(defun copy-until-end-of-page (input-stream output-stream)
  "Reads from input-stream and writes to output-stream until the sequence 9B 32 3B 39 56 is encountered."
  (let ((end-sequence #(#x9B #x32 #x3B #x39 #x56))
        (buffer (make-array 5 :element-type 'unsigned-byte :initial-element 0)))
    (loop
       ;; Shift the buffer left and read the next byte
       for byte = (read-byte input-stream)
       do
          ;; Shift buffer to the left and add the new byte at the end
          (dotimes (i 4)
            (setf (aref buffer i) (aref buffer (1+ i))))
          (setf (aref buffer 4) byte)

          ;; Check if the buffer matches the end-sequence
          (when (equalp buffer end-sequence)
            (return)) ; Stop reading if the end sequence is detected

          ;; Write the oldest byte in the buffer to the output stream
          (write-byte (aref buffer 0) output-stream))

    ;; If the loop exited because of the end sequence, write the remaining valid bytes
    (dotimes (i 4)
      (write-byte (aref buffer i) output-stream))))

(defun save-page (&optional (filename (format nil "page-~A.cept" (incf *page-no*))))
  (send-current-page)
  (with-open-file (f filename
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (unwind-protect
         (copy-until-end-of-page cept:*cept-stream* f)
      (format t "~A written~%" f)
      (close f))))

(defun load-page (filename)
  (cond
    ((probe-file filename)
     (format t "~&; loading page ~A~%" filename)
     (cept:clear-page)
     (cept:write-cept (read-file-into-byte-vector filename)))
    (t
     (format t "~&; File ~S does not exist~%" filename)
     (cept:write-cept #\return (format nil "Page ~A does not exist" (pathname-name filename)))))
  (finish-output cept:*cept-stream*))

(defun clear-input-line ()
  (cept:write-cept #\return (make-string 40 :initial-element #\space)))

(defun get-input (prompt)
  (cept:write-cept #\return prompt ": ")
  (let ((buffer (make-array 40 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (let ((char (code-char (read-cept-byte)))
            (current-position (+ (length prompt) 2 (length buffer))))
        (cond
          ((equal char #\return)
           (return))
          ((member char '(#\backspace #\delete #\can))
           (when (plusp (length buffer))
             (vector-pop buffer)
             (if (< current-position 40)
                 (cept:write-cept #\backspace #\space #\backspace)
                 (cept:write-cept #\space))))
          ((and (graphic-char-p char)
                (< current-position 40))
           (vector-push char buffer)
           (cept:write-cept char))
          (t (format t "; ignored: ~S~%" char)))))
    (clear-input-line)
    (coerce buffer 'string)))

(defun set-filename ()
  (let ((new-filename (get-input (format nil "Page name [~A]" (pathname-name *current-filename*)))))
    (unless (emptyp new-filename)
      (setf *current-filename* (merge-pathnames new-filename *current-filename*)))
    (format t "; filename ~S~%" *current-filename*)
    (cond
      ((probe-file *current-filename*)
       (load-page *current-filename*))
      (t
       (cept:service-jump-return)
       (cept:clear-page)
       (cept:service-jump 23)))))

(defun local-command ()
  (cept:service-jump 23)
  (cept:write-cept "[L]oad [S]ave [F]ilename ")
  (unwind-protect
       (case (code-char (prog1
                            (read-cept-byte)
                          (clear-input-line)))
         (#\l (load-page *current-filename*))
         (#\s (save-page *current-filename*))
         (#\f (set-filename))
         #+(or)
         (#\q (throw 'exit nil)))
    (cept:service-jump-return)))

(defun handle-byte (stream byte)
  (format t "<~2,'0X ~C~%" byte (if (<= 32 byte 127) (code-char byte) #\?))
             (case byte
               (#x13 ;; DC3 => '*'
                ; (cept:write-cept 1 #x42 0 #x0 0 0) ; send stack line (?)
                )
               (#x1a (local-command))
               (t
                (write-byte byte stream))))

(defun do-editing-commands ()
  (catch 'exit
    (loop for byte = (read-cept-byte)
          do (handle-byte cept:*cept-stream* byte)
             (finish-output cept:*cept-stream*)))
  (cept:hide-cursor))

(defun slideshow (&key sleep (cept-files (directory "pages/*.cept")))
  (let* ((cept-files (sort cept-files 'string-lessp :key 'pathname-name)))
    (assert cept-files)
    (let ((i 0))
      (loop
        (cept:reset)
        (load-page (nth i cept-files))
        (cond
          (sleep
           (sleep sleep)
           (incf i))
          (t
           (case (code-char (read-cept-byte))
             ((#\backspace #\delete)
              (when (zerop i)
                (setf i (length cept-files)))
              (decf i))
             ((#\space #\return)
              (incf i))
             (#\q
              (return-from slideshow)))))
        (when (= i (length cept-files))
          (setf i 0))))))

(defun editor ()
  (set-pc-mode)
  (cept:show-cursor)
  (do-editing-commands))

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
               (loop until (eql (code-char (read-cept-byte)) #\space)))))

(defun show-article-file (&key (text-file "cc-exponate.md") (frame "pages/ccframe.cept") (sleep 15))
  (cept:clear-page)
  (load-page frame)
  (loop
    (dolist (article (read-text-to-articles text-file))
      (destructuring-bind (title text) article
        (show-article title text :sleep sleep)))))

(defun emulate-modem-dialer (stream)
  (loop with character-stream = (flex:make-flexi-stream stream :external-format (flex:make-external-format :latin-1 :eol-style :cr))
        for line = (read-line character-stream)
        do (format t "; got modem command ~A~%" line)
        while (not (ppcre:scan "(?i)^AT.*D[PD]?[0-9]" line))
        finally (format character-stream "CONNECT~%")
                (finish-output character-stream))
  (format t "; dial command detected, continuing"))

(defun handle-client (handler stream handler-arguments)
  ;; for now, assume we're only handling one client at a time
  (unwind-protect
       (progn
         (setf cept:*cept-stream* stream)
         (apply handler handler-arguments))
    (ignore-errors
     (close stream))
    (setf cept:*cept-stream* nil)))

(defvar *tcp-server* nil)

(defun stop-tcp-server ()
  (when (and *tcp-server*
             (bt:thread-alive-p *tcp-server*))
    (bt:interrupt-thread *tcp-server* (lambda () (throw 'exit nil)))
    (setf *tcp-server* nil)))

(defun start-tcp-server (handler &rest handler-arguments &key (port 20000) emulate-modem-dialer-p &allow-other-keys)
  (stop-tcp-server)
  (setf *tcp-server*
       (bt:make-thread
        (lambda ()
          (catch 'exit
            (let ((socket (usocket:socket-listen "0.0.0.0" port :reuse-address t)))
              (unwind-protect
                   (loop
                     (let ((client-socket (usocket:socket-accept socket :element-type '(unsigned-byte 8))))
                       (format t "Connection accepted~%")
                       (handler-case
                           (let ((stream (usocket:socket-stream client-socket)))
                             #+sbcl
                             (setf (sb-impl::fd-stream-buffering stream) :none)
                             (when emulate-modem-dialer-p
                               (emulate-modem-dialer stream))
                             (handle-client handler stream
                                            (remove-from-plist handler-arguments :port :emulate-modem-dialer-p)))
                         (error (e)
                           (format t "Error handling client: ~a~%" e)))
                       (usocket:socket-close client-socket)))
                (usocket:socket-close socket)))))
        :name (format nil "CEPT server on port ~A" port))))

(defun start-serial-server (handler &rest handler-arguments &key (port *default-port*) &allow-other-keys)
  (with-open-stream (stream (open-port port))
    (handle-client handler stream (remove-from-plist handler-arguments :port))))
