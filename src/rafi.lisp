;; -*- Lisp -*-

(defpackage :rafi
  (:use :cl :alexandria)
  (:export
   #:start-serial-server))

(in-package :rafi)

;; Port must be opened with crtscts enabled ("stty -aF /dev/ttyUSB0 crtscts" before opening)

(defvar *default-port* "/dev/ttyUSB0")

(defvar *current-filename* #p"pages/page.cept")

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
    (cept:with-cept-stream (stream)
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
      (let ((char (code-char (read-byte cept:*cept-stream*)))
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
       (cept:service-break-return)
       (cept:clear-page)
       (cept:service-break 23)))))

(defun local-command ()
  (cept:service-break 23)
  (cept:write-cept "[L]oad [S]ave [F]ilename ")
  (unwind-protect
       (case (code-char (prog1
                            (read-byte cept:*cept-stream*)
                          (clear-input-line)))
         (#\l (load-page *current-filename*))
         (#\s (save-page *current-filename*))
         (#\f (set-filename))
         #+(or)
         (#\q (throw 'exit nil)))
    (cept:service-break-return)))

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
    (loop for byte = (read-byte cept:*cept-stream*)
          do (handle-byte cept:*cept-stream* byte)
             (finish-output cept:*cept-stream*)))
  (cept:hide-cursor))

(defun editor ()
  (set-pc-mode)
  (cept:show-cursor)
  (do-editing-commands))

(defun start-serial-server (handler &key (port *default-port*))
  (unless (probe-file port)
    (warn "serial port ~A not found, not opened" port)
    (return-from start-serial-server nil))
  (with-open-stream (stream (open-port port))
    (funcall handler stream)))

