;; -*- Lisp -*-

(defpackage :rafi
  (:use :cl :alexandria :cserial-port))

(in-package :rafi)

;; Port must be opened with crtscts enabled ("stty -aF /dev/ttyUSB0 crtscts" before opening)

(defvar *default-port* "/dev/ttyUSB0")

(defvar *rafi-stream* nil)

(defun open-port (&optional (port *default-port*))
  (uiop:run-program (format nil "stty < ~A crtscts" port)
                    :output *standard-output*
                    :error-output *standard-output*)
  (setf *rafi-stream*
        (cserial-port:make-serial-stream (cserial-port:open-serial port :baud-rate 9600))))

(defun to-octets (things)
  (flex:with-output-to-sequence (s)
    (dolist (thing things)
      (etypecase thing
        (string (write-sequence (flex:string-to-octets thing) s))
        ((array (unsigned-byte 8)) (write-sequence thing s))
        (character (write-byte (char-code thing) s))
        (number (write-byte thing s))))))

(defun write-rafi (&rest stuff)
  (let ((octets (to-octets stuff)))
    (write-sequence (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets)
                    *rafi-stream*)))

(defun reset ()
  (write-rafi 1 #x64))

(defun set-pc-mode ()
  (write-rafi 1 #x75 "pacdfx"))          ; Datenverteiler setzen

(defun test ()
  (write-rafi 1 #x78))

(defun disable-system-line (&optional permanentp)
  (write-rafi 1 (if permanentp #x6b #x6a)))

(defun clear-page ()
  (write-rafi #x0c))

(defun send-current-page ()
  (write-rafi 1 #x4c))

(defun switch-to-page (page-no)
  (write-rafi 1 (ecase page-no
                  (0 #x6c)
                  (1 #x6d)
                  (2 #x6e)
                  (3 #x6f))))

(defun constant-input ()
  (write-rafi 1 #x49))

(defun service-jump (&optional (line 23))
  (write-rafi #x1f #x2f #x40 (+ #x41 line)))

(defun jump-return ()
  (write-rafi #x1F #x2F #x4F))

(defun reset-page (&optional (arg #x42))
  (write-rafi #x1f #x2f arg))

(defun show-cursor ()
  (write-rafi #x11))

(defun hide-cursor ()
  (write-rafi #x14))

(defun setup ()
  (set-pc-mode))

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
         (copy-until-end-of-page *rafi-stream* f)
      (format t "~A written~%" f)
      (close f))))

(defun load-page (filename)
  (format t "~&; loading page ~A~%" filename)
  (clear-page)
  (write-rafi (read-file-into-byte-vector filename))
  (disable-system-line))

(defmacro with-rafi-stream ((stream) &body body)
  `(let ((*rafi-stream* ,stream))
     ,@body))

(defmacro with-rafi-port ((&optional (port *default-port*)) &body body)
  `(let ((*rafi-stream* (open-port ,port)))
     (unwind-protect
          (progn
            (setup)
            ,@body)
       (close *rafi-stream*))))

(defun local-command ()
  (case (code-char (read-byte *rafi-stream*))
    (#\l (load-page "page.cept"))
    (#\s (save-page "page.cept"))))

(defun handle-byte (stream byte)
  (format t "<~2,'0X~%" byte)
             (case byte
               (#x13 (throw 'exit nil)) ;; DC3 => '*'
               (#x1a (local-command))
               (t
                (write-byte byte stream)
                (finish-output stream))))

(defun do-editing-commands ()
  (catch 'exit
    (loop for byte = (read-byte *rafi-stream*)
          do (handle-byte *rafi-stream* byte))))

(defun slideshow (&key (dir "pages") (sleep 10) (port *default-port*))
  (let* ((cept-files (directory (merge-pathnames (format nil "~A/*.cept" dir))))
         (cept-files (sort cept-files 'string-lessp :key 'pathname-name)))
    (assert cept-files)
    (with-rafi-port (port)
      (setup)
      (hide-cursor)
      (disable-system-line)
      (loop
        (dolist (file cept-files)
          (load-page file)
          (sleep sleep))))))

(defun editor (&optional (port *default-port*))
  (with-rafi-port (port)
    (setup)
    (show-cursor)
    (do-editing-commands)))

