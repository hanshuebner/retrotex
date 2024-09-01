;; -*- Lisp -*-

(defpackage :rafi
  (:use :cl :alexandria :cserial-port))

(in-package :rafi)

;; Port must be opened with crtscts enabled ("stty -aF /dev/ttyUSB0 crtscts" before opening)

(defvar *default-port* "/dev/ttyUSB0")

(defvar *rafi-stream* nil)

(defun open-port (&optional (port *default-port*))
  (setf *rafi-stream*
        (cserial-port:make-serial-stream (cserial-port:open-serial port :baud-rate 9600))))

(defun to-octets (things)
  (loop for x in things
        if (stringp x)
          append (coerce (sb-ext:string-to-octets x) 'list)
        else
          if (characterp x)
               collect (char-code x)
        else
          collect x))

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

(defun disable-system-line ()
  (write-rafi 1 #x6b))

(defun get-current-page ()
  (write-rafi 1 #x4c))

(defun switch-to (page-no)
  (write-rafi 1 (ecase page-no
                  (0 #x6c)
                  (1 #x6d)
                  (2 #x6e)
                  (3 #x6f))))

(defun constant-input ()
  (write-rafi 1 #x49))

(defun reset-page (&optional (arg #x42))
  (write-rafi #x1f #x2f arg))

(defun show-cursor ()
  (write-rafi #x11))

(defun setup ()
  (set-pc-mode)
  (switch-to 1)
  (show-cursor))

(defmacro with-rafi-stream ((stream) &body body)
  `(let ((*rafi-stream* ,stream))
     ,@body))

(defun local-command ()
  (constant-input))

(defun handle-byte (stream byte)
  (format t "<~2,'0X~%" byte)
             (case byte
               (#x13 (throw 'exit nil)) ;; DC3 => '*'
               (#x1a (local-command))
               (t
                (write-byte byte stream)
                (finish-output stream))))

(defun do-editing-commands (stream)
  (with-rafi-stream (stream)
    (catch 'exit
      (loop for byte = (read-byte stream)
            do (handle-byte stream byte)))))

(defun editor (&optional (port *default-port*))
  (let ((stream (open-port port)))
    (with-rafi-stream (stream)
        (setup))
    (unwind-protect
         (do-editing-commands stream)
      (close stream))))

