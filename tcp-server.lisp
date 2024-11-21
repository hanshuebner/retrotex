;; -*- Lisp -*-

(defpackage :tcp-server
  (:use :cl :alexandria)
  (:export #:stop
           #:start))

(in-package :tcp-server)


(defmethod emulate-modem-dialer ((type (eql :hayes)) stream)
  (loop with character-stream = (flex:make-flexi-stream stream :external-format (flex:make-external-format :latin-1 :eol-style :cr))
        for line = (read-line character-stream)
        do (format t "; got modem command ~A~%" line)
        while (not (ppcre:scan "(?i)^AT.*D[PD]?[0-9]" line))
        finally (format character-stream "CONNECT~%")
                (finish-output character-stream))
  (format t "; dial command detected, continuing~%"))

(defmethod emulate-modem-dialer ((type (eql :teletool)) stream)
  (loop for c = (read-byte stream)
        do (format t "; Modem dialer received #x~2,'0X ~A~%" c (code-char c))
           ;(write-byte (char-code #\enq) stream)
           (sleep .2)
           (finish-output stream)
        until (= c 3))
  (write-byte (char-code #\ack) stream)
  (finish-output stream))

(defvar *tcp-server* nil)

(defun stop ()
  (when (and *tcp-server*
             (bt:thread-alive-p *tcp-server*))
    (bt:interrupt-thread *tcp-server* (lambda () (throw 'exit nil)))
    (setf *tcp-server* nil)))

(defun start (handler &key (port 20000) modem-type)
  (stop)
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
                             (when modem-type
                               (emulate-modem-dialer modem-type stream))
                             (unwind-protect
                                  (funcall handler stream)
                               (ignore-errors (close stream))))
                         (error (e)
                           (format t "Error handling client: ~a~%" e)))
                       (usocket:socket-close client-socket)
                       (format t "TCP client disconnected")))
                (usocket:socket-close socket)))))
        :name (format nil "CEPT server on port ~A" port))))

