;; -*- Lisp -*-

(defpackage :webserver
  (:use :cl :alexandria :lparallel.queue)
  (:export #:start))

(in-package :webserver)

(defclass binary-websocket-stream (trivial-gray-streams:fundamental-binary-input-stream
                                   trivial-gray-streams:fundamental-binary-output-stream
                                   hunchensocket:websocket-client)
  ((queue :initform (make-queue) :accessor queue)))

(defmethod initialize-instance :after ((stream binary-websocket-stream) &key)
  (format t "; setting *cept-stream* to ~A~%" stream)
  (setf cept:*cept-stream* stream))

(defmethod hunchensocket:binary-message-received (resource (stream binary-websocket-stream) data)
  (loop for byte across data
        do (push-queue byte (queue stream))))

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-websocket-stream))
  (with-slots (queue) stream
    (pop-queue queue)))

(defmethod trivial-gray-streams:stream-write-byte ((stream binary-websocket-stream) byte)
  (hunchensocket:send-binary-message stream (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream binary-websocket-stream) sequence start end &key)
  (hunchensocket:send-binary-message stream (subseq sequence start end)))

(defclass cept-websocket-resource (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'binary-websocket-stream))

(defmethod hunchensocket:check-message ((resource cept-websocket-resource)
                                        (client binary-websocket-stream)
                                        (opcode (eql hunchensocket::+binary-frame+))
                                        length total))

(defmethod hunchensocket:client-connected ((resource cept-websocket-resource)
                                           (client binary-websocket-stream))
  (format t "; client connected~%")
  (bt:make-thread (lambda ()
                    (handler-case
                        (funcall (acceptor-handler *acceptor*) client)
                      (error (e)
                        (format t "; error handling client: ~A~%" e))))
                  :name "Websocket client handler process"
                  :initial-bindings (cons (cons 'cept:*cept-stream* client)
                                          bt:*default-special-bindings*)))

(defmethod hunchensocket:client-disconnected ((resource cept-websocket-resource)
                                              (client binary-websocket-stream))
  (format t "; client disconnected~%")
  (push-queue :eof (queue client)))

(setf hunchensocket:*websocket-dispatch-table* (list (constantly (make-instance 'cept-websocket-resource))))

(defvar *acceptor* nil)

(defclass acceptor (hunchensocket:websocket-acceptor hunchentoot:easy-acceptor)
  ((handler :initarg :handler :reader acceptor-handler)))

(defun no-cache-callback (file content-type)
  (declare (ignore file content-type))
  (hunchentoot:no-cache))

(setf hunchentoot:*dispatch-table*
      (list (hunchentoot:create-folder-dispatcher-and-handler "/"
                                                              #p"web-c14/public/"
                                                              nil
                                                              'no-cache-callback)))

(defun start (handler &key (port 8881))
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor* (make-instance 'acceptor :port port :handler handler))
  (hunchentoot:start *acceptor*))
