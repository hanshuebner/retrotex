;; -*- Lisp -*-

(defpackage :webserver
  (:use :cl :alexandria))

(in-package :webserver)

(defclass binary-websocket-stream (trivial-gray-streams:fundamental-binary-input-stream
                                   trivial-gray-streams:fundamental-binary-output-stream
                                   hunchensocket:websocket-client)
  ((queue :initform (queues:make-queue :simple-cqueue) :accessor queue)))

(defmethod initialize-instance :after ((stream binary-websocket-stream) &key)
  (format t "; setting *cept-stream* to ~A~%" stream)
  (setf cept:*cept-stream* stream))

(defmethod hunchensocket:binary-message-received (resource (stream binary-websocket-stream) data)
  (loop for byte across data
        do (queues:qpush (queue stream) byte)))

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-websocket-stream))
  (with-slots (queue) stream
    (if (zerop (queues:qsize queue))
        :eof
        (queues:qpop queue))))

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

(setf hunchensocket:*websocket-dispatch-table* (list (constantly (make-instance 'cept-websocket-resource))))

(defvar *acceptor* nil)

(defun start (&key (port 8881))
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (push '("application/x-javascript" "js" "mjs") hunchentoot::*mime-type-list*)
  (setf *acceptor* (make-instance 'hunchensocket:websocket-acceptor
                                  :port port
                                  :document-root #p"web-c14/"))
  (hunchentoot:start *acceptor*))
