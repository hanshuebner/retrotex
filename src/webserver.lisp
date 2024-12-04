;; -*- Lisp -*-

(defpackage :webserver
  (:use :cl :alexandria)
  (:export #:start)
  (:local-nicknames
   (:lq :lparallel.queue)
   (:ee :event-emitter)))

(in-package :webserver)

(defclass binary-websocket-stream (trivial-gray-streams:fundamental-binary-input-stream
                                   trivial-gray-streams:fundamental-binary-output-stream
                                   hunchensocket:websocket-client)
  ((queue :initform (lq:make-queue) :accessor queue)))

(defmethod initialize-instance :after ((stream binary-websocket-stream) &key)
  (format t "; setting *cept-stream* to ~A~%" stream)
  (setf cept:*cept-stream* stream))

(defmethod hunchensocket:binary-message-received (resource (stream binary-websocket-stream) data)
  (loop for byte across data
        do (lq:push-queue byte (queue stream))))

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-websocket-stream))
  (with-slots (queue) stream
    (lq:pop-queue queue)))

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

(defmethod hunchentoot:process-request :around ((request hunchensocket::websocket-request))
  (let ((hunchentoot:*request* request))
    (call-next-method)))

(defmethod hunchensocket:client-connected ((resource cept-websocket-resource)
                                           (client binary-websocket-stream))
  (setf (hunchentoot:session-value 'cept-thread)
        (bt:make-thread (lambda ()
                          (catch 'stop-handling-client
                            (handler-bind
                                ((error (lambda (e)
                                          (format t "; error handling client: ~A~%~A~%"
                                                  e
                                                  (hunchentoot::get-backtrace))
                                          (throw 'stop-handling-client nil))))
                              (funcall (acceptor-websocket-handler *acceptor*) client))))
                        :name "Websocket client handler process"
                        :initial-bindings `((cept:*cept-stream* . ,client)
                                            ,@bt:*default-special-bindings*))))

(defmethod hunchensocket:client-disconnected ((resource cept-websocket-resource)
                                              (client binary-websocket-stream))
  (format t "; client disconnected~%")
  (lq:push-queue :eof (queue client)))

(setf hunchensocket:*websocket-dispatch-table* (list (constantly (make-instance 'cept-websocket-resource))))

(defvar *acceptor* nil)

(defclass acceptor (hunchensocket:websocket-acceptor hunchentoot:easy-acceptor)
  ((websocket-handler :initarg :websocket-handler :reader acceptor-websocket-handler)))

(defmacro with-html (() &body body)
  `(with-output-to-string (s)
     (xhtml-generator:with-xhtml (s)
       ,@body)))

(defun send-emulator-command (command &rest arguments)
  (let ((thread (hunchentoot:session-value 'cept-thread)))
    (cond
      (thread
       (bt:interrupt-thread thread (lambda () (apply 'ee:emit command page:*session* arguments)))
       "OK")
      (t
       (setf (hunchentoot:return-code*) 400)
       "No emulator attached to session"))))

(hunchentoot:define-easy-handler (goto-page :uri "/goto-page") (number)
  (send-emulator-command :goto-page number))

(hunchentoot:define-easy-handler (load-btl-file :uri "/load-btl-file") (btl-filename)
  (send-emulator-command :load-btl-file btl-filename))

(hunchentoot:define-easy-handler (home :uri "/emulator") ()
  (hunchentoot:start-session)
  (with-html ()
    (:html (:head
            (:title "CEPT Terminal Emulator")
            ((:meta :charset "UTF-8"))
            ((:meta :name "viewport" :content "width=device-width, initial-scale=1.0"))
            ((:link :rel "stylesheet" :href "terminal.css")))
           (:body
            (:h1 "Web-CEPT-Emulator")
            ((:div :class "container")
             ((:div :class "left btl-info")
              ((:table)
               (:tr (:th "Name") (:td "ccc.btl"))
               (:tr (:th "Datum") (:td "20.02.1987"))))
             ((:div :class "middle")
              ((:div :id "display-container")
               ((:canvas :id "emulator" :width "600" :height "300" :tabindex "1")))
              ((:div :id "keyboard-container")
               ((:object :id "keyboard-svg" data="rafi-editing-keyboard.svg" :type "image/svg+xml"))))
             ((:div :class "right") " "))
            ((:script :src "bundle.js"))))))

(defun start (websocket-handler &key (port 8881))
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor* (make-instance 'acceptor
                                  :port port
                                  :websocket-handler websocket-handler
                                  :document-root #p"web-c14/public/"))
  (hunchentoot:start *acceptor*))
