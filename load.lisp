;; -*- Lisp -*-

(pushnew *default-pathname-defaults* asdf:*central-registry* :test #'equalp)
(ql:quickload :rafi-edit)
