;; -*- Lisp -*-

(pushnew *default-pathname-defaults* asdf:*central-registry* :test #'equalp)
(ql:quickload :retrotex)
(cl-interpol:enable-interpol-syntax)
