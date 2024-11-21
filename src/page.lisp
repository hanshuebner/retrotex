;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:page
           #:display
           #:load-pages
           #:db
           #:choices
           #:nummer))

(in-package :page)

(defclass page ()
  ((db :initarg :db
       :reader db
       :documentation "Datenbank, in der diese Seite abgelegt ist")
   (nummer :initarg :nummer
           :reader nummer
           :documentation "Seitennummer inklusive Blattkennzeichen")
   (choices :initarg :choices
            :reader choices
            :documentation "Hashtable Auswahlstring => Seitennummer")))

(defgeneric display (page stream)
  (:documentation "Ausgeben der angegebenen Seite"))
