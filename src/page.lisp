;; -*- Lisp -*-

(defpackage :page
  (:use :cl :alexandria)
  (:export #:choice
           #:page
           #:display
           #:load-pages
           #:page-db
           #:page-number
           #:page-choices))

(in-package :page)

(defclass page ()
  ((db :initarg :db
       :reader page-db
       :documentation "Datenbank, in der diese Seite abgelegt ist")
   (number :initarg :number
           :reader page-number
           :documentation "Seitennummer inklusive Blattkennzeichen")
   (choices :initarg :choices
            :reader page-choices
            :documentation "Hashtable mit Schlüssel Auswahlstring und Wert Seitennummer")))

(defgeneric display (page stream)
  (:documentation "Ausgeben der angegebenen Seite"))

(defgeneric load-pages (class-name db)
  (:documentation "Laden der Seiten mir der angegebenen Klasse in die Datenbank")
  (:method-combination progn))
