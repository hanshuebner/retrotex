(defsystem "retrotex"
  :description "retrotex: A simple editing and signage system for the RAFI C14 Bildschirmtext terminal"
  :author "Hans Hübner <hans.huebner@example.com>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on ("alexandria"
               "bordeaux-threads"
               "hunchensocket"
               "hunchentoot"
               "cserial-port"
               "cl-gd"
               "cl-interpol"
               "cl-ppcre"
               "flexi-streams"
               "usocket")
  :components ((:file "cept")
               (:file "retrotex")
               (:file "drcs")))
