(defsystem "rafi-edit"
  :description "rafi-edit: A simple editing and signage system for the RAFI C14 Bildschirmtext terminal"
  :author "Hans Hübner <hans.huebner@example.com>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on ("alexandria"
               "cserial-port"
               "flexi-streams"
               "cl-ppcre"
               "cl-interpol"
               "flexi-streams")
  :components ((:file "cept")
               (:file "rafi")))
