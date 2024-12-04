(defsystem "retrotex"
  :description "retrotex: A Bildschirmtext server and terminal emulation system"
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
               "event-emitter"
               "flexi-streams"
               "trivial-gray-streams"
               "lparallel"
               "usocket"
               "xhtmlgen"
               "yason")
  :components ((:module "src"
                :serial t
                :components ((:file "cept")
                             (:file "drcs")
                             (:file "page")
                             (:file "rafi")
                             (:file "webserver")
                             (:file "tcp-server")
                             (:file "json-page")
                             (:file "btx-index")
                             (:file "btl-definitions")
                             (:file "btl-page")
                             (:file "retrotex")))))
