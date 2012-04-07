(asdf:defsystem #:lisp-logs
  :serial t
  :components ((:file "package")
               (:file "lisp-logs")
	       (:file "server"))
  :depends-on (:hunchentoot :drakma :cl-who
	       :cl-interpol :cl-ppcre
	       :ultralight-css))
