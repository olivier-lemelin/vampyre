(defsystem :vampyre
  :depends-on (:cl-ppcre :hunchentoot :cl-who :parenscript :hunchensocket :verbose :jsown)
  :serial t
  :components ((:file "package")
	       (:file "vampyre")))
