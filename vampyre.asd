(defsystem :vampyre
  :depends-on (:cl-ppcre :hunchentoot :cl-who :parenscript :hunchensocket :verbose)
  :serial t
  :components ((:file "package")
	       (:file "vampyre")))
