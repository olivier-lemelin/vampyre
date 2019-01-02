(defsystem :vampyre
  :depends-on (:hunchentoot :cl-who :parenscript :hunchensocket :verbose :jsown :cl-ascii-table)
  :serial t
  :components ((:file "package")
	       (:file "delivery-server")
	       (:file "vampyre")))
