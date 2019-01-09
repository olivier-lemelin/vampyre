(defpackage vampyre.utils
  (:use cl)
  (:export :display-pairs :get-values :random-name))

(defpackage vampyre.delivery-server
  (:use cl hunchentoot hunchensocket ascii-table vampyre.utils parenscript)
  (:shadow :listen))

(defpackage vampyre
  (:use cl hunchentoot cl-who parenscript hunchensocket jsown))

