(defpackage vampyre
  (:use cl hunchentoot cl-who parenscript hunchensocket)
  (:import-from :cl-ppcre :split)
  (:export :main))
