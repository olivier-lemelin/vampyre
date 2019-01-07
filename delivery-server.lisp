(in-package :vampyre.delivery-server)

;;; Virtual Host
;;; ============

;;; Subclass ACCEPTOR

;; Allows for each virtual host to define its own dispatch table.
;;  Thus, we are able to run multiple servers in a single CL instance.
(defclass virtual-host (hunchentoot:acceptor)
  ;; slots
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  ;; options
  (:default-initargs                    ; default-initargs must be used
   :address "127.0.0.1"))               ; because ACCEPTOR uses it

;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
(defmethod hunchentoot:acceptor-dispatch-request ((vhost virtual-host) request)
  "Dispatches to the virtual host's internal dispatch table rather than the global."
  ;; try REQUEST on each dispatcher in turn
  (mapc (lambda (dispatcher)
    (let ((handler (funcall dispatcher request)))
      (when handler               ; Handler found. FUNCALL it and return result
        (return-from hunchentoot:acceptor-dispatch-request (funcall handler)))))
  (dispatch-table vhost))
  (call-next-method))

(defmethod port ((server virtual-host))
  "Returns the port on which the server is listening."
  (acceptor-port server))


;;; Delivery Server
;;; ===============

;; TODO: remove direct format usage, use logger instead.

(defvar *default-delivery-server-port* 80)
(defvar *delivery-servers* '())


;;; This is our main delivery server class.
(defclass delivery-server (virtual-host)
  ((deliverer
    :initarg :deliverer
    :initform (error "Please name this delivery server!")
    :reader deliverer
    :documentation "Name of this delivery server.")
  (url-table
    :initform (make-hash-table :test 'equal)
    :accessor url-table
    :documentation "User-accessible URL table that is used while rebuilding the server's dispatch table.")))


(defmethod (setf url-table) (new-val (obj delivery-server))
  "Ensures that the dispatch-table is rebuilt when we modify the url table."
  (setf (slot-value obj 'url-table) new-val)
  (rebuild-dispatch-table obj))


;; URL Handling Functions

(defmethod add-url-dispatcher ((server delivery-server) url handler-func)
  "Defines a URL to be handled by the given delivery server, calling the given function to do so."
  (push (hunchentoot:create-prefix-dispatcher url handler-func)
	(dispatch-table server)))


(defmethod rebuild-dispatch-table ((server delivery-server))
  "Internal function to rebuild the dispatch table from the url-table."
  (format t "Rebuilding the dispatch table...~%")
  (maphash #'(lambda (url func)
	       (add-url-dispatcher server url func))
	   (url-table server)))


(defmethod handle-url ((server delivery-server) url handler-func)
  "Defines a URL to call a given function."
  (format t "Handling '~a' with ~a...~%" url handler-func)
  (setf (gethash url
		 (url-table server))
	handler-func)
  (rebuild-dispatch-table server))


(defun make-delivery-server (port name)
  "Creates a new delivery server, and adds it to the global list."
  ;; Ensures that no server with the same name or port already exist.
  (if (not (or (get-delivery-server-by-name name)
	       (get-delivery-server-by-port port)))
      (push (make-instance 'delivery-server
			   :port port
			   :deliverer name
			   :document-root "should/never/exist"
			   :error-template-directory "should/never/exist")
	    *delivery-servers*)
      (format t "Port is occupied, or name already exists!")))  


;; Start and Stop functions

(defmethod listen ((server delivery-server))
  "Starts the delivery server, stopping it first if it was already started."
  (when (hunchentoot:started-p server)
    (kill server))
  (format t "Starting Delivery server on port ~a...~%" (port server))
  (hunchentoot:start server))

(defmethod kill ((server delivery-server))
  "Stops the delivery server, or does nothing if it had not been started."
  (when (hunchentoot:started-p server)
    (hunchentoot:stop server :soft t)
    (format t "Stopped Delivery server.~%")))

(defmethod kill-and-clear ((server delivery-server))
  "Stops the delivery server if necessary, and clear it from the list."
  (kill server)
  (format t "Deleting deliverer ~a...~%" (deliverer server))
  (setf *delivery-servers* (delete server *delivery-servers*)))


;; Details Functions

(defmethod display-details ((server delivery-server))
  "Displays the details of each delivery server."
  (loop for pair in (get-details server)
     do (progn
	  (format t "~a: ~a~%" (car pair) (cdr pair)))))


;; TODO: make one per class that will be list-manipulated.
(defmethod get-details ((server delivery-server))
  "Gets the details of a delivery server."
  `((name . ,(deliverer server))
    (port . ,(port server))
    (active . ,(format nil "~:[no~;yes~]" (started-p server)))))


;; Display Functions

;; Make a macro for other lists?
(defun do-delivery-list (list-fun)
  (loop for e in *delivery-servers*
     do (progn (funcall list-fun e))))

(defun display-delivery-servers ()
  "Displays details about all delivery servers."
  (do-delivery-list #'(lambda (x) (display-pairs (get-details x)))))

(defun table-delivery-servers ()
  "Displays a table of the delivery servers in a nice format."
  (let ((table (ascii-table:make-table '("Name" "Port" "Active"))))
    (do-delivery-list (lambda (x) (ascii-table:add-row table (get-values (get-details x)))))
    (ascii-table:display table)))

(defmethod display-url-table ((server delivery-server))
  "Displays the URL table of the server in a nice format."
  (let ((table (ascii-table:make-table '("URL" "Handler"))))
    (maphash #'(lambda (k v)
		 (ascii-table:add-row table (list k v)))
	     (url-table server))
    (ascii-table:display table)))


;; Example Test Page

(defun gzip-compress (str-data)
  (salza2:compress-data (sb-ext:string-to-octets str-data)
			'salza2:gzip-compressor))

(defun send-test-page ()
  "Default test page to send.  Does not mimick any particular page."
  (setf (hunchentoot:content-type*) "text/html")
  
  ;; Sets the headers to Apache defaults.
  (setf (header-out "accept-Ranges") "bytes")
  (setf (header-out "connection") "Keep-Alive")
  (setf (header-out "content-Encoding") "gzip")
  (setf (header-out "etag") "2c39-57ed229677f4c-gzip")
  (setf (header-out "keep-Alive") "timeout=5, max=100")
  (setf (header-out "last-Modified") "Sun, 06 Jan 2019 23:06:26 GMT")
  (setf (header-out "server") "Apache/2.4.18 (Ubuntu)")
  (setf (header-out "vary") "Accept-Encoding")
  (setf (header-out "accept-Ranges") "bytes")

  ;; GZip the resulting page.
  (gzip-compress 
   (cl-who:with-html-output (*standard-output* nil :prologue nil)
     (:html
      (:head
       (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
       (:title "Apache2 Default Page: It works"))
      (:body
       (:h1 "It works!")
       (:script :type "text/javascript" :src "/script.js")))))
  nil)

; By default, we use a 404 page similar to Apache's.
(defmethod acceptor-status-message ((server delivery-server) http-return-code &key &allow-other-keys)
  "Builds and returns the Apache 404 error page if it is received."

  (cond ((equal http-return-code 404)
	 (progn
	   ;; Sets the headers to Apache defaults.
	   (setf (header-out "connection") "Keep-Alive")
	   (setf (header-out "keep-Alive") "timeout=5, max=100")
	   (setf (header-out "server") "Apache/2.4.18 (Ubuntu)")
	    (cl-who:with-html-output-to-string (*standard-output* nil)
	      (:html
	       (:head
		(:title "404 Not Found"))
	       (:body
		(:h1 "Not Found")
		(:p (cl-who:fmt (format nil "The requested URL ~a was not found on this server." (script-name *request*))))
		(:hr)
		(:address (cl-who:fmt (format nil "Apache/2.4.18 (Ubuntu) Server at ~a Port ~a" (acceptor-address server) (acceptor-port server)))))))))))


;;; Lookup Functions

(defun get-delivery-server-by-func (parameter-func value)
  "Obtains a single delivery server by applying a function and expecting a given result."
  (find-if (lambda (elem) (equal (funcall parameter-func elem) value))
	   *delivery-servers*))


(defun get-delivery-server-by-name (name)
  "Gets a server from the list using its name."
  (get-delivery-server-by-func 'deliverer name))


(defun get-delivery-server-by-port (port)
  "Checks whether a port is occupied by another server.  Does not check for other programs that could be using the port."
  (get-delivery-server-by-func 'port port))







;; TODO: For the communication server

;; Combines both websocket server and regular web server in a single acceptor.
(defclass super-acceptor (virtual-host
			  hunchensocket:websocket-acceptor)
  ())
