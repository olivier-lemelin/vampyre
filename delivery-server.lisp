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

;;; This is our main delivery server class.
(defclass delivery-server (virtual-host)
  ((deliverer
    :initarg :deliverer
    :initform (error "Please name this delivery server!")
    :reader deliverer
    :documentation "Name of this delivery server.")))

(defvar *default-delivery-server-port* 80)
(defvar *delivery-servers* '())

(defun make-delivery-server (port name)
  "Creates a new delivery server, and adds it to the global list."
  (if (not (or (check-name-exists name)
	       (check-port-occupied port)))
      (push (make-instance 'delivery-server :port port :deliverer name)
	    *delivery-servers*)
      (format t "Port is occupied, or name already exists!")))
  

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

(defmethod display-details ((server delivery-server))
  "Displays the details of each delivery server."
  (loop for pair in (get-details server)
     do (progn
	  (format t "~a: ~a~%" (car pair) (cdr pair)))))

;; make one per class that will be list-manipulated.
(defmethod get-details ((server delivery-server))
  "Gets the details of a delivery server."
  `((name . ,(deliverer server))
    (port . ,(port server))
    (active . ,(format nil "~:[no~;yes~]" (started-p server)))))


(defun send-test-page ()
  (setf (hunchentoot:content-type*) "text/html")
  (cl-who:with-html-output (*standard-output* nil :prologue nil)
    (:html
     (:body
      (:h1 "Hello World!")
      (:script :type "text/javascript" :src "/script.js")))))


(defmethod add-url-handler ((server delivery-server) url handler-func)
  (format t "Handling URL '~a' using function ~a." url handler-func)
  (push (hunchentoot:create-prefix-dispatcher url handler-func)
	(dispatch-table server)))



(defun define-stage-one-url (url agent-delivery-fun acceptor)
  "Adds a URL to the host that can be used to deliver the Stage 1 script."
  (format t "Adding delivery page: ~a...~%" url)
  (push (hunchentoot:create-prefix-dispatcher url agent-delivery-fun)
	(dispatch-table acceptor)))


(defun get-delivery-server-by-func (parameter-func value)
  (find-if (lambda (elem) (equal (funcall parameter-func elem) value))
	   *delivery-servers*))

;; Make more general for other lists.
(defun get-delivery-server-by-name (name)
  "Gets a server from the list using its name."
  (get-delivery-server-by-func 'deliverer name))

(defun check-port-occupied (port)
  "Checks whether a port is occupied by another server.  Does not check for other programs that could be using the port."
  (get-delivery-server-by-func 'port port))

(defun check-name-exists (name)
  "Checks whether a given name exists for an element in the list."
  (get-delivery-server-by-name name))

;; Send to utils!
(defun display-pairs (pairs)
  (loop for pair in pairs do
       (format t "~a: ~a~%" (car pair) (cdr pair))))

;; Make a macro for other lists?
(defun do-delivery-list (list-fun)
  (loop for e in *delivery-servers*
     do (progn (funcall list-fun e))))

(defun display-delivery-servers ()
  (do-delivery-list #'(lambda (x) (display-pairs (get-details x)))))

(defun display-row (details)
  (format t "~& ~12a ~5d ~6d"
	  (cdr (assoc 'name details))
	  (cdr (assoc 'port details))
	  (cdr (assoc 'active details))))



;; TODO: To utilities!
(defun get-values (alist)
  (loop for i in alist collect (cdr i)))

(defun table-delivery-servers ()
  (let ((table (ascii-table:make-table '("Name" "Port" "Active"))))
    (do-delivery-list (lambda (x) (ascii-table:add-row table (get-values (get-details x)))))
    (ascii-table:display table)))




(defun define-script-delivery-urls (url-list script-fun acceptor)
  (loop
     for url in url-list
     do (define-stage-one-url url script-fun acceptor)))





;; TODO: For the communication server

;; Combines both websocket server and regular web server in a single acceptor.
(defclass super-acceptor (virtual-host
			  hunchensocket:websocket-acceptor)
  ())
