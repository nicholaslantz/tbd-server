(defpackage :server
  (:use :cl :bibliotheca :usocket :bordeaux-threads :cl-ppcre :lparallel :xml)
  (:shadowing-import-from :cl-ppcre :split))
(in-package :server)

(defconstant +nl+ (coerce #(#\Return #\Newline) 'string))
(defparameter *header*
  (format nil "HTTP/1.1 200 OK~AContent-Type: text/html~AConnection: close~A~A" +nl+ +nl+ +nl+ +nl+))
(defparameter *html*
  `(html
    (head
     (meta (:@ (charset . UTF-8)))
     (title "An Example Page")
     (meta (:@ (name . "author") (content . "Nicholas Lantz")))
     (meta (:@ (name . "viewport") (content . "width=device-width, initial-scale=1.0"))))
    (body
     (h3 "Hello world")
     (hr (:@ (class . hello)))
     (p "Black Holes are really cool!")
     (p (a (:@ (href . "https://duckduckgo.com?q=black+holes")) "Check it out!")))))

;; Return fn that when called will block until new connection is received.
(defparameter *listener* (socket-listen #(127 0 0 1) 8200))

;; When main-handle is called, it blocks until a request is received on
;; *listener*.  It should then submit-task to handle it.
(defun main-handle ()
  (handler-case
    (let ((connection (socket-accept *listener*)))
      (make-thread (lambda () (handle connection)))
      (main-handle))
    (usocket:bad-file-descriptor-error () :socket-listen-not-called)))

(defparameter *listener-thread* (make-thread #'main-handle :name "listener thread"))

;; When connection is received, read all lines, parse, and return welcome page
(eval-when (:compile-toplevel)
  (defun handle (connection)
    (let ((lines
	   (mapcar (lambda (s)
		     (coerce (strip (coerce s 'list)) 'string))
		   (read-lines-until
		    (socket-stream connection)
		    (lambda (s)
		      (string-equal (format nil "~C" #\Return) s))))))
      (format #.*standard-output* "~S~%" (parse-request lines))
      (format (socket-stream connection)
	      "~A~A" *header* (document *html*))
      (force-output (socket-stream connection))
      (socket-close connection))))

(defun parse-request (req)
  (cons (parse-head (car req))
	(parse-fields (cdr req))))

(defun parse-head (head)
  (destructuring-bind (method resource proto)
      (mapcar (lambda (l) (coerce l 'string))
	      (split "\\s+" head))
    (list (intern (string-upcase method)) resource (intern proto))))

(defun parse-fields (fields &optional (acc nil))
  (if (null fields)
      acc
      (destructuring-bind (up-field up-value)
	  (split ": " (car fields))
	(let* ((field (intern (string-upcase up-field)))
	       (value up-value))
	  (parse-fields (cdr fields) (acons field value acc))))))

(defun parse-authority (author)
  (register-groups-bind (user domain (#'read-from-string port))
      ("^(?:(\\w+)@)?(\\w+)(?::(\\d+))?$" author)
    `((domain . ,domain) (user . ,user) (port . ,port))))

(defun split-comma-separated-list (lst)
  (split ",\\s*" lst))

;; TODO: Add parse-table as we add more features and need to respond to
;; browser requests better.
(let ((parse-table '()))
  ;;      `((host . parse-authority)
  ;; 	 (connection . list->string)
  ;; 	 (user-agent . list->string)
  ;; 	 (accept . split-comma-separated-list)
  ;; 	 (accept-language . split-comma-separated-list)
  ;; 	 (dnt . ,(lambda (lst) (eq (car lst) #\1)))
  ;; 	 (referer . parse-authority)
  ;; 	 (accept-encoding . ,(lambda (lst)
  ;; 			       (mapcar #'list->string (split lst '(#\, #\Space))))))))
  (defun parse-field (field value)
    (funcall (assocdr-default field parse-table #'identity) value)))

(defun list->string (lst)
  (coerce lst 'string))

(defun strip-left (line &optional (what '(#\Return #\Newline #\Space)))
  (if (member (car line) what)
      (strip-left (cdr line) what)
      line))

(defun strip-right (line &optional (what '(#\Return #\Newline #\Space)))
  (nreverse (strip-left (reverse line) what)))

(defun strip (line &optional (what '(#\Return #\Newline #\Space)))
  (strip-right (strip-left line what) what))

(defun read-lines-until (s test &optional (acc nil))
  (let ((line (read-line s)))
    (if (funcall test line)
	(reverse acc)
	(read-lines-until s test (cons line acc)))))

(defun response (resp)
  (join-strings (cons (response-header (car resp))
		      (response-fields (cdr resp)))
		+nl+))

(defun response-header (resp-head)
  (join-strings (mapcar #'princ-to-string resp-head)
		" "))

(defun response-fields (resp-fields &optional acc)
  (if (null resp-fields)
      acc
      (let ((field (caar resp-fields))
	    (val (cdar resp-fields)))
	(response-fields (cdr resp-fields)
			 (cons (join-strings (list (string-capitalize (symbol-name field)) val)
					     ": ")
			       acc)))))
