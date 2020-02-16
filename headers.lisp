(defpackage :headers
  (:use :cl :bibliotheca :date)
  (:export :header))
(in-package :headers)

(defparameter *http-status-codes*
  '((:ok . 200) (:not-found . 404) (:internal-server-error . 500)))

(defun internal-server-error-header (&optional (date (get-universal-time)))
  (form-header 'http/1.1 500 "Internal Server Error"
	       (cons 'connection "Close")
	       (cons 'date (datetime date))
	       (cons 'server 'TBD)))

(define-condition status-not-found (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Status not found in *http-status-codes*"))))

(defun check-status (status)
  (assocdr status *http-status-codes*))

; FIXME: (http-status-code 1000) => "Nil"
;        For any number not in *http-status-codes*, we get a weird result,
;        is tha ok?
(defun http-status-code-text (status)
  (if (or (eq status :ok) (eql status 200))
      ; status 200 uses different capitalization rules, so
      ; we handle that case.
      "OK"
      (~>> (typecase status
	     (integer (car (rassoc status *http-status-codes*)))
	     (symbol status))
	(string-capitalize)
	(substitute #\Space #\-))))

(defun http-status-code-number (status)
  (typecase status
    (integer (cdr (rassoc status *http-status-codes*)))
    (symbol (assocdr status *http-status-codes*))))

(defun form-header (proto status-num status-text &rest fields)
  (join-strings (cons (format nil "~A ~D ~A" proto status-num status-text)
			(mapcar #'header-field fields))
		  format nil "~C~C" #\Return #\New))

(defun header-field (cell)
  (format nil "~:(~A~): ~A" (car cell) (cdr cell)))

(defun header (status &key (content-type "text/html") (date (get-universal-time)))
  (let ((text (http-status-code-text status))
	(number (http-status-code-number status)))
    (if (null number)
	(progn
	  (warn (format nil "~A not found in *http-status-codes*" status) 'status-not-found)
	  (internal-server-error-header))
	(form-header 'http/1.1 number text
		     (cons 'connection "Close")
		     (cons 'content-encoding "identity")
		     (cons 'content-type content-type)
		     (cons 'date (datetime date))
		     (cons 'server "TBD")))))

