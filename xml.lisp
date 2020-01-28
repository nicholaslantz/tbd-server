(defpackage :xml
  (:use :cl :bibliotheca)
  (:export :as-xml))
(in-package :xml)

(defparameter *test*
  '(html
    (head
     (meta (@ (charset . UTF-8)))
     (title An Example Page)
     (author Nicholas Lantz))
    (body
     (h2 Hello world!))))

(define-condition document-type-unavailable (error)
  ((message :initarg :message :reader message)))

(defparameter *headers*
  '((:html5 . "<!DOCTYPE html>")))

(defun xml-open (sym &optional (attrs nil))
  "Convert symbol to an XML open tag."
  (if (null attrs)
      (format nil "<~A>" (dc-sym-name sym))
      (format nil "<~A ~A>"
	      (dc-sym-name sym)
	      (join-strings (mapcar #'as-xml-attribute attrs) " "))))

(defun xml-close (sym)
  (concat "</" (string-downcase (symbol-name sym)) ">"))

(defun dc-sym-name (sym)
  (string-downcase (symbol-name sym)))

(defun attributesp (form)
  "If form has attributes, return them otherwise nil."
  (when (and (consp form)
	     (consp (cadr form))
	     (eq (caadr form) '@))
    (cadr form)))

(defun attributes (form)
  (cdr (attributesp form)))

(defun as-xml-attribute (attr)
  (format nil "~A=\"~A\""
	  (dc-sym-name (car attr))
	  (dc-sym-name (cdr attr))))

(defun document (form &key (type (caar *headers*)))
  (unless (assoc type *headers*)
    (cerror (format nil "~A is not available, default is ~A, use that instead?"
		    type (caar *headers*))
	    'document-type-unavailable)
    (setq type (caar *headers*)))
  (concat (assocdr type *headers*) (as-xml form)))

(defun as-xml (form)
  (if (atom form)
      (princ-to-string form)
      (let* ((elt (car form))
	     (attrs (attributes form))
	     (children (if attrs
			   (cddr form)
			   (cdr form))))
	(apply #'concat (list (xml-open elt attrs)
			      (join-strings (mapcar #'as-xml children) " ")
			      (xml-close elt))))))

