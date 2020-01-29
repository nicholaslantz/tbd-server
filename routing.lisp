(defpackage :routing
  (:use :cl :cl-ppcre))
(in-package :routing)

;; A route should take a route, then a function that returns the content
;; the user requested.  Alternatively data itself.
;;
;; Routing should be hierarchical.  For instance, if I have a www directory,
;; I want to be able to request /www/users.html and it will return the
;; users.html file, I also want to do /www/users/0.html and it will consult
;; the users directory.  I also want somethng like /www/a/specific/path/ that
;; has a particular definition to take precedence over the more general one.
;;
;; So the basic idea is to form a tree of all the different routes to go down,
;; and match a particular request as best as possible.
(defparameter *routes* '((index . index-page)
			 (www . directory-server)))
