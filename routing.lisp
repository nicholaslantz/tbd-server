(defpackage :routing
  (:use :cl :bibliotheca :cl-ppcre)
  (:shadowing-import-from :cl-ppcre :split))
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
;;
;; So I want relevant routing functions in here, but I want the actual routes
;; somewhere else, the main server file for now I suppose.
(defparameter *test* '(root
		       (hello . "Hello!")
		       (users
			(nick . "nick")
			(evan . "evan")
			(joe . "joe"))))

(defparameter *test-tree* '(a
			    (b c)
			    (d (e f))))

;; The below seemed useful, but they aren't yet.
(defun map-tree (fn tree)
  (if (atom tree)
      (funcall fn tree)
      (mapcar (lambda (subtree) (maptree fn subtree)) tree)))

(defun reduce-tree (fn tree &key (initial-value nil initial-value-p))
  (if initial-value-p
      (-reduce-tree fn tree initial-value)
      (reduce fn (mapcar (lambda (x) (-reduce-tree fn x (car tree))) (cdr tree)))))

(defun -reduce-tree (fn tree initial-value)
  (if (atom tree)
      (funcall fn initial-value tree)
      (let ((results (mapcar
		      (lambda (x) (-reduce-tree fn x initial-value))
		      tree)))
	(reduce fn results))))

(defun filter-tree (pred tree))

'(users nick)
(defun path-exists (path tree)
  (if (endp path)
      tree
      (if-not-let ((next (assoc (car path) (cdr tree))))
		  nil
		  (path-exists (cdr path) next))))

(defun route (path &key (route-tree *test*))
  (let ((path-syms (mapcar
		    (lambda (name) (intern (string-upcase name)))
		    path)))
    (labels ((recur (subpath route-subtree)
	       (if-not-let ((next (assocdr (car subpath) route-subtree)))
			   "not found"
			   (if (null (cdr)))))))))
