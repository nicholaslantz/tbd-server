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
		       (hello "Hello!")
		       (users
			(nick "nick")
			(evan "evan")
			(joe "joe"))))

;; In the case of users, it seems a little silly to have a different render
;; routine for each user.  I should allow more detail to be able to be passed
;; to the render engine.  For instance:
'(defun render-user (user))
;; What would be really cool is if the extra detail is automatically passed into
;; the function through the params.  The render always has the option to fail
;; if the user doesn't exist or whatever.  So what now?
;;
;; This problem is also perfect for OO as I have it set up.

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

;; TODO
(defun filter-tree (pred tree)
  tree)

(defun path-exists (path tree &key (best-match-p t) (best-path nil))
  (if (endp path)
      tree
      (if-not-let ((next (assoc (car path) (cdr tree))))
		  (if (not best-match-p)
		      nil
		      (list best-path path))
		  (path-exists (cdr path) next
			       :best-match-p best-match-p
			       :best-path (cons (car next) best-path)))))

(defun path-best-match (path tree)
  (-path-best-match path tree))

(defun route (path &key (route-tree *test*))
  (let ((path-syms (mapcar
		    (lambda (name) (intern (string-upcase name)))
		    path)))
    (labels ((recur (subpath route-subtree)
	       (if-not-let ((next (assocdr (car subpath) route-subtree)))
			   "not found"
			   (if (null (cdr)))))))))
