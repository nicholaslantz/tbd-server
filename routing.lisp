(defpackage :routing
  (:use :cl :bibliotheca :cl-ppcre)
  (:shadowing-import-from :cl-ppcre :split)
  (:export :route))
(in-package :routing)

(defparameter *test*
  '(root
    (files
     (:get (:path) root-files-get)
     (:post (:path)))
    (about
     (us)
     (careers))))

;; Do we really want to copy the function names for each of these?
;; The decorator pattern at least reduces that boilerplate.  I could
;; define a function that generates this tree depending on what
;; has been defined so for.  That's not too bad.  Hmm... I still get to
;; salvage my work as the tree still exists, I just don't define it
;; explicitly.

'((user 'string) (project 'string) (page 'string))
'((user 'string) (project 'string))
'(root
  ((user string)
   ()))

(defmacro defroute (name path &key (method :get) (routes *test*) &body body)
  (progn
    (add-branch path *test*)
    (defun name)))

(defroute get-user-project (users name* project*)
	  :users)
(defroute set-user-project (users name* project*) :method :post)
(defroute get-user (users name*))

'(users
  (name*
   :get get-user
   (project*
    :get get-user-project
    :post set-user-project)))


(defun add-branch (path tree)
  (if (not (eql (car path) (car tree)))
      (append path tree)
      (let ((child (find (cadr path) (cdr tree) :key #'car)))
	(append (remove child tree) (list (add-branch (cdr path) child))))))

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

(defun route (path routes)
  "TODO: The data formats are tricky to specify."
  (route-syms
   (mapcar (lambda (n) (intern (string-upcase n))) (cdr (split "/" path)))
   routes))

(defun route-syms (path tree &optional (breadcrumbs nil))
  (cond ((endp tree)
	 :not-found)
	((or (endp path) (atom (cdr tree)))
	 (values (list (cdr tree) path) (reverse breadcrumbs)))
	(t (route-syms
	    (cdr path)
	    (assoc (car path) (cdr tree))
	    (cons (car path) breadcrumbs)))))
