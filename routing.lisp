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

;; Dude, this could get really complex and magical if I want it to...
(defroute (about) about () :get
	  "hello.")
(defroute (users) user-projects (user project) :get
	  "User 1, project 5.")

(defparameter *test*
  '((about
     :get sym)
    (users
     (*
      (*
       :get sym)))))

(defparameter *test2*
  '((about
     :post sym)))

(defparameter *test3*
  '((careers
     :get sym)))

(defun shallow-merge (&rest alists)
  (labels ((rec (t1 t2)
	     (if (null t2)
		 t1
		 (if (assoc (caar t2) t1)
		     (rec (cons (car t2)
				(remove (assoc (caar t2) t1)
					t1
					:test #'equal))
			  (cdr t2))
		     (rec (cons (car t2) t1) (cdr t2))))))
    (reduce #'rec alists)))

(defun deep-merge (&rest trees)
  (labels ((rec-merge (t1 t2)
	     (if (and (consp t1) (consp t2))
		 (deep-merge t1 t2)
		 t2)))
    (if (consp trees)
	(reduce (lambda (a b) (rec-merge a b)) trees)
	(car trees))))

(defun deep-merge (&rest trees)
  (labels ((rec (&rest ts)
	     (if (some (lambda (a) (consp a)) ts)
		 (apply rec ts))))))


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
