(defpackage :routing
  (:use :cl :bibliotheca :cl-ppcre)
  (:shadowing-import-from :cl-ppcre :split)
  (:export :route))
(in-package :routing)

;; I still need to decide on the wildcard system.
;;
;; I'm leaning towards * just matching everything for now.

(defmacro defroute (name path &key (method :get) (routes *test*) &body body)
  (progn
    (add-branch path *test*)
    (defun name)))

(defroute get-user-project (users name* project*)
	  :users)
(defroute set-user-project (users name* project*) :method :post)
(defroute get-user (users name*))

(defparameter *routes* nil)

;; FIXME: The (list (list ,method ... shouldn't need to be there,
;;        find a way to use quotes/commas.
(defmacro defroute (path lambda-list handler &key (method :get) (tree '*routes*))
  `(setf ,tree
	 (path-merge ,tree (append ',path (list (list ,method
						      (lambda ,lambda-list ,handler)))))))

(defroute (index) () 
  "Home Page.")

(defroute (about) ()
  "About us")

(defroute (about careers) ()
  "Come work for us!")

(defroute (users * *) (user project)
  (format nil "~a: ~a" user project))

(defparameter *test*
  '((about (:get sym))
    (users (* (* (:get sym))))
    (deeply (nested (branch (:get sym))))))

(defparameter *test2*
  '((users (* (* (:post sym))))
    (about
     (:post hacked)
     (:create haxor)
     (:get new))))

(defun shallow-merge (&rest alists)
  (labels ((rec (t1 t2)
	     (cond ((null t2) t1)
		   ((assoc (caar t2) t1)
		    (rec (cons (car t2)
			       (remove (assoc (caar t2) t1)
				       t1
				       :test #'equal))
			 (cdr t2)))
		   (t (rec (cons (car t2) t1) (cdr t2))))))
    (reduce #'rec alists)))

(defun deep-merge (t1 t2)
  (cond ((null t2) t1)
	((assoc (caar t2) t1)
	 (cons (cons (caar t2)
		     (if (atom (car (assocdr (caar t2) t1)))
			 (append (cdar t2) (assocdr (car t2) t1))
			 (deep-merge (assocdr (caar t2) t1) (cdar t2))))
	       (deep-merge (remove (assoc (caar t2) t1)
				   t1
				   :test #'equal)
			   (cdr t2))))
	(t (deep-merge (cons (car t2) t1) (cdr t2)))))

(defun path-merge (tree path)
  (deep-merge tree (list (reduce #'list path :from-end t))))

(defun path-exists (tree path)
  (cond ((endp path) (if (null tree) t tree))
	((null (assoc (car path) tree)) nil)
	(t (path-exists (assocdr (car path) tree) (cdr path)))))

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

(defun filter-tree (pred tree)
  tree)
