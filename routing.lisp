(defpackage :routing
  (:use :cl :bibliotheca :cl-ppcre)
  (:shadowing-import-from :cl-ppcre :split)
  (:export :route :defroute))
(in-package :routing)

;; TODO: Lots of unused code in here, remove it once it's refiled.

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

;; This funcion should not prune more than it has to.  That is,
;; it should only prune leaves, never branches of the tree.
(defun path-prune (tree path)
  "TODO"
  (list tree path))

(defun path-exists (tree path)
  (cond ((endp path) (if (null tree) t tree))
	((null (assoc (car path) tree)) nil)
	(t (path-exists (assocdr (car path) tree) (cdr path)))))

;; TODO: make wildcar symbol configurable
(defun path-exists-wildcard (tree path &optional (acc nil))
  (cond ((endp path) (values (car tree) (nreverse acc)))
	((null (assoc (car path) tree))
	 (if (and (assoc '* tree) (not (keywordp (car path))))
	     (path-exists-wildcard (assocdr '* tree)
				   (cdr path)
				   (cons (car path) acc))
	     nil))
	(t (path-exists-wildcard (assocdr (car path) tree)
				 (cdr path)
				 acc))))

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

(define-condition path-already-exists (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Path already exists in the route tree."))))

(defparameter *routes* nil)

;; FIXME: The (list (list ,method ... shouldn't need to be there,
;;        find a way to use quotes/commas.
(defmacro defroute (path lambda-list
		    (&key (method :get) (tree '*routes*))
		    &body handler)
  (let ((full-path (gensym)))
    (setf full-path (append path (list method)))
    `(progn
       (when (path-exists ,tree ',full-path)
	 (cerror "Modify the entry"
		 'path-already-exists))
       (setf ,tree
	     (path-merge ,tree (append
				',path
				(list (list ,method
					    (lambda ,lambda-list ,@handler)))))))))
(defroute () ()
    ()
  "Home Page.")

(defroute (about) ()
    ()
  "About us")

(defroute (about careers) ()
    ()
  "Come work for us!")

(defroute (users * *) (user project)
    ()
  (format nil "~a: ~a" user project))

(defun route (path &optional (method :get) (tree *routes*))
  (let ((sympath (~>> path
		   (split "/")
		   (remove-if (lambda (n) (string= "" n)))
		   (mapcar #'string-upcase)
		   (mapcar #'intern))))
    (multiple-value-bind (handler vars)
	(path-exists-wildcard tree (append sympath (list method)))
      (if handler
	  (apply handler vars)
	  "Not Found"))))
