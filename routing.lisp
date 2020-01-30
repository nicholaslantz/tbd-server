(defpackage :routing
  (:use :cl :bibliotheca :cl-ppcre)
  (:shadowing-import-from :cl-ppcre :split))
(in-package :routing)

(defparameter *test*
  '(root
    (about . serve-about)
    (www . file-server)
    (users . render-user)))

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

(defun route (path tree &optional (breadcrumbs nil))
  (cond ((endp tree)
	 :not-found)
	((endp path)
	 (list (cdr tree) (reverse breadcrumbs)))
	((atom tree) (list tree) (reverse breadcrumbs))
	(t (route (cdr path) (assoc (car path) (cdr tree)) (cons (car path) breadcrumbs)))))
