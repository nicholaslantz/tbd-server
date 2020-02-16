(defpackage :date
  (:use :cl :bibliotheca)
  (:export :datetime))
(in-package :date)

(defparameter *days-of-week*
  #(Mon Tue Wed Thu Fri Sat Sun))

(defparameter *months*
  #(Jan Feb Mar Apr May Jun
    Jul Aug Sep Oct Nov Dec))

(defparameter *timezone*
  (~>> (get-universal-time)
    (decode-universal-time)
    (multiple-value-list)
    (nth 8)))

(defun datetime (&optional (universal-time (get-universal-time)))
  (let ((utc-time (+ universal-time (* *timezone* 60 60))))
    (multiple-value-bind
	  (sec min hr date month year weekday)
	(decode-universal-time utc-time)
      (format nil
	      "~:(~A~), ~D ~:(~A~) ~D ~2,'0D:~2,'0D:~2,'0D GMT"
	      (aref *days-of-week* weekday)
	      date (aref *months* (- month 1)) year
	      hr min sec))))
