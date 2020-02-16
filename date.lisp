(defpackage :date
  (:use :cl :bibliotheca)
  (:export :datetime))
(in-package :date)

(defparameter *days-of-week*
  #(Mon Tue Wed Thu Fri Sat Sun))

(defparameter *months*
  #(Jan Feb Mar Apr May Jun
    Jul Aug Sep Oct Nov Dec))

(defun datetime (&optional (universal-time (get-universal-time)))
  (multiple-value-bind
	(sec min hr date month year weekday)
      (decode-universal-time universal-time)
    (format nil
	    "~:(~A~), ~D ~:(~A~) ~D ~2,'0D:~2,'0D:~2,'0D"
	    (aref *days-of-week* weekday)
	    date (aref *months* (- month 1)) year
	    hr min sec)))
