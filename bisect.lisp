;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               bisect.lisp
;;;;
;;;;   Started:            Mon May  3 15:53:09 2021
;;;;   Modifications:
;;;;
;;;;   Purpose: Ch. 6 §6.17
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :bisect (:use :common-lisp :lang :test) (:export :root))

(in-package :bisect)

(defconstant epsilon 1d-12)

(defun root (f a b &optional (eps epsilon))
  (labels ((opposite-sign-p (a b)
             (minusp (* a b)))
           (find-root (low high)
;             (let ((mid (/ (+ low high) 2)))
;             https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
             (let* ((mid (+ low (/ (- high low) 2)))
                    (f-mid (funcall f mid))
                    (f-low (funcall f low)))
               (cond ((or (zerop f-mid) (< (- high low) eps)) mid)
                     ((opposite-sign-p f-low f-mid) (find-root low mid))
                     (t (find-root mid high)))) ))
  (assert (opposite-sign-p (funcall f a) (funcall f b)) () "Endpoints must have opposite signs.")
  (find-root a b)))
