;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               sum-squares.lisp
;;;;
;;;;   Started:            Mon May  3 15:37:23 2021
;;;;   Modifications:
;;;;
;;;;   Purpose: Ch. 6 §6.16
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

(defpackage :sum-squares (:use :common-lisp :lang :test))

(in-package :sum-squares)

(defun sum-squares (f m n)
  (loop for k from m to n
        for val = (funcall f k)
        summing (* val val)))

(format t "First computation: ~F~%" (sum-squares #'sin 2d0 13d0))
(format t "Second computation: ~F~%" (sum-squares #'/ 1d0 1d4))
