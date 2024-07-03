#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               kepler.lisp
;;;;
;;;;   Started:            Thu May  6 19:07:32 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   ABC §6.17
;;;;   Solve Kepler's equation m = x - e sin(x) for specific values of m, e.
;;;;   Equivalently: y = x = m + e sin(x)
;;;;   Find intersection between y = x and y = m + e sin(x), i.e.,
;;;;     y = x
;;;;   -(y = m + e sin(x))
;;;;   -------------------
;;;;     0 = x - e sin(x) - m
;;;;
;;;;   Use bisection function to aproximate root.
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
(load "/home/slytobias/lisp/books/ABC/bisect.lisp")

(defpackage :kepler (:use :common-lisp :lang :test))

(in-package :kepler)

(defconstant epsilon 1d-15)

(defun kepler (x e m)
  (- x (* e (sin x)) m))

(let* ((e 0.5d0)
       (m 2.2d0)
       (x (bisect:root #'(lambda (x) (kepler x e m)) -100d0 100d0 epsilon)))
  (format t "Approximate root: ~F~%" x)
  (format t "Function value: ~F~%" (kepler x e m)))
