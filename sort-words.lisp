;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               sort-words.lisp
;;;;
;;;;   Started:            Tue Apr 27 16:53:10 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
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

(defpackage :sort-words (:use :common-lisp :lang :test))

(in-package :sort-words)

(defclass word-stream ()
  ((source :initarg :source)
   (openp :initform t))
  (:documentation "A word stream encapsulates an input source and provides access to the successive words in the stream until exhausted."))

(defun make-word-stream (file)
  (let ((source (open file :direction :input)))
    (make-instance 'word-stream :source source)))

(defgeneric closedp (word-stream)
  (:documentation "Is the word stream closed?"))
(defmethod closedp ((ws word-stream))
  (with-slots (openp) ws
    (not openp)))

(defgeneric exhaustedp (word-stream)
  (:documentation "Have all words been removed from the stream?"))
(defmethod exhaustedp ((ws word-stream))
  (if (closedp ws)
      t
      (with-slots (source) ws
        (cond ((interactive-stream-p source)
               (let ((ch (peek-char nil source nil nil)))
                 (null ch)))
              ((listen source) nil)
              (t (with-slots (openp) ws
                   (close source)
                   (setf openp nil))
                 t)))) )

(defgeneric read-word (word-stream)
  (:documentation "Read the next word from the word stream."))
(defmethod read-word ((ws word-stream))
  (with-slots (source) ws
    (loop for ch = (safe-read-char ws)
          never (exhaustedp ws)
          until (alpha-char-p ch)
          finally (unread-char ch source))
    (unless (exhaustedp ws)
      (with-output-to-string (word)
        (loop for ch = (safe-read-char ws)
              never (exhaustedp ws)
              while (alpha-char-p ch)
              do (write-char ch word)))) ))

(defun safe-read-char (word-stream)
  "Safely read a character from the stream. Returns NIL if none remains."
  (if (exhaustedp word-stream)
      nil
      (with-slots (source) word-stream
        (read-char source))))
