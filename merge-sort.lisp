;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               merge-sort.lisp
;;;;
;;;;   Started:            Fri Apr  2 02:42:12 2021
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
;;;;   Notes: All perform stable merge.
;;;;   Initial example from book requires that array length is power of 2!
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :merge-sort (:use :common-lisp :test) (:shadow :merge))

(in-package :merge-sort)

;;;
;;;    Original arrays not modified. New merged array created.
;;;    
;; (defun merge-fp (a b)
;;   (let* ((length-a (length a))
;;          (length-b (length b))
;;          (result (make-array (+ length-a length-b))))
;;     (do ((i 0)
;;          (j 0)
;;          (k 0 (1+ k)))
;;         ((cond ((= i length-a) (setf (subseq result k) (subseq b j)))
;;                ((= i length-b) (setf (subseq result k) (subseq a i)))
;;                (t nil)))
;;       (let ((elt-a (aref a i))
;;             (elt-b (aref b j)))
;;         (cond ((< elt-a elt-b)
;;                (setf (aref result k) elt-a)
;;                (incf i))
;;               (t (setf (aref result k) elt-b)
;;                  (incf j)))) )
;;     result))

(defun merge-fp (a b pred)
  (let* ((length-a (length a))
         (length-b (length b))
         (result (make-array (+ length-a length-b))))
    (do ((i 0)
         (j 0)
         (k 0 (1+ k)))
        ((cond ((= i length-a) (setf (subseq result k) (subseq b j)))
               ((= i length-b) (setf (subseq result k) (subseq a i)))
               (t nil)))
      (let ((elt-a (aref a i))
            (elt-b (aref b j)))
        (cond ((funcall pred elt-b elt-a)
               (setf (aref result k) elt-b)
               (incf j))
              (t (setf (aref result k) elt-a)
                 (incf i)))) )
    result))

;; (defun merge-fp (a b)
;;   (let* ((length-a (length a))
;;          (length-b (length b))
;;          (result (make-array (+ length-a length-b))))
;;     (labels ((do-merge (i j k)
;;                (cond ((= i length-a) (setf (subseq result k) (subseq b j)))
;;                      ((= j length-b) (setf (subseq result k) (subseq a i)))
;;                      (t (let ((elt-a (aref a i))
;;                               (elt-b (aref b j)))
;;                           (cond ((< elt-a elt-b)
;;                                  (setf (aref result k) elt-a)
;;                                  (do-merge (1+ i) j (1+ k)))
;;                                 (t (setf (aref result k) elt-b)
;;                                    (do-merge i (1+ j) (1+ k)))) )))) )
;;       (do-merge 0 0 0))
;;     result))

(defun merge-fp (a b pred)
  (let* ((length-a (length a))
         (length-b (length b))
         (result (make-array (+ length-a length-b))))
    (labels ((do-merge (i j k)
               (cond ((= i length-a) (setf (subseq result k) (subseq b j)))
                     ((= j length-b) (setf (subseq result k) (subseq a i)))
                     (t (let ((elt-a (aref a i))
                              (elt-b (aref b j)))
                          (cond ((funcall pred elt-b elt-a)
                                 (setf (aref result k) elt-b)
                                 (do-merge i (1+ j) (1+ k)))
                                (t (setf (aref result k) elt-a)
                                   (do-merge (1+ i) j (1+ k)))) )))) )
      (do-merge 0 0 0))
    result))

;; (defun merge (a b)
;;   (loop with i = 0
;;         with j = 0
;;         for k from 0
;;         until (or (= i (length a)) (= j (length b)))
;;         collect (if (< (aref a i) (aref b j)) (prog1 (aref a i) (incf i)) (prog1 (aref b j) (incf j))) into result ; Ugh!
;;         finally (return (concatenate 'vector result (if (= i (length a)) (subseq b j) (subseq a i)))) ))

;; (defun merge-loop (a b)
;;   (let ((i 0)
;;         (j 0))
;;     (labels ((choose (elt-a elt-b)
;;                (cond ((< elt-a elt-b) (incf i) elt-a)
;;                      (t (incf j) elt-b)))
;;              (remnant ()
;;                (if (= i (length a))
;;                    (subseq b j)
;;                    (subseq a i))))
;;       (loop for k from 0
;;             until (or (= i (length a)) (= j (length b)))
;;             collect (choose (aref a i) (aref b j)) into result
;;             finally (return (concatenate 'vector result (remnant)))) )))

(defun merge-loop (a b pred)
  (let ((i 0)
        (j 0))
    (labels ((choose (elt-a elt-b)
               (cond ((funcall pred elt-b elt-a) (incf j) elt-b)
                     (t (incf i) elt-a)))
             (remnant ()
               (if (= i (length a))
                   (subseq b j)
                   (subseq a i))))
      (loop for k from 0
            until (or (= i (length a)) (= j (length b)))
            collect (choose (aref a i) (aref b j)) into result
            finally (return (concatenate 'vector result (remnant)))) )))

;;;
;;;    This is generalized beyond MERGE-SORT-ABC. Will merge any 2 input arrays of any length.
;;;    In MERGE-SORT-ABC, A and B are both the same source array and COUNT-A and COUNT-B are identical.
;;;    
;; (defun merge-abc (a b c index-a index-b index-c count-a count-b)
;;   "Merge COUNT-A elements of array A starting at INDEX-A with COUNT-B elements of array B starting at INDEX-B into destination array C."
;;   (labels ((do-merge (i j k)
;;              (cond ((= i count-a) (setf (subseq c (+ k index-c)) (subseq b (+ j index-b) (+ index-b count-b))))
;;                    ((= j count-b) (setf (subseq c (+ k index-c)) (subseq a (+ i index-a) (+ index-a count-a))))
;;                    (t (let ((elt-a (aref a (+ i index-a)))
;;                             (elt-b (aref b (+ j index-b))))
;;                         (cond ((< elt-a elt-b)
;;                                (setf (aref c (+ k index-c)) elt-a)
;;                                (do-merge (1+ i) j (1+ k)))
;;                               (t (setf (aref c (+ k index-c)) elt-b)
;;                                  (do-merge i (1+ j) (1+ k)))) )))) )
;;     (do-merge 0 0 0)))

(defun merge-abc (a b c index-a index-b index-c count-a count-b pred)
  "Merge COUNT-A elements of array A starting at INDEX-A with COUNT-B elements of array B starting at INDEX-B into destination array C."
  (labels ((do-merge (i j k)
             (cond ((= i count-a) (setf (subseq c (+ k index-c)) (subseq b (+ j index-b) (+ index-b count-b))))
                   ((= j count-b) (setf (subseq c (+ k index-c)) (subseq a (+ i index-a) (+ index-a count-a))))
                   (t (let ((elt-a (aref a (+ i index-a)))
                            (elt-b (aref b (+ j index-b))))
                        (cond ((funcall pred elt-b elt-a)
                               (setf (aref c (+ k index-c)) elt-b)
                               (do-merge i (1+ j) (1+ k)))
                              (t (setf (aref c (+ k index-c)) elt-a)
                                 (do-merge (1+ i) j (1+ k)))) )))) )
    (do-merge 0 0 0)))

;;;
;;;    A and B are actual elements of the original array being sorted. C is an actual subsequence of
;;;    the WORK-AREA rather than a copy.
;;;    
;; (defun merge-abc-displaced (a b c)
;;   "Merge elements of A and B into C."
;;   (let ((length-a (length a))
;;         (length-b (length b)))
;;     (labels ((do-merge (i j k)
;;                (cond ((= i length-a) (setf (subseq c k) (subseq b j)))
;;                      ((= j length-b) (setf (subseq c k) (subseq a i)))
;;                      (t (let ((elt-a (aref a i))
;;                               (elt-b (aref b j)))
;;                           (cond ((< elt-a elt-b)
;;                                  (setf (aref c k) elt-a)
;;                                  (do-merge (1+ i) j (1+ k)))
;;                                 (t (setf (aref c k) elt-b)
;;                                    (do-merge i (1+ j) (1+ k)))) )))) )
;;       (do-merge 0 0 0))))

(defun merge-abc-displaced (a b c pred)
  "Merge elements of A and B into C."
  (let ((length-a (length a))
        (length-b (length b)))
    (labels ((do-merge (i j k)
               (cond ((= i length-a) (setf (subseq c k) (subseq b j)))
                     ((= j length-b) (setf (subseq c k) (subseq a i)))
                     (t (let ((elt-a (aref a i))
                              (elt-b (aref b j)))
                          (cond ((funcall pred elt-b elt-a)
                                 (setf (aref c k) elt-b)
                                 (do-merge i (1+ j) (1+ k)))
                                (t (setf (aref c k) elt-a)
                                   (do-merge (1+ i) j (1+ k)))) )))) )
      (do-merge 0 0 0))))

;;;
;;;    Divide and conquer.
;;;    
;; (defun merge-sort-fp (a)
;;   (cond ((= (length a) 1) a)
;;         (t (multiple-value-bind (left right) (split a)
;;              (merge-fp (merge-sort-fp left) (merge-sort-fp right)))) ))

;; (defun split (xs)
;;   (let ((n (truncate (length xs) 2)))
;;     (values (subseq xs 0 n) (subseq xs n))))

;; (defun merge-sort-fp (a)
;;   (let* ((n (length a))
;;          (split (truncate n 2)))
;;     (cond ((<= n 1) a)
;;           (t (merge-fp (merge-sort-fp (subseq a 0 split)) (merge-sort-fp (subseq a split)))) )))

(defun merge-sort-fp (a pred)
  (let* ((n (length a))
         (split (truncate n 2)))
    (cond ((<= n 1) a)
          (t (merge-fp (merge-sort-fp (subseq a 0 split) pred) (merge-sort-fp (subseq a split) pred) pred)))) )

;; (defun merge-sort (a)
;;   "Sort the array A in place using the merge sort algorithm."
;;   (let* ((n (length a))
;;          (work-area (make-array n)))
;;     (do ((merge-size 1 (* merge-size 2)))
;;         ((>= merge-size n) a)
;;       (do ((j 0 (+ j (* 2 merge-size))))
;;           ((>= j (- n merge-size)))
;;         (setf (subseq work-area j)
;;               (merge-fp (subseq a j (+ j merge-size))
;;                         (subseq a (+ j merge-size) (+ j merge-size merge-size)))) )
;;       (setf (subseq a 0) (subseq work-area 0)))) )

(defun merge-sort (a pred)
  "Sort the array A in place using the merge sort algorithm."
  (let* ((n (length a))
         (work-area (make-array n)))
    (do ((merge-size 1 (* merge-size 2)))
        ((>= merge-size n) a)
      (do ((j 0 (+ j (* 2 merge-size))))
          ((>= j (- n merge-size)))
        (setf (subseq work-area j)
              (merge-fp (subseq a j (+ j merge-size))
                        (subseq a (+ j merge-size) (+ j merge-size merge-size))
                        pred)))
      (setf (subseq a 0) (subseq work-area 0)))) )

;;;
;;;    Sort in place. No need for WORK-AREA since MERGE-FP creates new array.
;;;    
;; (defun merge-sort-semi-fp (a)
;;   "Sort the array A in place using the merge sort algorithm."
;;   (let ((n (length a)))
;;     (do ((merge-size 1 (* merge-size 2)))
;;         ((>= merge-size n) a)
;;       (do ((j 0 (+ j (* 2 merge-size))))
;;           ((>= j (- n merge-size)))
;;         (setf (subseq a j)
;;               (merge-fp (subseq a j (+ j merge-size))
;;                         (subseq a (+ j merge-size) (+ j merge-size merge-size)))) ))))

(defun merge-sort-semi-fp (a pred)
  "Sort the array A in place using the merge sort algorithm."
  (let ((n (length a)))
    (do ((merge-size 1 (* merge-size 2)))
        ((>= merge-size n) a)
      (do ((j 0 (+ j (* 2 merge-size))))
          ((>= j (- n merge-size)))
        (setf (subseq a j)
              (merge-fp (subseq a j (+ j merge-size))
                        (subseq a (+ j merge-size) (+ j merge-size merge-size))
                        pred)))) ))

;;;
;;;    Merge WORK-AREA in place. No pointers to subsequences in Lisp. Must pass in indices explicitly.
;;;    
;; (defun merge-sort-abc (a)
;;   "Sort the array A in place using the merge sort algorithm."
;;   (let* ((n (length a))
;;          (work-area (make-array n)))
;;     (do ((merge-size 1 (* merge-size 2)))
;;         ((>= merge-size n) a)
;;       (do ((j 0 (+ j (* 2 merge-size))))
;;           ((>= j (- n merge-size)))
;;         (merge-abc a a work-area j (+ j merge-size) j merge-size merge-size))
;;       (setf (subseq a 0) (subseq work-area 0)))) )

(defun merge-sort-abc (a pred)
  "Sort the array A in place using the merge sort algorithm."
  (let* ((n (length a))
         (work-area (make-array n)))
    (do ((merge-size 1 (* merge-size 2)))
        ((>= merge-size n) a)
      (do ((j 0 (+ j (* 2 merge-size))))
          ((>= j (- n merge-size)))
        (merge-abc a a work-area j (+ j merge-size) j merge-size merge-size pred))
      (setf (subseq a 0) (subseq work-area 0)))) )

;;;
;;;    This is most like the C implementation. It merges actual elts of A into actual elts
;;;    of WORK-AREA rather than extracting a subsequence and then re-assigning after.
;;;
;;;    We are abandoning FP for the sake of minimizing memory use.
;;;    Currently, length of A must be power of 2!!!!
;;;    
;; (defun merge-sort-abc-displaced (a)
;;   "Sort the array A in place using the merge sort algorithm."
;;   (let* ((n (length a))
;;          (work-area (make-array n)))
;;     (do ((merge-size 1 (* merge-size 2)))
;;         ((>= merge-size n) a)
;;       (do ((j 0 (+ j (* 2 merge-size))))
;;           ((>= j (- n merge-size)))
;;         (merge-abc-displaced (make-array merge-size :displaced-to a :displaced-index-offset j)
;;                              (make-array merge-size :displaced-to a :displaced-index-offset (+ j merge-size))
;;                              (make-array (* 2 merge-size) :displaced-to work-area :displaced-index-offset j)))
;;       (setf a (copy-seq work-area)))) )

(defun merge-sort-abc-displaced (a pred)
  "Sort the array A in place using the merge sort algorithm."
  (let* ((n (length a))
         (work-area (make-array n)))
    (do ((merge-size 1 (* merge-size 2)))
        ((>= merge-size n) a)
      (do ((j 0 (+ j (* 2 merge-size))))
          ((>= j (- n merge-size)))
        (merge-abc-displaced (make-array merge-size :displaced-to a :displaced-index-offset j)
                             (make-array merge-size :displaced-to a :displaced-index-offset (+ j merge-size))
                             (make-array (* 2 merge-size) :displaced-to work-area :displaced-index-offset j)
                             pred))
      (setf a (copy-seq work-area)))) )

(deftest test-merge-sort (f)
  (check
   (equalp (funcall f (vector -7) #'<) #(-7))
   (equalp (funcall f (vector 2 2.0d0) #'<) #(2 2.0d0))
   (equalp (funcall f (vector 2.0d0 2) #'<) #(2.0d0 2))
   (equalp (funcall f (vector -3 -1 -4 4 6 -8 8 -5 -5 -9 2 -1 -5 10 5 1 10 -3 8 6 -4 -1 8 2 6 10) #'<)
           #(-9 -8 -5 -5 -5 -4 -4 -3 -3 -1 -1 -1 1 2 2 4 5 6 6 6 8 8 8 10 10 10))
   (equalp (funcall f (vector -7 -3 -2 9 6 -4 10 9 -4 1 1 -4) #'<)
           #(-7 -4 -4 -4 -3 -2 1 1 6 9 9 10))
   (equalp (funcall f (vector 1 -4 2 -4 7 2 -5 3 4 5 3 -5 3 -6 7 -2 -6 8 1 0 -1 7 -1) #'<)
           #(-6 -6 -5 -5 -4 -4 -2 -1 -1 0 1 1 2 2 3 3 3 4 5 7 7 7 8))
   (equalp (funcall f (vector -5 -7 9 9 -4 2 5 9 9 4 3 8 -6 10 -9 -3 0 5 2 2 8 3 0 -3) #'<)
           #(-9 -7 -6 -5 -4 -3 -3 0 0 2 2 2 3 3 4 5 5 8 8 9 9 9 9 10))
   (equalp (funcall f (vector -8 -4 1 2 -7 -4 -9 -7 -1 -1 -9 -8 2 -9 6 -2 10 -7 2 2 7 -7 -1 -5 4 -3 -9 7 -9) #'<)
           #(-9 -9 -9 -9 -9 -8 -8 -7 -7 -7 -7 -5 -4 -4 -3 -2 -1 -1 -1 1 2 2 2 2 4 6 7 7 10))
   (equalp (funcall f (vector 2 6 2 -7 10 -4 -9 -9 5 -6 3) #'<)
           #(-9 -9 -7 -6 -4 2 2 3 5 6 10))
   (equalp (funcall f (vector 5 8 -8 5 -3 4 0 2 -4 -1 -6 2 8 -4 0 -5 -5) #'<)
           #(-8 -6 -5 -5 -4 -4 -3 -1 0 0 2 2 4 5 5 8 8))
   (equalp (funcall f (vector 10 0 3 9 7 -4 -8 0 -9 5 0 2 -9 9 1 -5 -2 -7 8 -5 -2 -8 -9 7 5 -3 4) #'<)
           #(-9 -9 -9 -8 -8 -7 -5 -5 -4 -3 -2 -2 0 0 0 1 2 3 4 5 5 7 7 8 9 9 10))
   (equalp (funcall f (vector -7 1 6 9 8 -2 10 -9 0 1 -8 7 4 4 -6 10 9 -6 -9 -5 2 3 -5 8) #'<)
           #(-9 -9 -8 -7 -6 -6 -5 -5 -2 0 1 1 2 3 4 4 6 7 8 8 9 9 10 10))
   (equalp (funcall f (vector -4 -7 -7 10 -8 -2 -9 -8 1 -6 3 -5 -8 3 3 -3 -4 4 0 6 -3 -6 5 7 1 6 2 6) #'<)
           #(-9 -8 -8 -8 -7 -7 -6 -6 -5 -4 -4 -3 -3 -2 0 1 1 2 3 3 3 4 5 6 6 6 7 10))
   (equalp (funcall f (vector 5 8 -8 5 -3 4 0 2 -4 -1 -6 2 8 -4 0 -5 -5) #'>)
           #(8 8 5 5 4 2 2 0 0 -1 -3 -4 -4 -5 -5 -6 -8))
   (equalp (funcall f (vector 5 8 -8 5 -3 4 0 2 -4 -1 -6 2 8 -4 0 -5 -5) #'(lambda (a b) (< (abs a) (abs b))))
           #(0 0 -1 2 2 -3 4 -4 -4 5 5 -5 -5 -6 8 -8 8))
   (equalp (funcall f (vector '(1 2 3) '(4 5) '(6 7 8 9) '(10) '(11)) #'(lambda (a b) (< (length a) (length b))))
           #((10) (11) (4 5) (1 2 3) (6 7 8 9)))) )

;;;
;;;    For algorithms that require power of 2 length...
;;;    
(deftest test-merge-sort-fussy (f)
  (check
   (equalp (funcall f (vector -7) #'<) #(-7))
   (equalp (funcall f (vector 2 2.0d0) #'<) #(2 2.0d0))
   (equalp (funcall f (vector 2.0d0 2) #'<) #(2.0d0 2))
   (equalp (funcall f (vector -4 -1) #'<) #(-4 -1))
   (equalp (funcall f (vector 1 -1 6 -1) #'<) #(-1 -1 1 6))
   (equalp (funcall f (vector 6 9 7 2 1 -1 -2 0) #'<) #(-2 -1 0 1 2 6 7 9))
   (equalp (funcall f (vector 8 -6 -9 2 -4 -5 0 1 4 0 10 -7 -6 8 8 0) #'<)
           #(-9 -7 -6 -6 -5 -4 0 0 0 1 2 4 8 8 8 10))
   (equalp (funcall f (vector 2 2 -7 9 -8 -2 8 -4 3 -7 -2 8 -6 1 4 5 3 1 7 -8 3 0 10 1 -4 3 1 2 -7 1 -8 4) #'<)
           #(-8 -8 -8 -7 -7 -7 -6 -4 -4 -2 -2 0 1 1 1 1 1 2 2 2 3 3 3 3 4 4 5 7 8 8 9 10))
   (equalp (funcall f (vector 5 8 -8 5 -3 4 0 2 -4 -1 -6 2 8 -4 0 -5) #'>)
           #(8 8 5 5 4 2 2 0 0 -1 -3 -4 -4 -5 -6 -8))
   (equalp (funcall f (vector 5 8 -8 5 -3 4 0 2 -4 -1 -6 2 8 -4 0 -5) #'(lambda (a b) (< (abs a) (abs b))))
           #(0 0 -1 2 2 -3 4 -4 -4 5 5 -5 -6 8 -8 8))))

