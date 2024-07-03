;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               coins.lisp
;;;;
;;;;   Started:            Wed Mar  3 01:16:14 2021
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
;;;;   Notes: ABC Chapter 5 ex. 24
;;;;   See Programming Clojure 1e (Lazier than Lazy)...
;;;;
;;;;   Count H T T H as 2 consecutive alternating runs??
;;;;
;;;;   Different approaches to FSM below:
;;;;   1. Nested local functions, one per state.
;;;;   2. TAGBODY with labels for each state. Not as desirable as 1. "as is" but maybe
;;;;      when packaged as a macro... See WITH-STATES
;;;;   3. Recursive function/DO loop with explicit state parameter -> CASE
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :coins (:use :common-lisp :test) (:shadow :count))

(in-package :coins)

(defvar *rnd* (make-random-state t))
(defun toss-coin ()
  (if (zerop (random 2 *rnd*))
      :head
      :tail))
;; (defun toss-coin ()
;; (print  (if (< (random 1d0 *rnd*) 0.5)
;;       :head
;;       :tail)) )

;;;
;;;    This first version (first two) is incomplete. It only records consecutive runs of heads/tails not alternating runs.
;;;    
;; (defun trial (&optional (count 100))
;;   (labels ((heads (i heads-run max-heads-run tails-run max-tails-run)
;;              (cond ((zerop i) (list :heads (max heads-run max-heads-run)
;;                                     :tails (max tails-run max-tails-run)))
;;                    (t (ecase (toss-coin)
;;                         (:head (heads (1- i) (1+ heads-run) max-heads-run 0 (max tails-run max-tails-run)))
;;                         (:tail (tails (1- i) 0 (max heads-run max-heads-run) (1+ tails-run) max-tails-run)))) ))
;;            (tails (i heads-run max-heads-run tails-run max-tails-run)
;;              (cond ((zerop i) (list :heads (max heads-run max-heads-run)
;;                                     :tails (max tails-run max-tails-run)))
;;                    (t (ecase (toss-coin)
;;                         (:head (heads (1- i) (1+ heads-run) max-heads-run 0 (max tails-run max-tails-run)))
;;                         (:tail (tails (1- i) 0 (max heads-run max-heads-run) (1+ tails-run) max-tails-run)))) )))
;;     (ecase (toss-coin)
;;       (:head (heads count 1 0 0 0))
;;       (:tail (tails count 0 0 1 0)))) )

;;;
;;;    After initially thinking I needed separate cases above, I noticed the redundancy.
;;;    
;; (defun trial (&optional (count 100))
;;   (labels ((toss (i heads tails heads-run max-heads-run tails-run max-tails-run)
;;              (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,(max heads-run max-heads-run)
;;                                 :tails ,tails (,(percentage tails count)) run ,(max tails-run max-tails-run)))
;;                    (t (ecase (toss-coin)
;;                         (:head (toss (1- i) (1+ heads) tails (1+ heads-run) max-heads-run 0 (max tails-run max-tails-run)))
;;                         (:tail (toss (1- i) heads (1+ tails) 0 (max heads-run max-heads-run) (1+ tails-run) max-tails-run)))) )))
;;     (toss count 0 0 0 0 0 0)))

(defun percentage (bin total)
  (format nil "~,2F%" (* 100d0 (/ bin total))))

;;;
;;;    Abandoned...
;;;    
;; (defun trial (&optional (count 100))
;;   (assert (plusp count))
;;   (labels ((toss0 (i)
;;              (ecase (toss-coin)
;;                (:head (toss1 (1- i) 1 0 1 0 :head))
;;                (:tail (toss1 (1- i) 0 1 0 1 :tail))))
;;            ;; (toss1 (i heads tails heads-run tails-run prev)
;;            ;;   (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,heads-run
;;            ;;                      :tails ,tails (,(percentage tails count)) run ,tails-run))
;;            ;;         (t (ecase (toss-coin)
;;            ;;              (:head (toss (1- i) (1+ heads) tails (1+ heads-run) 0 0 tails-run 0 0 prev :head))
;;            ;;              (:tail (toss (1- i) heads (1+ tails) 0 heads-run (1+ tails-run) 0 0 0 prev :tail)))) ))
;;            (toss1 (i heads tails heads-run tails-run prev)
;;              (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,heads-run
;;                                 :tails ,tails (,(percentage tails count)) run ,tails-run))
;;                    (t (ecase (toss-coin)
;;                         (:head (toss-h (1- i) (1+ heads) tails (1+ heads-run) 0 tails-run 0 0 prev :head))
;;                         (:tail (toss-t (1- i) heads (1+ tails) heads-run (1+ tails-run) 0 0 0 prev :tail)))) ))
;;            (toss-h (i heads tails heads-run max-heads-run max-tails-run alt-run max-alt-run prev curr)
;;              (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,(max heads-run max-heads-run)
;;                                 :tails ,tails (,(percentage tails count)) run ,max-tails-run))
;;                    (t (ecase (toss-coin)
;;                         (:head (toss-h (1- i) (1+ heads) tails (1+ heads-run) max-heads-run max-tails-run alt-run max-alt-run curr :head))
;;                         (:tail (toss-t (1- i) heads (1+ tails) (max heads-run max-heads-run) 1 max-tails-run alt-run max-alt-run curr :tail)))) ))
;;            (toss-t (i heads tails max-heads-run tails-run max-tails-run alt-run max-alt-run prev curr)
;;              (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,max-heads-run
;;                                 :tails ,tails (,(percentage tails count)) run ,(max tails-run max-tails-run)))
;;                    (t (ecase (toss-coin)
;;                         (:head (toss-h (1- i) (1+ heads) tails 1 max-heads-run (max tails-run max-tails-run) alt-run max-alt-run curr :head))
;;                         (:tail (toss-t (1- i) heads (1+ tails) max-heads-run (1+ tails-run) max-tails-run alt-run max-alt-run curr :tail)))) ))
;;            (toss (i heads tails heads-run max-heads-run tails-run max-tails-run alt-run max-alt-run prev curr)
;;              (cond ((zerop i) `(:heads ,heads (,(percentage heads count)) run ,(max heads-run max-heads-run)
;;                                        :tails ,tails (,(percentage tails count)) run ,(max tails-run max-tails-run)))
;;                    (t (ecase (toss-coin)
;;                         (:head (toss (1- i) (1+ heads) tails (1+ heads-run) max-heads-run 0 (max tails-run max-tails-run)))
;;                         (:tail (toss (1- i) heads (1+ tails) 0 (max heads-run max-heads-run) (1+ tails-run) max-tails-run)))) )))
;;     (toss0 count)))

;;;
;;;   FSM - No side effects.
;;;   
;;;   SBCL does TCO on this whole damn thing!!
;;;   * (trial 10000000)
;;;   (:HEADS 5000509 ("50.01%") RUN 23 :TAILS 4999491 ("49.99%") RUN 23 -- 13)
;;;   * (trial 100000000)
;;;   (:HEADS 49999983 ("50.00%") RUN 25 :TAILS 50000017 ("50.00%") RUN 27 -- 13)
;;;   * (trial 1000000000)
;;;   (:HEADS 500010366 ("50.00%") RUN 28 :TAILS 499989634 ("50.00%") RUN 29 -- 15)
;;;   
(defun trial (&optional (count 100))
  (assert (plusp count))
  (labels ((start (i)
             (ecase (toss-coin)
               (:head (h0 (1- i)))
               (:tail (t0 (1- i)))) )
           (stats (heads tails max-heads-run max-tails-run &optional (max-alt-run 0))
             `(:heads ,heads (,(percentage heads count)) run ,max-heads-run
               :tails ,tails (,(percentage tails count)) run ,max-tails-run -- ,max-alt-run))
           (h0 (i)
             (cond ((zerop i) (stats 1 0 1 0))
                   (t (ecase (toss-coin)
                        (:head (hh (1- i) 2 0 2 0 0 0))
                        (:tail (ht (1- i) 1 1 1 1 0 1 0 :alt-run-type :ht)))) ))
           (t0 (i)
             (cond ((zerop i) (stats 0 1 0 1))
                   (t (ecase (toss-coin)
                        (:head (th (1- i) 1 1 1 0 1 1 0 :alt-run-type :th))
                        (:tail (tt (1- i) 0 2 0 2 0 0)))) ))
           (hh (i heads tails heads-run max-heads-run max-tails-run max-alt-run)
             (cond ((zerop i) (stats heads tails (max heads-run max-heads-run) max-tails-run max-alt-run))
                   (t (ecase (toss-coin)
                        (:head (hh (1- i) (1+ heads) tails (1+ heads-run) max-heads-run max-tails-run max-alt-run))
                        (:tail (ht (1- i) heads (1+ tails) (max heads-run max-heads-run) 1 max-tails-run 1 max-alt-run :alt-run-type :ht)))) ))
           (ht (i heads tails max-heads-run tails-run max-tails-run alt-run max-alt-run &key alt-run-type)
             (cond ((zerop i) (stats heads tails max-heads-run (max tails-run max-tails-run) (max alt-run max-alt-run))) ; MAX tails unnecessary???? Only corner case: HT, HHT, HHHT, ...
                   (t (ecase (toss-coin)
                        (:head (th (1- i) (1+ heads) tails 1 max-heads-run (max tails-run max-tails-run) (ecase alt-run-type (:th (1+ alt-run)) (:ht alt-run)) max-alt-run :alt-run-type alt-run-type))
                        (:tail (tt (1- i) heads (1+ tails) max-heads-run (1+ tails-run) max-tails-run (max alt-run max-alt-run)))) )))
           (th (i heads tails heads-run max-heads-run max-tails-run alt-run max-alt-run &key alt-run-type)
             (cond ((zerop i) (stats heads tails (max heads-run max-heads-run) max-tails-run (max alt-run max-alt-run))) ; MAX heads unnecessary????
                   (t (ecase (toss-coin)
                        (:head (hh (1- i) (1+ heads) tails (1+ heads-run) max-heads-run max-tails-run (max alt-run max-alt-run)))
                        (:tail (ht (1- i) heads (1+ tails) (max heads-run max-heads-run) 1 max-tails-run (ecase alt-run-type (:ht (1+ alt-run)) (:th alt-run)) max-alt-run :alt-run-type alt-run-type)))) ))
           (tt (i heads tails max-heads-run tails-run max-tails-run max-alt-run)
             (cond ((zerop i) (stats heads tails max-heads-run (max tails-run max-tails-run) max-alt-run))
                   (t (ecase (toss-coin)
                        (:head (th (1- i) (1+ heads) tails 1 max-heads-run (max tails-run max-tails-run) 1 max-alt-run :alt-run-type :th))
                        (:tail (tt (1- i) heads (1+ tails) max-heads-run (1+ tails-run) max-tails-run max-alt-run)))) )))
    (start count)))

;;;
;;;    Explicit modification of state.
;;;    
(defun trial (&optional (count 100))
  (assert (plusp count))
  (let ((heads 0)
        (tails 0)
        (heads-run 0)
        (max-heads-run 0)
        (tails-run 0)
        (max-tails-run 0)
        (alt-run 0)
        (max-alt-run 0))
    (labels ((start (i)
               (ecase (toss-coin)
                 (:head (incf heads) (incf heads-run) (h0 (1- i)))
                 (:tail (incf tails) (incf tails-run) (t0 (1- i)))) )
             (stats ()
               `(:heads ,heads (,(percentage heads count)) run ,max-heads-run
                 :tails ,tails (,(percentage tails count)) run ,max-tails-run -- ,max-alt-run))
             (h0 (i)
               (cond ((zerop i) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (incf heads-run) (hh (1- i)))
                          (:tail (incf tails) (setf max-heads-run (max heads-run max-heads-run)) (setf heads-run 0 tails-run 1) (incf alt-run) (ht (1- i) :alt-run-type :ht)))) ))
             (t0 (i)
               (cond ((zerop i) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (setf max-tails-run (max tails-run max-tails-run)) (setf heads-run 1 tails-run 0) (incf alt-run) (th (1- i) :alt-run-type :th))
                          (:tail (incf tails) (incf tails-run) (tt (1- i)))) )))
             (hh (i)
               (cond ((zerop i) (setf max-heads-run (max heads-run max-heads-run)) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (incf heads-run) (hh (1- i)))
                          (:tail (incf tails) (setf max-heads-run (max heads-run max-heads-run)) (setf heads-run 0 tails-run 1) (setf alt-run 1) (ht (1- i) :alt-run-type :ht)))) ))
             (ht (i &key alt-run-type)
               (cond ((zerop i) (setf max-tails-run (max tails-run max-tails-run)) (setf max-alt-run (max alt-run max-alt-run)) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (setf max-tails-run (max tails-run max-tails-run)) (setf heads-run 1 tails-run 0) (when (eq alt-run-type :th) (incf alt-run)) (th (1- i) :alt-run-type alt-run-type))
                          (:tail (incf tails) (incf tails-run) (setf max-alt-run (max alt-run max-alt-run)) (tt (1- i)))) )))
             (th (i &key alt-run-type)
               (cond ((zerop i) (setf max-heads-run (max heads-run max-heads-run)) (setf max-alt-run (max alt-run max-alt-run)) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (incf heads-run) (setf max-alt-run (max alt-run max-alt-run)) (hh (1- i)))
                          (:tail (incf tails) (setf max-heads-run (max heads-run max-heads-run)) (setf heads-run 0 tails-run 1) (when (eq alt-run-type :ht) (incf alt-run)) (ht (1- i) :alt-run-type alt-run-type)))) ))
             (tt (i)
               (cond ((zerop i) (setf max-tails-run (max tails-run max-tails-run)) (stats))
                     (t (ecase (toss-coin)
                          (:head (incf heads) (setf max-tails-run (max tails-run max-tails-run)) (setf heads-run 1 tails-run 0) (setf alt-run 1) (th (1- i) :alt-run-type :th))
                          (:tail (incf tails) (incf tails-run) (tt (1- i)))) ))))
      (start count))))


;;;
;;;    Different approach below.
;;;    Use (potentially repeatable) stream of tosses.
;;;
;;;
;;;    File feeder reads from file (Compressed? 1010110)
;;;
(defclass tosser ()
  ((state :initform (make-random-state t)))
  (:documentation "Generates a random coin toss producing either the keyword :HEAD or :TAIL."))

(defun toss (tosser)
  (with-slots (state) tosser
    (if (zerop (random 2 state)) :head :tail)))

(defclass feeder ()
  ((count :initform 0)
   (heads :initform 0)
   (tails :initform 0))
  (:documentation "A FEEDER provides a stream of coin tosses until it is exhausted. It can be reset to produce (in most implementations) the same stream again."))

(defgeneric count (feeder)
  (:documentation "Return the number of tosses already produced by this feeder."))
(defmethod count ((f feeder))
  (with-slots (count) f
    (values count (exhaustedp f))))

(defgeneric heads (feeder)
  (:documentation "Return the number of heads tosses that have been produced by this feeder."))
(defmethod heads ((f feeder))
  (with-slots (heads) f
    (values heads (exhaustedp f))))

(defgeneric tails (feeder)
  (:documentation "Return the number of tails tosses that have been produced by this feeder."))
(defmethod tails ((f feeder))
  (with-slots (tails) f
    (values tails (exhaustedp f))))

(defgeneric feed (feeder)
  (:documentation "Feed one coin toss to the consumer."))
(defmethod feed :around ((f feeder))
  (if (exhaustedp f)
      (error "Feeder is exhausted.")
      (with-slots (count heads tails) f
        (let ((toss (call-next-method)))
          (incf count)
          (ecase toss
            (:head (incf heads))
            (:tail (incf tails)))
          toss))))

(defgeneric exhaustedp (feeder)
  (:documentation "Have all coin tosses from this FEEDER been consumed?"))

(defgeneric reset (feeder)
  (:documentation "Return this feeder to its initial state in order to provide (in most implementations) the same stream again."))
(defmethod reset ((f feeder))
  (with-slots (tosser count heads tails) f
    (setf count 0 heads 0 tails 0)))
  
(defclass random-feeder (feeder)
  ((tosser :initform (make-instance 'tosser))
   (limit :initarg :count))
  (:documentation "Produces a random sequence of LIMIT coin tosses. Upon reset will produce the same number although the exact sequence will be different."))

(defmethod feed ((f random-feeder))
  (with-slots (tosser) f
    (toss tosser)))

(defmethod exhaustedp ((f random-feeder))
  (with-slots (count limit) f
    (= count limit)))

(defclass list-feeder (feeder)
  ((elements :initarg :elements)
   (remaining))
  (:documentation "Produces a fixed sequence of coin tosses. The sequence is either pre-determined if a list is provided or generated on demand if a count is provided."))

(defmethod initialize-instance :after ((f list-feeder) &rest initargs &key count)
  (declare (ignore initargs))
  (with-slots (elements remaining) f
    (unless (null count)
      (let ((tosser (make-instance 'tosser)))
        (setf elements (loop repeat count collect (toss tosser)))) )
    (setf remaining elements)))

(defmethod feed ((f list-feeder))
  (with-slots (remaining) f
    (prog1 (first remaining)
      (setf remaining (rest remaining)))) )

(defmethod exhaustedp ((f list-feeder))
  (with-slots (remaining) f
    (null remaining)))

(defmethod reset ((f list-feeder))
  (with-slots (elements remaining) f
    (setf remaining elements)
    (call-next-method)))

;;;
;;;    Verbose bookkeeping.
;;;    
;; (defun trial (feeder)
;;   (assert (not (exhaustedp feeder)))
;;   (labels ((start ()
;;              (ecase (feed feeder)
;;                (:head (h0))
;;                (:tail (t0))))
;;            (stats (max-heads-run max-tails-run &optional (max-alt-run 0))
;;              (let ((heads (heads feeder))
;;                    (tails (tails feeder))
;;                    (count (count feeder)))
;;                `(:heads ,heads (,(percentage heads count)) run ,max-heads-run
;;                  :tails ,tails (,(percentage tails count)) run ,max-tails-run -- ,max-alt-run)))
;;            (h0 ()
;;              (cond ((exhaustedp feeder) (stats 1 0))
;;                    (t (ecase (feed feeder)
;;                         (:head (hh 2 0 0 0))
;;                         (:tail (ht 1 1 0 1 0 :alt-run-type :ht)))) ))
;;            (t0 ()
;;              (cond ((exhaustedp feeder) (stats 0 1))
;;                    (t (ecase (feed feeder)
;;                         (:head (th 1 0 1 1 0 :alt-run-type :th))
;;                         (:tail (tt 0 2 0 0)))) ))
;;            (hh (heads-run max-heads-run max-tails-run max-alt-run)
;;              (cond ((exhaustedp feeder) (stats (max heads-run max-heads-run) max-tails-run max-alt-run))
;;                    (t (ecase (feed feeder)
;;                         (:head (hh (1+ heads-run) max-heads-run max-tails-run max-alt-run))
;;                         (:tail (ht (max heads-run max-heads-run) 1 max-tails-run 1 max-alt-run :alt-run-type :ht)))) ))
;;            (ht (max-heads-run tails-run max-tails-run alt-run max-alt-run &key alt-run-type)
;;              (cond ((exhaustedp feeder) (stats max-heads-run (max tails-run max-tails-run) (max alt-run max-alt-run))) ; MAX tails unnecessary???? Only corner case: HT, HHT, HHHT, ...
;;                    (t (ecase (feed feeder)
;;                         (:head (th 1 max-heads-run (max tails-run max-tails-run) (ecase alt-run-type (:th (1+ alt-run)) (:ht alt-run)) max-alt-run :alt-run-type alt-run-type))
;;                         (:tail (tt max-heads-run (1+ tails-run) max-tails-run (max alt-run max-alt-run)))) )))
;;            (th (heads-run max-heads-run max-tails-run alt-run max-alt-run &key alt-run-type)
;;              (cond ((exhaustedp feeder) (stats (max heads-run max-heads-run) max-tails-run (max alt-run max-alt-run))) ; MAX heads unnecessary????
;;                    (t (ecase (feed feeder)
;;                         (:head (hh (1+ heads-run) max-heads-run max-tails-run (max alt-run max-alt-run)))
;;                         (:tail (ht (max heads-run max-heads-run) 1 max-tails-run (ecase alt-run-type (:ht (1+ alt-run)) (:th alt-run)) max-alt-run :alt-run-type alt-run-type)))) ))
;;            (tt (max-heads-run tails-run max-tails-run max-alt-run)
;;              (cond ((exhaustedp feeder) (stats max-heads-run (max tails-run max-tails-run) max-alt-run))
;;                    (t (ecase (feed feeder)
;;                         (:head (th 1 max-heads-run (max tails-run max-tails-run) 1 max-alt-run :alt-run-type :th))
;;                         (:tail (tt max-heads-run (1+ tails-run) max-tails-run max-alt-run)))) )))
;;     (start)))

(defclass run-statistics ()
  ((current-heads-run :initform 0)
   (max-heads-run :initform 0)
   (current-tails-run :initform 0)
   (max-tails-run :initform 0)
   (current-alt-run :initform 0)
   (max-alt-run :initform 0)
   (alt-run-type :initform nil)))

(defun start-heads-run (run-statistics &key start-alt-run)
  (with-slots (current-heads-run current-tails-run max-tails-run current-alt-run alt-run-type) run-statistics
    (setf max-tails-run (max current-tails-run max-tails-run)
          current-tails-run 0
          current-heads-run 1)
    (if (null start-alt-run)
        (when (eq alt-run-type :th) (incf current-alt-run))
        (setf current-alt-run 1 alt-run-type start-alt-run))))

(defun start-tails-run (run-statistics &key start-alt-run)
  (with-slots (current-heads-run current-tails-run max-heads-run current-alt-run alt-run-type) run-statistics
    (setf max-heads-run (max current-heads-run max-heads-run)
          current-heads-run 0
          current-tails-run 1)
    (if (null start-alt-run)
        (when (eq alt-run-type :ht) (incf current-alt-run))
        (setf current-alt-run 1 alt-run-type start-alt-run))))

(defun continue-heads-run (run-statistics)
  (with-slots (current-heads-run current-alt-run max-alt-run) run-statistics
    (setf max-alt-run (max current-alt-run max-alt-run)
          current-alt-run 0)
    (incf current-heads-run)))

(defun continue-tails-run (run-statistics)
  (with-slots (current-tails-run current-alt-run max-alt-run) run-statistics
    (setf max-alt-run (max current-alt-run max-alt-run)
          current-alt-run 0)
    (incf current-tails-run)))

(defun finalize-stats (run-statistics)
  (with-slots (current-heads-run max-heads-run current-tails-run max-tails-run current-alt-run max-alt-run) run-statistics
    (unless (zerop current-heads-run)
      (setf max-heads-run (max current-heads-run max-heads-run)))
    (unless (zerop current-tails-run)
      (setf max-tails-run (max current-tails-run max-tails-run)))
    (unless (zerop current-alt-run)
      (setf max-alt-run (max current-alt-run max-alt-run)))))
  
;;;
;;;    Less verbose. Delegate bookkeeping to RUN-STATISTICS object. More fluent state changes.
;;;    
;; (defun trial (feeder)
;;   (assert (not (exhaustedp feeder)))
;;   (let ((stats (make-instance 'run-statistics)))
;;     (labels ((start ()
;;                (ecase (feed feeder)
;;                  (:head (start-heads-run stats) (h0))
;;                  (:tail (start-tails-run stats) (t0))))
;;              (h0 ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (continue-heads-run stats) (hh))
;;                           (:tail (start-tails-run stats :start-alt-run :ht) (ht)))) ))
;;              (t0 ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (start-heads-run stats :start-alt-run :th) (th))
;;                           (:tail (continue-tails-run stats) (tt)))) ))
;;              (hh ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (continue-heads-run stats) (hh))
;;                           (:tail (start-tails-run stats :start-alt-run :ht) (ht)))) ))
;;              (ht ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (start-heads-run stats) (th))
;;                           (:tail (continue-tails-run stats) (tt)))) ))
;;              (th ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (continue-heads-run stats) (hh))
;;                           (:tail (start-tails-run stats) (ht)))) ))
;;              (tt ()
;;                (cond ((exhaustedp feeder) (finalize-stats stats))
;;                      (t (ecase (feed feeder)
;;                           (:head (start-heads-run stats :start-alt-run :th) (th))
;;                           (:tail (continue-tails-run stats) (tt)))) )))
;;       (start)
;;       stats)))

;;;
;;;    Does this introduce weird scope for STATS?? Relies on side effect to enclosing STATS object...
;;;    
(defmacro branch-on-toss ((feeder stats) head tail)
  (let ((f (gensym))
        (s (gensym)))
    `(let ((,f ,feeder)
           (,s ,stats))
       (cond ((exhaustedp ,f) (finalize-stats ,s))
             (t (let ((stats ,s))
                  (ecase (feed ,f)
                    ,head
                    ,tail)))) )))

;;;
;;;    Same as previous, just condensed due to macro BRANCH-ON-TOSS.
;;;    
(defun trial (feeder)
  (assert (not (exhaustedp feeder)))
  (let ((stats (make-instance 'run-statistics)))
    (labels ((start ()
               (ecase (feed feeder)
                 (:head (start-heads-run stats) (h0))
                 (:tail (start-tails-run stats) (t0))))
             (h0 ()
               (branch-on-toss (feeder stats)
                 (:head (continue-heads-run stats) (hh))
                 (:tail (start-tails-run stats :start-alt-run :ht) (ht))))
             (t0 ()
               (branch-on-toss (feeder stats)
                 (:head (start-heads-run stats :start-alt-run :th) (th))
                 (:tail (continue-tails-run stats) (tt))))
             (hh ()
               (branch-on-toss (feeder stats)
                 (:head (continue-heads-run stats) (hh))
                 (:tail (start-tails-run stats :start-alt-run :ht) (ht))))
             (ht ()
               (branch-on-toss (feeder stats)
                 (:head (start-heads-run stats) (th))
                 (:tail (continue-tails-run stats) (tt))))
             (th ()
               (branch-on-toss (feeder stats)
                 (:head (continue-heads-run stats) (hh))
                 (:tail (start-tails-run stats) (ht))))
             (tt ()
               (branch-on-toss (feeder stats)
                 (:head (start-heads-run stats :start-alt-run :th) (th))
                 (:tail (continue-tails-run stats) (tt)))) )
      (start)
      stats)))

(defun trial-tr (feeder)
  (assert (not (exhaustedp feeder)))
  (let ((stats (make-instance 'run-statistics)))
    (labels ((transition (state)
               (if (exhaustedp feeder)
                   (finalize-stats stats)
                   (ecase state
                     (:start (ecase (feed feeder)
                               (:head (start-heads-run stats) (transition :h0))
                               (:tail (start-tails-run stats) (transition :t0))))
                     (:h0 (ecase (feed feeder)
                            (:head (continue-heads-run stats) (transition :hh))
                            (:tail (start-tails-run stats :start-alt-run :ht) (transition :ht))))
                     (:t0 (ecase (feed feeder)
                            (:head (start-heads-run stats :start-alt-run :th) (transition :th))
                            (:tail (continue-tails-run stats) (transition :tt))))
                     (:hh (ecase (feed feeder)
                            (:head (continue-heads-run stats) (transition :hh))
                            (:tail (start-tails-run stats :start-alt-run :ht) (transition :ht))))
                     (:ht (ecase (feed feeder)
                            (:head (start-heads-run stats) (transition :th))
                            (:tail (continue-tails-run stats) (transition :tt))))
                     (:th (ecase (feed feeder)
                            (:head (continue-heads-run stats) (transition :hh))
                            (:tail (start-tails-run stats) (transition :ht))))
                     (:tt (ecase (feed feeder)
                            (:head (start-heads-run stats :start-alt-run :th) (transition :th))
                            (:tail (continue-tails-run stats) (transition :tt)))) ))))
      (transition :start)
      stats)))

(defun trial-do (feeder)
  (assert (not (exhaustedp feeder)))
  (do ((stats (make-instance 'run-statistics))
       (state :start))
      ((exhaustedp feeder) (finalize-stats stats) stats)
    (case state
      (:start (ecase (feed feeder)
                (:head (start-heads-run stats) (setf state :h0))
                (:tail (start-tails-run stats) (setf state :t0))))
      (:h0 (ecase (feed feeder)
             (:head (continue-heads-run stats) (setf state :hh))
             (:tail (start-tails-run stats :start-alt-run :ht) (setf state :ht))))
      (:t0 (ecase (feed feeder)
             (:head (start-heads-run stats :start-alt-run :th) (setf state :th))
             (:tail (continue-tails-run stats) (setf state :tt))))
      (:hh (ecase (feed feeder)
             (:head (continue-heads-run stats) (setf state :hh))
             (:tail (start-tails-run stats :start-alt-run :ht) (setf state :ht))))
      (:ht (ecase (feed feeder)
             (:head (start-heads-run stats) (setf state :th))
             (:tail (continue-tails-run stats) (setf state :tt))))
      (:th (ecase (feed feeder)
             (:head (continue-heads-run stats) (setf state :hh))
             (:tail (start-tails-run stats) (setf state :ht))))
      (:tt (ecase (feed feeder)
             (:head (start-heads-run stats :start-alt-run :th) (setf state :th))
             (:tail (continue-tails-run stats) (setf state :tt)))) )))


(defun trial-tagbody (feeder)
  (assert (not (exhaustedp feeder)))
  (let ((stats (make-instance 'run-statistics)))
    (tagbody
     :start
       (ecase (feed feeder)
         (:head (start-heads-run stats) (go :h0))
         (:tail (start-tails-run stats) (go :t0)))
     :h0
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (continue-heads-run stats) (go :hh))
                  (:tail (start-tails-run stats :start-alt-run :ht) (go :ht)))) )
     :t0
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (start-heads-run stats :start-alt-run :th) (go :th))
                  (:tail (continue-tails-run stats) (go :tt)))) )
     :hh
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (continue-heads-run stats) (go :hh))
                  (:tail (start-tails-run stats :start-alt-run :ht) (go :ht)))) )
     :ht
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (start-heads-run stats) (go :th))
                  (:tail (continue-tails-run stats) (go :tt)))) )
     :th
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (continue-heads-run stats) (go :hh))
                  (:tail (start-tails-run stats) (go :ht)))) )
     :tt
       (cond ((exhaustedp feeder) (go :exit))
             (t (ecase (feed feeder)
                  (:head (start-heads-run stats :start-alt-run :th) (go :th))
                  (:tail (continue-tails-run stats) (go :tt)))) )
     :exit
       (finalize-stats stats))
    stats))

(defmacro with-states (&rest states)
  (let* ((state-labels (mapcar #'(lambda (state) (intern (symbol-name (first state)) :keyword)) states)))
    `(macrolet (,@(mapcar #'(lambda (state label) `(,(first state) () '(go ,label))) states state-labels))
       (tagbody
          ,@(loop for state in states
                  for label in state-labels
                  nconc (list label)
                  nconc (rest state))))))
    
(defun trial-tagbody-macro (feeder)
  (assert (not (exhaustedp feeder)))
  (let ((stats (make-instance 'run-statistics)))
    (with-states (start (ecase (feed feeder)
                          (:head (start-heads-run stats) (h0))
                          (:tail (start-tails-run stats) (t0))))
                 (h0 (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (continue-heads-run stats) (hh))
                                (:tail (start-tails-run stats :start-alt-run :ht) (ht)))) ))
                 (t0 (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (start-heads-run stats :start-alt-run :th) (th))
                                (:tail (continue-tails-run stats) (tt)))) ))
                 (hh (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (continue-heads-run stats) (hh))
                                (:tail (start-tails-run stats :start-alt-run :ht) (ht)))) ))
                 (ht (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (start-heads-run stats) (th))
                                (:tail (continue-tails-run stats) (tt)))) ))
                 (th (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (continue-heads-run stats) (hh))
                                (:tail (start-tails-run stats) (ht)))) ))
                 (tt (cond ((exhaustedp feeder) (exit))
                           (t (ecase (feed feeder)
                                (:head (start-heads-run stats :start-alt-run :th) (th))
                                (:tail (continue-tails-run stats) (tt)))) ))
                 (exit (finalize-stats stats)))
    stats))

;;;
;;;    Expansion of above WITH-STATES macro:
;;;
;; (MACROLET ((START NIL '(GO :START))
;;            (H0 NIL '(GO :H0))
;;            (T0 NIL '(GO :T0))
;;            (HH NIL '(GO :HH))
;;            (HT NIL '(GO :HT))
;;            (TH NIL '(GO :TH))
;;            (TT NIL '(GO :TT))
;;            (EXIT NIL '(GO :EXIT)))
;;   (TAGBODY
;;    :START  (ECASE (FEED FEEDER)
;;              (:HEAD (START-HEADS-RUN STATS) (H0))
;;              (:TAIL (START-TAILS-RUN STATS) (T0)))
;;    :H0     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (CONTINUE-HEADS-RUN STATS) (HH))
;;                     (:TAIL (START-TAILS-RUN STATS :START-ALT-RUN :HT)
;;                      (HT)))))
;;    :T0     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (START-HEADS-RUN STATS :START-ALT-RUN :TH)
;;                      (TH))
;;                     (:TAIL (CONTINUE-TAILS-RUN STATS) (TT)))))
;;    :HH     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (CONTINUE-HEADS-RUN STATS) (HH))
;;                     (:TAIL (START-TAILS-RUN STATS :START-ALT-RUN :HT)
;;                      (HT)))))
;;    :HT     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (START-HEADS-RUN STATS) (TH))
;;                     (:TAIL (CONTINUE-TAILS-RUN STATS) (TT)))))
;;    :TH     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (CONTINUE-HEADS-RUN STATS) (HH))
;;                     (:TAIL (START-TAILS-RUN STATS) (HT)))))
;;    :TT     (COND ((EXHAUSTEDP FEEDER) (EXIT))
;;                  (T
;;                   (ECASE (FEED FEEDER)
;;                     (:HEAD (START-HEADS-RUN STATS :START-ALT-RUN :TH)
;;                      (TH))
;;                     (:TAIL (CONTINUE-TAILS-RUN STATS) (TT)))))
;;    :EXIT   (FINALIZE-STATS STATS)))

;;;
;;;    Almost identical to BRANCH-ON-TOSS!?!?!? Not general enough.
;;;    
(defmacro branch-on-toss2 ((feeder stats) head tail)
  (let ((f (gensym))
        (s (gensym)))
    `(let ((,f ,feeder)
           (,s ,stats))
       (cond ((exhaustedp ,f) (exit))
             (t (let ((stats ,s))
                  (ecase (feed ,f)
                    ,head
                    ,tail)))) )))

;;;
;;;    A little too much magic going on here????
;;;    
(defun trial-tagbody-mega-macro (feeder)
  (assert (not (exhaustedp feeder)))
  (let ((stats (make-instance 'run-statistics)))
    (with-states (start (ecase (feed feeder)
                          (:head (start-heads-run stats) (h0))
                          (:tail (start-tails-run stats) (t0))))
                 (h0 (branch-on-toss2 (feeder stats)
                       (:head (continue-heads-run stats) (hh))
                       (:tail (start-tails-run stats :start-alt-run :ht) (ht))))
                 (t0 (branch-on-toss2 (feeder stats)
                       (:head (start-heads-run stats :start-alt-run :th) (th))
                       (:tail (continue-tails-run stats) (tt))))
                 (hh (branch-on-toss2 (feeder stats)
                       (:head (continue-heads-run stats) (hh))
                       (:tail (start-tails-run stats :start-alt-run :ht) (ht))))
                 (ht (branch-on-toss2 (feeder stats)
                       (:head (start-heads-run stats) (th))
                       (:tail (continue-tails-run stats) (tt))))
                 (th (branch-on-toss2 (feeder stats)
                       (:head (continue-heads-run stats) (hh))
                       (:tail (start-tails-run stats) (ht))))
                 (tt (branch-on-toss2 (feeder stats)
                       (:head (start-heads-run stats :start-alt-run :th) (th))
                       (:tail (continue-tails-run stats) (tt))))
                 (exit (finalize-stats stats)))
    stats))
