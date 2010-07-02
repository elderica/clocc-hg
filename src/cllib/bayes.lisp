;;; Naive Bayesian classifier
;;;
;;; C. Elkan "Boosting and Naive Bayesian Learning",
;;;	UCSD, Technical Report CS97-557, 1997
;;; http://www-cse.ucsd.edu/users/elkan/papers/bnb.ps
;;;
;;; Copyright (C) 2010 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(nb-model nb-model-make nb-add-observation nb-model-prune
          nb-predict-classes))

(defstruct nb-model
  (name (port:required-argument)) ; any ID
  (count 0 :type real)       ; sample count; real to accommodate weights
  (class-names (port:required-argument) :type (vector symbol))
  (class-counts (port:required-argument) :type (vector real))
  (features (port:required-argument) :type hash-table)) ; feature -> counts

(defun nb-model-check (model)
  (let ((cc (nb-model-class-counts model)))
    (assert (= (length (nb-model-class-names model)) (length cc)))
    (assert (= (nb-model-count model) (reduce #'+ cc)))))

(defmethod print-object ((model nb-model) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (model out :type t)
        (format out "~S observation~:P:~:D classes:~S features:~:D"
                (nb-model-name model) (nb-model-count model)
                (nb-model-class-names model)
                (hash-table-count (nb-model-features model))))))

(defun nb-model-make (name class-names &key (feature-test 'equal))
  (make-nb-model :name name
                 :class-names class-names
                 :class-counts (make-array (length class-names)
                                           :initial-element 0)
                 :features (make-hash-table :test feature-test)))

(defun nb-class-index (model class)
  "Convert symbolic or numeric class id to in index in CLASS-COUNTS et al."
  (etypecase class
    (integer
     (if (< -1 class (length (nb-model-class-names model))) class
         (error "Class ~:D out of range for ~S" class model)))
    (symbol
     (or (position class (nb-model-class-names model))
         (error "Class ~S is not valid for ~S" class model)))))

(defun nb-add-observation (model class features &key (weight 1))
  "Add an observation with the given features of the given class."
  (assert (plusp weight) (weight) "~S: weight ~S must be positive"
          'nb-add-observation weight)
  (let* ((ci (nb-class-index model class))
         (cc (nb-model-class-counts model)) (nc (length cc)))
    (incf (nb-model-count model) weight)
    (incf (aref cc ci) weight)
    (dolist (feature features)
      (let ((vec (or (gethash feature (nb-model-features model))
                     (setf (gethash feature (nb-model-features model))
                           (make-array nc :initial-element 0)))))
        (incf (aref vec ci) weight)))))

(defun nb-model-prune (model threshold &key (out *standard-output*))
  "Remove features observed fewer than THRESHOLD times."
  (let ((features (nb-model-features model)) (removed 0))
    (when out (format t "~&Pruning ~S to ~:D~%" model threshold))
    (maphash (lambda (feature counts)
               (when (> threshold (reduce #'+ counts))
                 (when out (format out "Removing ~S ~S~%" feature counts))
                 (incf removed)
                 (remhash feature features)))
             features)
    (when out (format t "Pruned ~S (removed ~:D feature~:P)~%" model removed))
    removed))

(defun logodds (this total)
  (and (< 0 this total)
       (- (log this) (log (- total this)))))

(defun nb-predict-classes (model features)
  "Return the vector of logodds for classes.
I.e., P(class) = 1/(1+exp(-logodds))."
  (let ((count (nb-model-count model))
        (nc (length (nb-model-class-names model)))
        (ft (nb-model-features model)))
    (when (zerop count)
      (error "~S(~S): no observations yet" 'nb-predict-classes model))
    (reduce (lambda (vec feature)
              (let ((fc (gethash feature ft)))
                (when fc
                  (let ((tot (reduce #'+ fc)))
                    ;; tot>0 is guaranteed by the assert in nb-add-observation
                    (loop :for i :below nc
                      :for c :across fc
                      :for lo = (logodds c tot)
                      :when lo :do (incf (aref vec i) lo))))
                vec))
            features
            :initial-value (map 'vector ; compute "b = ln b1 - ln b0"
                                (lambda (n) (logodds n count))
                                (nb-model-class-counts model)))))

(provide :bayes)
;;; file bayes.lisp ends here
