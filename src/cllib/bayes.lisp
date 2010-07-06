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
          feature-power feature-weight
          nb-predict-classes logodds-to-prob best-class))

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

(defun feature-weight (counts)
  "The number of times the feature appears in the training sample."
  (reduce #'+ counts))

(defun feature-power (counts)
  "An indicator of the predictive power of the feature."
  (* (feature-weight counts)
     (- (log (length counts) 2) ; max possible entropy
        (entropy-distribution counts)))) ; actual entropy

(defun nb-model-prune (model feature-quality threshold
                       &key (out *standard-output*))
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
  ;; should be using ieee infinities and nans...
  (cond ((zerop this) '-infinity)
        ((= total this) '+infinity)
        (t (- (log this) (log (- total this))))))

(defgeneric logodds-to-prob (lo)
  (:method ((lo sequence))
    (map (type-of lo) #'logodds-to-prob lo))
  (:method ((lo (eql '-infinity))) 0)
  (:method ((lo (eql '+infinity))) 1)
  (:method ((lo (eql 'nan))) 'nan)
  (:method ((lo number)) (/ (1+ (exp (- lo))))))

(defun logodds+ (lo0 lo1)
  (case lo0
    ((nan) 'nan)
    ((+infinity) (case lo1 ((nan -infinity) 'nan) (t '+infinity)))
    ((-infinity) (case lo1 ((nan +infinity) 'nan) (t '-infinity)))
    (t (etypecase lo1 (symbol lo1) (number (+ lo0 lo1))))))

(defun nb-predict-classes (model features)
  "Return the vector of logodds for classes.
I.e., P(class) = 1/(1+exp(-logodds))."
  (let ((count (nb-model-count model))
        (nc (length (nb-model-class-names model)))
        (ft (nb-model-features model)))
    (unless (plusp count)
      (error "~S(~S): no observations yet" 'nb-predict-classes model))
    (reduce (lambda (vec feature)
              (let ((fc (gethash feature ft)))
                (when fc
                  (let ((tot (reduce #'+ fc)))
                    ;; tot>0 is guaranteed by the assert in nb-add-observation
                    (loop :for i :below nc :for c :across fc
                      :do (setf (aref vec i)
                                (logodds+ (aref vec i) (logodds c tot))))))
                vec))
            features
            :initial-value (map 'vector ; compute "b = ln b1 - ln b0"
                                (lambda (n) (logodds n count))
                                (nb-model-class-counts model)))))

(defun best-class (logodds)
  "Return the best class index or NIL if all logodds is -INFINITY or NAN
or T if multiple best classes."
  (loop :with ret = nil :with best = nil
    :for lo :across logodds :for index :upfrom 0 :do
    (case lo
      ((-infinity nan))
      ((+infinity) (if (eq best '+infinity) (return T)
                       (setq best '+infinity ret index)))
      (t (unless (eq best '+infinity)
           (when (= best lo) (return T))
           (when (< best lo) (setq best lo ret index)))))
    :finally (return (values ret best))))

(provide :bayes)
;;; file bayes.lisp ends here
