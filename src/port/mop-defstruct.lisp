;;; Check whether MOP supports DEFSTRUCT objects
;;;
;;; Copyright (C) 2017 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mop (translate-logical-pathname "clocc:src;port;mop")))

(in-package :port)

(eval-when (:compile-toplevel :execute)
  (defstruct s a))

(unless (ignore-errors
          (slot-definition-initargs (car (class-direct-slots (find-class 's)))))
  (pushnew :no-defstruct-mop *features*))

(provide :mop-defstruct)
;;; file mop-defstruct.lisp ends here
