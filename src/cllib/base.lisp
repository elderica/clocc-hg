;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  (require :port-sys (translate-logical-pathname "port:sys")))

(defpackage "CLLIB"
  (:use "COMMON-LISP" "PORT")
  (:nicknames "ORG.CONS.CLOCC/SDS/CLLIB")
  #+cmu (:shadow defstruct)
  (:export "VALUE" "CODE" "DEFSTRUCT"
           "*DATADIR*" "*MAIL-HOST-ADDRESS*" "*USER-MAIL-ADDRESS*"))

(in-package :cllib)

;;;
;;; {{{CMUCL structure hack - make them externalizable
;;;

#+cmu
(defmacro defstruct (name &rest slots)
  `(progn
     (eval-when (compile load eval) (cl:defstruct ,name ,@slots))
     ,(unless (and (consp name) (assoc :type (cdr name)))
       `(defmethod make-load-form ((self ,(if (consp name) (first name) name))
                                   &optional environment)
          (make-load-form-saving-slots self :environment environment)))))

;;;
;;; }}}{{{paths
;;;

(setf (logical-pathname-translations "cllib")
      `(("**;*" ,(logical-pathname "clocc:src;cllib;**;*"))
        ("**;*.*" ,(logical-pathname "clocc:src;cllib;**;*.*"))))

(defun mk-path (default &rest make-pathname-args)
  "This is a helper function for portable creation of pathnames.
If you need to create a pathname under a specific directory, you need
to pass it first to `make-pathname' and then to `merge-pathnames' since
otherwise `*default-pathname-defaults*' will get in the way.
Beware: `default' should not be a relative pathname!"
  (merge-pathnames (apply #'make-pathname :defaults default
                          make-pathname-args)
                   default))

(defcustom *datadir* pathname
  (mk-path (or (user-homedir-pathname) "") :directory '(:relative "data"))
  "The directory where the data file are created by default.")
(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance)))
    (if st (subseq st 0 (position #\Space st)) "localhost"))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (or (getenv "USER") (getenv "USERNAME") "nobody")
               "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

;;;
;;; }}}{{{ generic
;;;

(declaim (ftype (function (t) number) value))
(defgeneric value (xx)
  (:documentation "Get the value of the object.")
  (:method ((xx number)) xx)
  (:method ((xx cons)) (value (cdr xx))))

(declaim (ftype (function (t) symbol) code))
(defgeneric code (xx)
  (:documentation "Get the code of the object.")
  (:method ((xx symbol)) xx))

(provide :cllib-base)
;;; }}} base.lisp ends here
