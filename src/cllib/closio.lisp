;;; Read/Write CLOS objects
;;; Load this file and you will be able to print CLOS objects with #[] format,
;;; bind `*readtable*' to `+clos-readtable+' and `read' will read #[]
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `class-slot-list', `class-slot-initargs'
  (require :sys (translate-logical-pathname "clocc:src;port;sys")))

(in-package :cllib)

(export '(+clos-readtable+ make-clos-readtable macroexpand-r))

;;;
;;; {{{ print structure in CMUCL via `print-object'
;;;

#+cmu
(progn
  (defmethod print-object ((xx structure-object) (out stream))
    (kernel:default-structure-print xx out 1))
  (defun print-struct-object (xx out depth)
    (declare (ignore depth))
    (print-object xx out)))

;;;
;;; }}}{{{ read CLOS objects
;;;

(eval-when (compile load eval)  ; CMUCL for `+clos-readtable+'
(defun read-object (st char arg)
  "Read an instance of a CLOS class printed as #[name{ slot val}]"
  (declare (ignore char arg))
  (apply #'make-instance (read-delimited-list #\] st t)))

(defun make-clos-readtable (&optional (rt (copy-readtable)))
  "Return the readtable for reading #[]."
  (set-syntax-from-char #\[ #\( rt)
  (set-syntax-from-char #\] #\) rt)
  (set-macro-character #\] (get-macro-character #\) rt) nil rt)
  (set-dispatch-macro-character #\# #\[ #'read-object rt)
  rt)
)

(defconst +clos-readtable+ readtable (make-clos-readtable)
  "The readtable for reading CLOS objects printed readably with #[].")

;;;
;;; }}}{{{ print CLOS objects readably
;;;

;; without this, Allegro issues a warning about redefining a symbol
;; (`print-object') in a locked package when compiling this file
#+allegro
(eval-when (compile)
  (setf (excl:package-definition-lock (find-package :common-lisp)) nil))

(defmethod print-object ((obj standard-object) (out stream))
  (if *print-readably*
      (loop :with cl = (class-of obj)
            :initially (format out "#[~s" (class-name cl))
            :for slot :in (class-slot-list cl nil)
            :and init :in (class-slot-initargs cl nil)
            :when (slot-boundp obj slot)
            :do (format out " ~s ~s" init (slot-value obj slot))
            :finally (write-string "]" out))
      (call-next-method)))

#+allegro
(eval-when (compile)
  (setf (excl:package-definition-lock (find-package :common-lisp)) t))

;;;
;;; }}}{{{ macroexpand-r
;;;

;;;###autoload
(defun macroexpand-r (form)
  "Recursive macroexpand - unreliable because of `macrolet' &c."
  (if (atom form) form
      (let ((res (macroexpand form)))
        (cons (car res) (mapcar #'macroexpand-r (cdr res))))))

;;; }}}

(provide :closio)
;;; file closio.lisp ends here
