;;; logging and progress reporting
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-type', `dfloat'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `linear'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `pr-secs'
  (require :cllib-tilsla (translate-logical-pathname "cllib:tilsla")))

(in-package :cllib)

(export '(get-int-time elapsed time-diff with-timing eta
          *print-log* mesg list-format))

;;;
;;;
;;;

(defun list-format (item-fmt)
  "Return the format string for list printing, printing the item as ITEM-FMT.
Taken from CLtL2 p602."
  (format nil "~~#[ none~~; ~a~~; ~a and ~a~~:;~~@{~~#[~~; and~~] ~a~~^,~~}~~]"
          item-fmt item-fmt item-fmt item-fmt))

;;;
;;; {{{ progress reporting
;;;

(declaim (ftype (function (&optional t) (values (integer 0))) get-int-time))
(defun get-int-time (&optional (run t))
  "Return the run (or real) time counter, as a double float."
  (if run (get-internal-run-time) (get-internal-real-time)))

(defun time-diff (end beg)
  "Compute the time (in seconds) between the two given internal timestamps."
  (dfloat (/ (- end beg) internal-time-units-per-second)))

(defun elapsed (bt run &optional fmt)
  "Return the time in seconds elapsed since BT,
previously set using `get-int-time'.
If FMT is non-NIL, return the corresponding string too."
  (declare (type (integer 0) bt))
  (let ((nn (time-diff (get-int-time run) bt)))
    (declare (double-float nn))
    (if fmt (values nn (format nil "~/pr-secs/" nn)) nn)))

;;;
;;; }}}{{{ logging
;;;

(defcustom *print-log* simple-vector
  '#(:log :logv :date :plot :head :res :opt :err :test :xml)
  "The list of message types which are being printed.")

(defun print-log-p (type)
  "Check whether this message TYPE is being logged."
  (or (eq type t) (find type *print-log* :test #'eq)))

(defmacro mesg (type str &rest args)
  "Call format -- conditionally.
This has to be a macro to avoid needless evaluating the args."
  (with-gensyms ("MESG-" out typ)
    `(let ((,out ,str) (,typ ,type))
      (declare (type (or stream (member nil t)) ,out))
      (when (and ,out (print-log-p ,typ))
        (format ,out ,@args)
        (force-output (if (eq t ,out) *standard-output* ,out))))))

(defmacro with-timing ((&key (terpri t) (done nil) (run t) (real t)
                             (count nil) (units "bytes")
                             (type t) (out '*standard-output*))
                       &body body)
  "Evaluate the body, then print the timing.
Within the body you can call a local function ETA of one argument -
the current relative position which returns 2 values:
the expected remaining run and real times."
  (with-gensyms ("TIMING-" bt bt1 %out el last-pos last-time last-time1)
    `(let* ((,bt (get-int-time)) (,bt1 (get-int-time nil)) ,el
            (,%out (and (print-log-p ,type) ,out))
            (,last-time ,bt) (,last-time1 ,bt1) (,last-pos 0)
            ,@(when count `((,count 0))))
      (unwind-protect
           (flet ((eta (pos) ; pos is the current relative position
                    (if (zerop pos) (values 0 0)
                      (let* ((now (get-int-time)) (now1 (get-int-time nil))
                             (lt ,last-time) (lt1 ,last-time1)
                             (eta (linear 0 ,bt pos now 1))
                             (eta1 (linear 0 ,bt1 pos now1 1)))
                        (setq ,last-time now ,last-time1 now1)
                        (if (= ,last-pos pos)
                          (values (time-diff eta now) (time-diff eta1 now1))
                          (let ((lp ,last-pos))
                            (setq ,last-pos pos)
                            (values
                             (time-diff
                              (/ (+ eta (linear lp lt pos now 1)) 2) now)
                             (time-diff
                              (/ (+ eta1 (linear lp lt1 pos now1 1)) 2) now1)
                             )))))))
             (declare (ignorable (function eta)))
             ,@body)
        (when ,%out
          (when ,done (princ "done" ,%out))
          (when (or ,run ',count) (setq ,el (elapsed ,bt t)))
          (when ,run (format ,%out " [run: ~/pr-secs/]" ,el))
          (when ,real (format ,%out " [real: ~/pr-secs/]" (elapsed ,bt1 nil)))
          ,(when count
             `(unless (zerop ,el)
                (format ,%out " [~5f ~a per second]" (/ ,count ,el) ,units)))
          (when ,terpri (terpri ,%out)))))))

;;; }}}

(provide :cllib-log)
;;; file log.lisp ends here
