;;; Read from/Write to files
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `+kwd+'
  (require :symb (translate-logical-pathname "cllib:symb"))
  ;; `get-float-time', `elapsed-1', `mesg'
  (require :log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(file-size-t file-size rename-files save-restore
          count-sexps code-complexity
          write-list-to-stream write-list-to-file
          read-list-from-stream read-list-from-file
          write-to-file read-from-file append-to-file
          read-trim skip-to-line skip-search skip-blanks read-non-blanks))

;;;
;;; {{{ file misc
;;;

(deftype file-size-t () '(unsigned-byte 32))

;;;###autoload
(defun file-size (fn)
  "Return the size of file named FN."
  (declare (values file-size-t))
  (with-open-file (str fn :direction :input) (file-length str)))

;;;###autoload
(defun rename-files (from to)
  "Rename file from wildcard FROM to wildcard TO.
file:/usr/doc/lisp/HyperSpec/Body/fun_translate-pathname.html"
  (dolist (file (directory from))
    (let ((new (translate-pathname file from to)))
      (format t "~a --> ~a~%" file new)
      (rename-file file new))))

;;;
;;; }}}{{{ `code-complexity'
;;;

;;;###autoload
(defun count-sexps (form)
  "Count the SEXPs in the form, quoted forms counting as 1."
  (declare (values index-t))
  (if (or (atom form) (eq 'quote (car form))) 0
      (do ((ff form (cdr ff)) (cc 1 (+ cc (count-sexps (car ff)))))
          ((atom ff) cc)
        (declare (type index-t cc)))))

;;;###autoload
(defun code-complexity (&optional file)
  "Count the sexps in the file."
  (declare (values index-t file-size-t boolean))
  (let ((errorp nil))
    (typecase file
      ((or pathname string)
       (with-open-file (str file :direction :input)
         (values
          (loop :for form = (handler-case (read str nil +eof+)
                              (error (err)
                                (format t " *** problem: ~a~%" err)
                                (setq errorp t)
                                (return cc)))
                :until (eq form +eof+) :finally (return cc)
                :sum (count-sexps form) :into cc :of-type index-t)
          (file-length str)
          errorp)))
      (cons
       (loop :for fl :in file :with cc :of-type index-t = 0
             :and cs :of-type file-size-t = 0 :and err :of-type boolean = nil
             :finally
             (format
              t " *** Total~20t~8:d forms, ~9:d bytes, ~7,2f bytes/form~%"
              tc ts (/ ts tc))
             :do (setf (values cc cs err) (code-complexity fl))
             (format t " * ~a~20t~8:d forms, ~9:d bytes, ~7,2f bytes/form~%"
                     fl cc cs (/ cs cc))
             (setq errorp (or errorp err))
             :sum cc :into tc :of-type index-t
             :sum cs :into ts :of-type file-size-t
             :finally (return (values tc ts errorp))))
      (t (error 'case-error :proc 'code-complexity :args
                (list 'file file 'pathname 'string 'cons))))))

;;;
;;; }}}{{{ Read/Write list
;;;

(defun write-list-to-stream (lst stream &optional (print-function #'prin1))
  "Write the list into the stream, printing the elements one per line.
PRINT-FUNCTION should take (at least) 2 arguments: a record and a stream.
PRIN1 is the default. Returns the length of the list."
  (declare (list lst) (stream stream) (values fixnum)
           (type (function (t stream) t) print-function))
  (do ((ll lst (cdr ll)) (len 0 (1+ len))) ((null ll) len)
    (declare (type index-t len))
    (funcall print-function (car ll) stream)
    (terpri stream)))

(defun write-list-to-file (lst fout &optional (print-function #'prin1))
  "Write the list into the file, printing the elements one per line.
Calls `write-list-to-stream', which see. Returns nil."
  (declare (list lst) (type (function (t stream) t) print-function))
  (let ((bt (get-float-time)) (bt1 (get-float-time nil)))
    (format t "~&Writing `~a'..." fout) (force-output)
    (format t "done [~:d records (~:d bytes) in ~a/~a]~%"
            (with-open-file
                (stout fout :direction :output :if-exists :supersede)
              (write-list-to-stream lst stout print-function))
            (file-size fout) (elapsed-1 bt t) (elapsed-1 bt1 nil))))

(defun read-list-from-stream (stream read-function &optional (eof +eof+)
                              &rest args)
  "Read the input STREAM into the list, each line becomes a list element.
READ-FUNCTION should take a stream and a read-ahead object as its arguments
and return two values: the record read and a read-ahead object or EOF for
end of file. ARGS are just passed to READ-FUNCTION.
EOF is passed to `read' and checked against with `eq'. It defaults to `+eof+'.
Return three values - the list read, its length, and the last element.
`*package*' is bound to KEYWORD, so bare symbols are read as keywords."
  (declare (stream stream) (values list fixnum t)
           (type (function (stream t t) (values t t)) read-function))
  (do* ((*package* +kwd+) (ra (read stream nil eof)) new lst (len 0 (1+ len)))
       ((eq ra eof) (values (nreverse lst) len new))
    (declare (type index-t len))
    (setf (values new ra) (apply read-function stream ra args))
    (push new lst)))

(defun read-list-from-file (fin read-function &optional (eof +eof+)
                            &rest args)
  "Read the file into the list. Just calls `read-list-from-stream'.
EOF defaults to `+eof+'.
  (read-list-from-file FILE-IN READ-FUNCTION EOF &rest ARGS)"
  (declare (type (function (stream t t) (values t t)) read-function)
           (type (or simple-string pathname) fin) (values list fixnum t))
  (let ((bt (get-float-time)) (bt1 (get-float-time nil)))
    (multiple-value-bind (lst len last)
        (with-open-file (stin fin :direction :input :if-does-not-exist nil)
          (unless stin
            (return-from read-list-from-file
              (format t "~& *** Cannot open file `~a' for reading~%" fin)))
          (format t "~&Reading `~a' [~:d bytes]..." fin (file-length stin))
          (force-output)
          (apply #'read-list-from-stream stin read-function eof args))
      (format t "done [~:d records in ~a/~a]~%"
              len (elapsed-1 bt t) (elapsed-1 bt1 nil))
      (values lst len last))))

;;;
;;; }}}{{{ Read/Write object
;;;

(defun write-to-file (obj file &optional (nice t) &rest comments)
  "Write the object to the file, readably.
The optional third argument is passed to `pr'."
  (declare (type (or simple-string pathname) file))
  (format t "Writing `~a'..." file) (force-output)
  (let ((bt (get-float-time)) (bt1 (get-float-time nil)) el el1)
    (with-open-file (str file :direction :output :if-exists :supersede)
      (declare (stream str))
      (format str ";; File: <~a - " file) (current-time str)
      (format str " ~a>~%;; Created by: ~a [~a]
;; *print-circle* = *print-pretty* = ~:[false~;true~]~%~{~a~}~2%"
              (getenv "USER") (lisp-implementation-type)
              (lisp-implementation-version) nice comments)
      (pr obj str nice)
      (format str "~2%;; file written [~a/~a]~%" (setq el (elapsed-1 bt t))
              (setq el1 (elapsed-1 bt1 nil)))
      (format t "done [~:d bytes, ~a/~a]~%" (file-length str) el el1))))

;;;###autoload
(defun read-from-file (file &key (readtable *readtable*) repeat
                       (out *standard-output*))
  "Read an object from a file.
The READTABLE keyword argument (default `*readtable*') specifies
the readtable to use.
The REPEAT keyword argument tells how many objects to read.
If NIL, read once and return the object read;
if a number, read that many times and return a list of objects read,
if T, read until end of file and return a list of objects read."
  (declare (type (or simple-string pathname) file))
  (let ((bt (get-float-time)) (bt1 (get-float-time nil)))
    (prog1 (with-open-file (str file :direction :input)
             (format out "~&Reading `~a' [~:d bytes]..."
                     file (file-length str))
             (force-output)
             (with-standard-io-syntax
               (let ((*readtable* readtable))
                 (cond ((null repeat) (read str))
                       ((numberp repeat)
                        (loop :repeat repeat :collect (read str)))
                       ((loop :for obj = (read str nil +eof+)
                              :while (not (eq obj +eof+))
                              :collect obj))))))
      (format out "done [~a/~a]~%" (elapsed-1 bt t) (elapsed-1 bt1 nil)))))

(defun append-to-file (file fmt &rest fmt-args)
  "Append to the file the formatted output."
  (declare (type (or simple-string pathname) file) (simple-string fmt))
  (let ((*print-pretty* nil) (*print-length* nil))
    (with-open-file (str file :direction :output :if-exists :append
                         :if-does-not-exist :create)
      (declare (stream str)) (apply #'format str fmt fmt-args))))

;;;
;;; }}}{{{ line-based input
;;;

(defsubst read-trim (stream)
  "Read a line from stream and trim it."
  (declare (type stream stream) (values simple-string))
  (string-trim +whitespace+ (read-line stream nil ".")))

(defun skip-to-line (st ln &optional out)
  "Read from stream ST until a line starting with LN.
The optional third argument specifies where the message should go.
By default nothing is printed."
  (declare (stream st) (simple-string ln))
  (mesg :head out " +++ `skip-to-line' --> `~a'~%" ln)
  (do ((len (length ln)) (rr (read-line st) (read-line st)))
      ((and (>= (length rr) len) (string-equal ln rr :end2 len))
       (subseq rr (length ln)))
    (declare (fixnum len) (simple-string rr))))

(defun skip-search (stream string &optional out)
  "Read from STREAM until STRING is found by `search.'"
  (declare (stream stream) (simple-string string)
           (values (or null simple-string)))
  (mesg :head out " +++ `skip-search' --> `~a'~%" string)
  (do ((st (read-line stream nil nil) (read-line stream nil nil)))
      ((or (null st) (search string st :test #'char-equal)) st)
    (declare (type (or null simple-string) st))))

(defun skip-blanks (stream)
  "Read from STREAM first non-blank string is found."
  (declare (type stream stream) (values simple-string))
  (do ((st (read-trim stream) (read-trim stream)))
      ((/= 0 (length st)) st)
    (declare (simple-string st))))

(defun read-non-blanks (stream)
  "Read from STREAM through the first blank string."
  (declare (type stream stream) (values simple-string))
  (do* ((st (read-trim stream) (read-trim stream))
        (res st (concatenate 'string res " " st)))
       ((zerop (length st)) res)
    (declare (simple-string st res))))

;;;
;;; }}}{{{ `save-restore'
;;;

(defun timestamp (&optional (time (get-universal-time)))
  "Return the current time as a string without blanks."
  (declare (integer time) (values simple-string))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d"
            ye mo da ho mi se)))

(defun file-newer (f0 f1)
  "Return T if the first arg is newer than the second.
Non-existent files are assumed to be VERY old."
  (flet ((fwd (ff) (if (probe-file ff) (file-write-date ff) 0)))
    (> (fwd f0) (fwd f1))))

(defsubst file-newest (f0 f1)
  "Returnt the newest of the two existing files."
  (if (> (file-write-date f0) (file-write-date f1)) f0 f1))

(defun latest-file (path &optional (nth 0))
  "Return the latest file matching PATH, which should be wild."
  (declare (fixnum nth))
  (let ((ll (directory path)))
    (when ll
      (if (zerop nth) (reduce #'file-newest ll)
          (nth nth (sort ll #'> :key #'file-write-date))))))

(defun save-restore (what &key (name "~a") pre-save post-read var
                     (voidp #'null) (basedir *datadir*)
                     (readtable *readtable*))
  "Save or read VAR into/from file.
NAME is the name template, and should contain one `~a' format instruction
 if you want the filename to contain the timestamp;
VAR is a symbol whos value is being saved;
VOIDP is a predicate called on value of VAR (default - NULL);
PRE-SAVE and POST-READ are functions called before save and after reading,
 they should be non-destructive and return the value to be written to the
 file and assigned to the variable respectively;
READTABLE is passed to `read-from-file'.
BASEDIR is the pathname relative to which NAME is expanded (`*datadir*')."
  (declare (simple-string name) (symbol var) (pathname basedir)
           (type (or function null) pre-save post-read) (function voidp))
  (let ((val (symbol-value var)))
    (if (or what (funcall voidp val))
        (setf (symbol-value var)
              (let ((vv (read-from-file
                         (latest-file
                          (if (or (stringp what) (pathnamep what)) what
                              (merge-pathnames (format nil name "*") basedir))
                          (if (numberp what) what 0))
                         :readtable readtable)))
                (if post-read (funcall post-read vv) vv)))
        (write-to-file
         (if pre-save (funcall pre-save val) val)
         (merge-pathnames (format nil name (timestamp)) basedir)))))

;;; }}}

(provide :fileio)
;;; file fileio.lisp ends here
