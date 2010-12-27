;;; read/write comma-separated values
;;;
;;; Copyright (C) 2003-2010 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `with-timing', `log'
  (require :cllib-log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(export '(csv-print-vector csv-parse-string csv-read-file with-csv csv-names
          class-csv-header class-csv-print *csv-first-line-names* *csv-junk*
          *csv-separator* *csv-whitespace* *csv-progress* *csv-progress-1*))

(defcustom *csv-separator* character #\,
  "The separator in the CSV file, normally the comma.")

(defcustom *csv-first-line-names* (or t nil :default) :default
  "How to treat the first line in WITH-CSV et el.
If this is T (or :DEFAULT and the first line starts with a +COMMENTS+
 character), treat the first line as the vector of column names.
Otherwise, the first line is nothing special.")

(defun csv-print-vector (vec &optional (out *standard-output*))
  "Print a vector as a comma-separated line."
  (declare (type vector vec) (stream out))
  (loop :with len = (length vec) :for val :across vec :and ii :from 1
        :when val :do (write val :stream out :escape nil)
        :unless (= ii len) :do (write-char *csv-separator* out))
  (terpri out))

(defcustom *csv-whitespace* (or null string) +whitespace+
  "The string of characters to trim from the values.")
(defcustom *csv-progress* integer 1000
  "*How often the progress report should be made")
(defcustom *csv-progress-1* integer 10
  "*How often the secondary progress report should be made")

(defcustom *csv-junk* (or symbol integer) :ERROR
  "How to treat lines of wrong length.
When the :JUNK argument is :ERROR, signal an error.
When it is :WARNING, issue a warning and drop the line.
When it is a number, issue at most this many warnings.
When it is :KEEP, keep the line as is.")

(defun csv-trim (whitespace string)
  "Trim the string argument from the whitespace."
  (let* ((clean (string-trim whitespace string)) (len1 (1- (length clean))))
    (when (and (plusp len1) (char= #\" (char clean 0) (char clean len1)))
      (setq clean (subseq clean 1 len1)))
    (if (zerop (length clean)) nil clean)))

(defun csv-parse-string (string &key
                         ((:separator *csv-separator*) *csv-separator*)
                         ((:whitespace *csv-whitespace*) *csv-whitespace*))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count *csv-separator* string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end)
    :for end = (or (position *csv-separator* string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim *csv-whitespace* (subseq string beg end))))
    :finally (return res)))

(defconst +comments+ string "#;" "Characters that start comments.")
(defun uncomment-line (line)
  "Remove the comment prefix from the string."
  (if (find (char line 0) +comments+)
      (string-left-trim +whitespace+ (string-left-trim +comments+ line))
      line))

;;;###autoload
(defun csv-names (file)
  "Read and parse as names the first line in the file."
  (csv-parse-string (uncomment-line (with-open-file (s file) (read-line s)))))

(defun csv-check-vec-len (vec cols fn pos)
  (unless (= cols (length vec))
    (error "~S:~:D: Wrong column count: ~:D instead of ~:D: ~S"
           fn pos (length vec) cols vec)))

(defmacro with-csv ((vec file &key (progress '*csv-progress*)
                         (first-line-names '*csv-first-line-names*)
                         (junk '*csv-junk*)
                         (progress-1 '*csv-progress-1*) limit
                         (out '*standard-output*) columns)
                    &body body)
  "Open FILE and set VEC to successive vectors in it.
Return 3 values:
  number of records (lines) read,
  number of bytes in the file,
  fraction of bytes read
  vector of column names if FIRST-LINE-NAMES is non-NIL
    or if it is :DEFAULT and the first line starts with a +COMMENTS+ character."
  (with-gensyms ("WITH-CSV-" in fn fsize ln len cols lim l1 fln drop ja)
    `(with-timing (:out ,out :count ,len :units "records" :progress ,progress
                   :progress-1 ,progress-1)
       (let* ((,fn ,file) ,fsize ,l1
              (,fln ,first-line-names) (,cols ,columns)
              (,ja ,junk) (,drop 0)
              ,@(when limit `((,lim ,limit))))
         (with-open-file (,in ,fn :direction :input)
           (format ,out "~&Reading `~a' [~:d bytes]..."
                   ,fn (setq ,fsize (file-length ,in)))
           (force-output ,out)
           (when (eq ,fln :default)
             (setq ,fln (find (peek-char nil ,in) +comments+)))
           (when ,fln
             (let ((line1 (read-line ,in)))
               (cond ((zerop (length line1))
                      (cerror "ignore, return NIL for names"
                              "empty first line, names expected"))
                     (t (setq ,l1 (csv-parse-string (uncomment-line line1)))
                        (if ,cols (csv-check-vec-len ,l1 ,cols ,fn 0)
                            (setq ,cols (length ,l1)))))))
           (loop :with ,vec :for ,ln = (read-line ,in nil nil) :while ,ln
             ,@(when limit
                 `(:when (and ,lim (= ,len ,lim))
                   :do (warn "reached the limit of ~:D record~:P ~
                              at ~:D byte~:P (~4F%), aborted~%"
                             ,len (file-position ,in)
                             (/ (file-position ,in) ,fsize 1d-2))
                       (loop-finish)
                   :end))
             :do (setq ,ln (string-trim *csv-whitespace* ,ln))
             :if (or (zerop (length ,ln)) ; empty line
                     (find (char ,ln 0) +comments+) ; comment line
                     (progn (setq ,vec (csv-parse-string ,ln)) (incf ,len)
                            (if ,cols
                                (case ,ja
                                  (:keep nil)
                                  (:error
                                   (csv-check-vec-len ,vec ,cols ,fn ,len))
                                  (t
                                   (handler-case (csv-check-vec-len
                                                  ,vec ,cols ,fn ,len)
                                     (error (c)
                                       (unless (eql ,ja 0)
                                         (warn (princ-to-string c)))
                                       (when (and (integerp ,ja) (plusp ,ja))
                                         (decf ,ja)
                                         (when (zerop ,ja)
                                           (warn "Further warnings omitted")))
                                       t))))
                                (and (setq ,cols (length ,vec)) nil))))
             :do (incf ,drop)
             :else :do ,@body
             (progress (/ (file-position ,in) ,fsize)
                       ;; print <*...*> when we expect to reach limit
                       (if ,(when limit `(and ,lim (> ,len (* ,lim pos))))
                           "*" ""))
             :end
             :finally (format ,out "done [~:d record~:p~@[, ~:d column~:p~]~
                                    ~[~:;,~:* ~:d line~:p dropped~]]"
                              ,len ,cols ,drop)
             :finally (return
                        (values ,len (file-length ,in)
                                (if (zerop ,fsize) 1
                                    (/ (file-position ,in) ,fsize))
                                ,l1))))))))

;;;###autoload
(defun csv-read-file (inf &key ((:junk *csv-junk*) *csv-junk*)
                      ((:first-line-names *csv-first-line-names*)
                       *csv-first-line-names*)
                      ((:separator *csv-separator*) *csv-separator*))
  "Read comma-separated values into a list of vectors."
  (let (len file-size complete names)
    (declare (ignorable complete))
    (values (with-collect (coll)
              (setf (values len file-size complete names)
                    (with-csv (vec inf) (coll vec))))
            len file-size names)))

;;; defstruct i/o
(defun class-csv-header (class &key (out *standard-output*))
  "Print the CSV header for the class to the stream."
  (format out "#~{~A~^,~}~%" (port:class-slot-list class)))

(defun class-csv-print (obj &key (out *standard-output*))
  (format out "~{~A~^,~}~%" (mapcar (lambda (slot) (slot-value obj slot))
                                    (port:class-slot-list obj))))

(provide :cllib-csv)
;;; file csv.lisp ends here
