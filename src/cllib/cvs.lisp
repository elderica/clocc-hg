;;; CVS (Concurrent Versioning System) interface
;;; http://www.cyclic.com
;;; http://www.sourcegear.com/CVS
;;;
;;; Copyright (C) 1996 by Bruno Haible
;;; Copyright (C) 1998-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `string-beg-with'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; `skip-to-line', `read-list-from-stream'
  (require :fileio (translate-logical-pathname "cllib:fileio"))
  ;; `string->dttm'
  (require :date (translate-logical-pathname "cllib:date"))
  ;; `hash-table->alist'
  (require :miscprint (translate-logical-pathname "cllib:miscprint")))

(in-package :cllib)

(export '(cvs-diff2patch cvs-stat-log))

;;;
;;; CVS diff ---> patch
;;;

;;;###autoload
(defgeneric cvs-diff2patch (in out)
  (:documentation "Convert a CVS diff to a patchable diff.")
  (:method ((in stream) (out stream))
    (do* ((line (read-line in nil nil) (read-line in nil nil))
          (len (length line) (length line)) path base)
         ((null line))
      (declare (simple-string line) (type index-t len))
      (cond ((or (string-beg-with "? " line len) ; skip
                 (string-beg-with "==" line len)
                 (string-beg-with "RCS" line len)
                 (string-beg-with "diff" line len)
                 (string-beg-with "retrieving" line len)))
            ((string-beg-with "Index: " line len)
             (setq path (subseq line 7)
                   base (subseq path (1+ (or (position #\/ path :from-end t)
                                             -1)))))
            ((and (>= len (+ 4 (length base))) (string= line "*** " :end1 4)
                  (or (string= line base :start1 4 :end1 (+ 4 (length base)))
                      (string= line "/tmp/" :start1 4 :end1 9)))
             (format out "*** ~a~a~%" path
                     (subseq line (position #\Tab line))))
            ((and (>= len (+ 4 (length base)))
                  (string= line "--- " :end1 4)
                  (or (string= line base :start1 4 :end1 (+ 4 (length base)))
                      (string= line "/tmp/" :start1 4 :end1 9)))
             (format out "--- ~a~a~%" path
                     (subseq line (position #\Tab line))))
            ((write-line line out)))))
  (:method ((in string) (out t)) (cvs-diff2patch (pathname in) out))
  (:method ((in t) (out string)) (cvs-diff2patch in (pathname out)))
  (:method ((in pathname) (out t))
    (with-open-file (istream in :direction :input)
      (format t "~&~s: reading: ~s [~:d bytes]~%" in)
      (cvs-diff2patch istream out)))
  (:method ((in t) (out pathname))
    (with-open-file (ostream out :direction :output)
      (format t "~&~s: writing: ~s~%" out)
      (cvs-diff2patch in ostream))))

;;;
;;; CVS log stat
;;;

(defstruct (revision #+cmu (:print-function print-struct-object))
  (rev "" :type string)
  (time 0 :type integer)
  (author "" :type string)
  (lines+ 0 :type index-t)
  (lines- 0 :type index-t)
  (log nil :type list))         ; of strings

(defstruct (file #+cmu (:print-function print-struct-object))
  (rcs "" :type string)
  (work "" :type string)
  (head "" :type string)
  (tot-rev 0 :type index-t)
  (revs nil :type list))        ; of revisions

(defparameter *cvs-log-sep-1* (make-string 28 :initial-element #\-))
(defparameter *cvs-log-sep-2* (make-string 77 :initial-element #\=))

(defun cvs-read-change (in ra)
  "Read a CHANGE from a stream.  Suitable for `read-list-from-stream'."
  (declare (stream in) (symbol ra))
  (unless (eq ra :revision)
    (error "~s: read-ahead is `~s' (`~s' expected)"
           'cvs-read-change ra :revision))
  (flet ((extract (line label)
           (let ((pos (search label line :test #'char=)))
             (when pos
               (subseq line (+ (length label) pos)
                       (position #\; line :start pos :test #'char=))))))
    (let* ((rev (read-line in)) (id (read-line in)) fin
           (lines (extract id "lines: "))
           (lines+- (and lines (string-tokens lines))))
      (values
       (make-revision :log (loop :for line = (read-line in)
                                 :do (setq fin (string= line *cvs-log-sep-2*))
                                 :until (or fin (string= line *cvs-log-sep-1*))
                                 :collect line)
                      :rev rev
                      :time (string->dttm (extract id "date: "))
                      :author (extract id "author: ")
                      :lines+ (or (car lines+-) 0)
                      :lines-  (abs (or (cadr lines+-) 0)))
       (if fin +eof+ (read in))))))

(defun cvs-read-file (in ra)
  "Read a FILE froma stream.  Suitable for `read-list-from-stream'."
  (declare (stream in) (symbol ra))
  (unless (eq ra :rcs)
    (error "~s: read-ahead is `~s' (`~s' expected)" 'cvs-read-file ra :rcs))
  (flet ((from-colon (line)
           (subseq line (+ 2 (position #\: line :test #'char=)))))
    (let* ((rcs (from-colon (read-line in)))
           (work (from-colon (read-line in)))
           (head (from-colon (read-line in)))
           (tot-rev-l (skip-to-line in "total revisions:" nil))
           (p0 (position #\; tot-rev-l :test #'char=))
           (p1 (position #\: tot-rev-l :test #'char= :from-end t))
           (tot-rev (parse-integer tot-rev-l :end p0))
           (sel-rev (parse-integer tot-rev-l :start (1+ p1)))
           (revs (progn (skip-to-line in *cvs-log-sep-1* nil)
                        (read-list-from-stream in #'cvs-read-change +eof+))))
      (unless (= tot-rev sel-rev)
        (warn "total revision (~d) != selected revisions (~d)"
              tot-rev sel-rev))
      (unless (= tot-rev (length revs))
        (warn "total revision (~d) != number of revisions (~d)"
              tot-rev (length revs)))
      ;; (format t "~&~s: ~a~%" 'cvs-read-file rcs)
      (values (make-file :revs revs :rcs rcs :work work :head head
                         :tot-rev tot-rev)
              (read in nil +eof+)))))

(defun cvs-read-log (path)
  "Read CVS log, return a list of FILE structures."
  (with-timing (:done t)
    (with-open-file (in path :direction :input)
      (format t "~&~s: ~a [~:d bytes]..." 'cvs-read-log path (file-length in))
      (force-output)
      (loop :while (char= #\? (peek-char nil in)) :do (read-line in))
      (read-list-from-stream in #'cvs-read-file))))

(defsubst rev-lines (rr)
  (declare (type revision rr))
  (+ (revision-lines+ rr) (revision-lines- rr)))

(defsubst file-lines (ff)
  (declare (type file ff))
  (reduce #'+ (file-revs ff) :key #'rev-lines))

;;;
;;; stat by the author
;;;

(eval-when (compile load eval)  ; CMUCL
(defstruct (author #+cmu (:print-function print-struct-object))
  (name "" :type string)
  (owns nil :type list)         ; list of files owned
  (mods nil :type list)         ; list of files modified
  (revs nil :type list))        ; list of revisions
)

(defun author-lines (au)
  (reduce #'+ (author-revs au) :key #'rev-lines))

(defmethod print-object ((au author) (out stream))
  (if *print-readably* (call-next-method)
      (format out "[~a: owns: ~:d mods: ~:d revs: ~:d lines: ~:d]"
              (author-name au) (length (author-owns au))
              (length (author-mods au)) (length (author-revs au))
              (author-lines au))))

;;;###autoload
(defun cvs-stat-log (path)
  "Generate and print some statistics of the CVS repository."
  (let ((fl (cvs-read-log path)) (aht (make-hash-table :test 'equal)) aul)
    (format t "~a: ~:d files, ~:d revisions, ~:d lines changed~%"
            path (length fl) (reduce #'+ fl :key #'file-tot-rev)
            (reduce #'+ fl :key #'file-lines))
    (dolist (ff fl)
      (do ((rr (file-revs ff) (cdr rr)) au re na)
          ((null rr))
        (setq re (car rr) na (revision-author re)
              au (or (gethash na aht)
                     (setf (gethash na aht) (make-author :name na))))
        (push re (author-revs au))
        (pushnew ff (author-mods au) :key #'file-rcs)
        (when (null (cdr rr)) (push ff (author-owns au)))))
    (setq aul (sort (mapcar #'cdr (cdr (hash-table->alist aht))) #'<
                    :key (compose length author-revs)))
    (progn (terpri)
    (format t "name        owns   modified  revisions   lines changed~%")
    (dolist (au aul)
      (format t "~a~10t ~5:d ~10:d ~10:d ~15:d~%"
              (author-name au) (length (author-owns au))
              (length (author-mods au)) (length (author-revs au))
              (author-lines au)))
    (format t "total~10t ~5:d ~10:d ~10:d ~15:d~%"
            (reduce #'+ aul :key (lambda (au) (length (author-owns au))))
            (reduce #'+ aul :key (lambda (au) (length (author-mods au))))
            (reduce #'+ aul :key (lambda (au) (length (author-revs au))))
            (reduce #'+ aul :key #'author-lines)))
    (values aul aht)))

(provide :cvs)
;;; file cvs.lisp ends here
