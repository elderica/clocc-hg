;;; File: <gq.lisp - 1999-01-09 Sat 21:36:50 EST sds@eho.eaglets.com>
;;;
;;; GetQuote
;;; get stock/mutual fund quotes from the Internet
;;; via the WWW using HTTP/1.0, save into a file, plot.
;;;
;;; Copyright (C) 1997, 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id$
;;; $Source$
;;; $Log$
;;; Revision 1.8  1999/01/08 17:12:16  sds
;;; Fixed `get-quotes-sm'.
;;;
;;; Revision 1.7  1998/12/23 18:42:20  sds
;;; Added `*gq-error-stream*'.
;;;
;;; Revision 1.6  1998/07/31 16:53:57  sds
;;; Declared `stream' as a stream in `print-*'.
;;;
;;; Revision 1.5  1998/06/30 13:47:42  sds
;;; Switched to `print-object'.
;;;
;;; Revision 1.4  1998/06/15 21:48:32  sds
;;; Made `gq-fix-date' return the last trading date (skip weekend).
;;;
;;; Revision 1.3  1998/03/10 18:29:20  sds
;;; Replaced `multiple-value-set*' with `(setf (values ))'.
;;;
;;; Revision 1.2  1997/10/31 21:59:41  sds
;;; Added `cite-info' and `strip-html-markup'.
;;;
;;; Revision 1.1  1997/10/15 15:49:26  sds
;;; Initial revision
;;;
;;;
;;; To run regularly (s/a gq.bat)
;;; at 19:00 /every:M,T,W,Th,F "c:\bin\clisp\lisp.exe -M c:\bin\clisp\lispinit.mem -q -i c:/home/sds/lisp/gq -x \"(update-quotes :plot nil)\""

(in-package :cl-user)

(eval-when (load compile eval)
  (let ((dir #+unix "/home/sds/eagle/" #+win32 "c:/home/sds/fx/")
        (*load-verbose* nil) (*load-print* nil))
    (unless (boundp '*require-table*) (load (concatenate 'string dir "base")))
    (sds-require "base") (sds-require "date")
    (sds-require "url") (sds-require "gnuplot"))
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(defcustom *gq-error-stream* (or null stream) nil
  "The error stream for `with-open-url'.")

(defun gq-complete-url (url &rest ticks)
  "Complete URL to get the quotes for TICKS."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (setf (url-path url) (format nil "~a~{~:@(~a~)~^+~}" (url-path url) ticks))
  url)

;;;
;;; {{{ Daily Data
;;;

(eval-when (load compile eval)
(defstruct (daily-data (:conc-name dd-))
  (nav 0.0d0 :type double-float)
  (chg 0.0d0 :type double-float)
  (prc 0.0d0 :type double-float)
  (bid 0.0d0 :type double-float)
  (ask 0.0d0 :type double-float)
  (pre 0.0d0 :type double-float)
  (low 0.0d0 :type double-float)
  (hgh 0.0d0 :type double-float))
)

(defun mk-daily-data (&rest args)
  "Make the DAILY-DATA structure, inferring the missing information."
  (let* ((dd (apply #'make-daily-data args))
         (nav (dd-nav dd)) (chg (dd-chg dd)))
    (unless (zerop chg)
      (when (zerop (dd-prc dd)) (setf (dd-prc dd) (* 100.0d0 (/ chg nav))))
      (when (zerop (dd-pre dd)) (setf (dd-pre dd) (- nav chg))))
    dd))

(defmethod print-object ((dd daily-data) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "price:~15t~7,2f~35tbid:~45t~7,2f
previous:~15t~7,2f~35task:~45t~7,2f
change:~15t~7,2f~35thigh:~45t~7,2f
%:~15t~7,2f~35tlow:~45t~7,2f~%"
              (dd-nav dd) (dd-bid dd) (dd-pre dd) (dd-ask dd)
              (dd-chg dd) (dd-hgh dd) (dd-prc dd) (dd-low dd))))

(defun gq-guess-date ()
  "Guess the date: the last date when the quote could be available."
  (declare (values date))
  (multiple-value-bind (se mi ho da mo ye wd) (get-decoded-time)
    (declare (ignore se mi) (fixnum ho da mo ye wd))
    (let ((td (mk-date :ye ye :mo mo :da da)))
      (declare (type date td))
      (if (< wd 5)              ; weekday
          (if (< ho 17) (yesterday td) td)
          (yesterday td (if (= wd 5) 1 2))))))

(defun get-quotes-apl (url &rest ticks)
  "Get the data from the APL WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks) :timeout 600
                       :err *gq-error-stream* :rt *html-readtable*)
    (do (zz res (ts (make-text-stream :sock sock)))
        ((or (null ticks) (eq (setq zz (next-token ts)) +eof+))
         (cons (gq-guess-date) (nreverse res)))
      (mesg :logv t " -> ~a~%" zz)
      (when (eq (car ticks) zz)
        (mesg :log t "Found: ~a~%" (pop ticks))
        (push (mk-daily-data
               :nav (next-token ts) :chg (next-token ts) :prc (next-token ts)
               :bid (next-token ts :type 'float :dflt 0.0d0 :num 3)
               :ask (next-token ts :type 'float :dflt 0.0d0)
               :pre (next-token ts :type 'float :dflt 0.0d0 :num 3)
               :low (next-token ts :type 'float :dflt 0.0d0 :num 2)
               :hgh (next-token ts :type 'float :dflt 0.0d0)) res)
        ;; (setq dt (infer-date (next-token ts 3) (next-token ts)))
        ))))

(defun get-quotes-pf (url &rest ticks)
  "Get the data from the PathFinder WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks) :timeout 600
                       :err *gq-error-stream* :rt *html-readtable*)
    (let ((dt +bad-date+) (ts (make-text-stream :sock sock)))
      (declare (type date dt) (type text-stream ts))
      (do ((t0 (next-token ts) (next-token ts)) (t1 nil t0))
          ((and (eq t1 'latest) (eq t0 'prices))
           (setq dt (infer-date (next-token ts) (next-token ts)))))
      (do ((zz (next-token ts) (next-token ts)) res)
          ((null ticks) (cons dt (nreverse res)))
        (when (eq (car ticks) zz)
          (pop ticks)
          (push (mk-daily-data :nav (next-number ts :num 6)) res))))))

(defun get-quotes-sm (url &rest ticks)
  "Get the data from the StockMaster WWW server."
  (do ((ti ticks (cdr ti)) (ds nil nil) (vs nil nil) hh ar res ts dt
       (gd (gq-guess-date)) (*ts-kill* '(#\%)))
      ((null ti)
       (setq dt (caar (last (car hh))))
       (unless (date= dt gd)
         (format t "Warning: implied date (~a) differs from given date (~a)~%"
                 gd dt))
       (values (cons dt (nreverse res))
               (apply #'mapcar
                      (lambda (&rest cns)
                        (assert
                         (every (lambda (cn) (date= (caar cns) (car cn)))
                                (cdr cns)) ()
                         "Date mismatch: ~a~%" cns)
                        (make-hist :date (caar cns) :navs (mapcar #'cdr cns)))
                      (nreverse hh))
               (nreverse ar)))
    (with-open-url (sock (gq-complete-url url (car ti)) :timeout 600
                         :rt *html-readtable* :err *gq-error-stream*)
      (do ((st (read-line sock) (read-line sock))
           (sy (format nil "(~a)" (car ti))))
          ((string-equal st sy :end1 (min (length st) (length sy)))
           (mesg :log *gq-error-stream* "found `~a'~%" st))
        (declare (simple-string st sy)))
      (setq ts (make-text-stream :sock sock))
      (push (mk-daily-data :nav (next-number ts) :chg (next-number ts)
                           :prc (next-number ts))
            res)
      (mesg :log *gq-error-stream* " ~a:~%~a~%" (car res))
      (push (list (next-number ts :num 5)
                  (next-token ts :type 'number :dflt 0.0)
                  (next-token ts :type 'number :dflt 0.0)
                  (next-token ts :type 'number :dflt 0.0)) ar)
      (push (mapcar
             #'cons
             (dotimes (ii 10 (nreverse ds))
               (push (infer-date (next-token ts) (next-token ts)) ds))
             (dotimes (ii 10 (nreverse vs))
               (push (next-number ts) vs)))
            hh))))

(defcustom *get-quote-url-list* list
  (list (list (make-url :prot :http :host "qs.secapl.com"
                        :path "/cgi-bin/qs?ticks=")
              'get-quotes-apl "APL")
        (list (make-url :prot :http :host "www.stockmaster.com"
                        :path "/wc/form/P1?template=sm/chart&Symbol=")
              'get-quotes-sm "StockMaster")
        (list (make-url :prot :http :host "quote.pathfinder.com"
                        :path "/money/quote/qc?symbols=")
              'get-quotes-pf "PathFinder")
        (list (make-url :prot :http :host "www.stockmaster.com"
                        :path "/cgi-bin/graph?sym=")
              'get-quotes-sm-old "old StockMaster"))
  "*The list of known URL templates for getting quotes.")

(defun get-quotes (server &rest ticks)
  "Get the quotes from one of `*get-quote-url-list*'.
The first arg, SERVER, when non-nil, specifies which server to use."
  (if server
      (let ((qq (if (numberp server) (nth server *get-quote-url-list*)
                    (find server *get-quote-url-list* :key #'third
                          :test (lambda (se na)
                                  (search se na :test #'char-equal))))))
        (assert qq (server) "Unknown server `~a'.~%" server)
        (multiple-value-call #'values (third qq)
                             (apply (second qq) (first qq) ticks)))
      (do ((ql *get-quote-url-list* (cdr ql)) res name)
          ((or (endp ql) (car res)) (values-list (cons name res)))
        (format t "~&Trying ~a..." (setq name (caddar ql)))
        (force-output)
        (setq res (multiple-value-list
                   (ignore-errors (apply (cadar ql) (caar ql) ticks))))
        (format t "~:[failed...~;success!~]~%" (car res)))))

(defun infer-date (mon day)
  "Guess what the date is.
Return the most recent date with these month and day."
  (multiple-value-bind (se mm ho da mo ye) (get-decoded-time)
    (declare (ignore se ho da) (fixnum mo ye mm))
    (setq mm (infer-month mon))
    (mk-date :da day :mo mm :ye (if (<= mm mo) ye (1- ye)))))

;;;
;;; }}}{{{ Holdings
;;;

(defcustom *hist-data-file* pathname
  (merge-pathnames "text/invest.txt" (user-homedir-pathname))
  "*The file with the historical data.")
(defcustom *hist-data-file-header* simple-string
  ";*GetQuote portfolio*
; file format is as follows:
; - lines starting with `;' are ignored
; - empty lines are ignored
; - all non-ignored lines until the separator token, `~',
; are expected to contain ticker info in the following format
;     <ticker> <number of shares> <buying price> \"<comment (fund name)>\"
; - all the lines after that are history of this portfolio, in the format
;     <date> <total value> [<price>]*
" "The header of the data file.")
(defcustom *hist-data-file-sep* symbol '~
  "*The separator between the portfolio and its history")
(defcustom *holdings* list nil
  "The holdings, to be read from `*hist-data-file*'.")
(defcustom *history* list nil
  "The history, to be read from `*hist-data-file*'.")

(eval-when (load compile eval)
(defstruct (pfl)
  (tick nil :type symbol)
  (nums 0.0d0 :type double-float)
  (bprc 0.0d0 :type double-float)
  (name "" :type string))
)

(defun read-pfl (stream ra)
  "Read a PFL from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (values (make-pfl :tick ra :nums (read stream) :bprc (read stream)
                    :name (read stream)) (read stream nil +eof+)))

(defmethod print-object ((pfl pfl) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "~:@(~a~) ~8,3f ~7,2f ~a" (pfl-tick pfl) (pfl-nums pfl)
              (pfl-bprc pfl) (pfl-name pfl))))

(defsubst find-pfl (sy)
  "Find the holding corresponding to the symbol."
  (declare (symbol sy)) (find sy *holdings* :key #'pfl-tick :test #'eq))

(eval-when (load compile eval)
(defstruct (hist)
  (date +bad-date+ :type date)
  (totl 0.0d0 :type double-float)
  (navs nil :type list))
)

(defmethod value ((hs hist)) (hist-totl hs))
(defmethod date ((hs hist)) (hist-date hs))

(defun read-hist (stream ra)
  "Read a HIST from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (do ((hist (make-hist :date (date ra) :totl (read stream))) rr
       (vl (read stream) (read stream nil +eof+)))
      ((not (numberp vl)) (setf (hist-navs hist) (nreverse rr))
       (values hist vl))
    (push vl rr)))

(defmethod print-object ((hist hist) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "~a ~15,5f~{ ~7,2f~}" (hist-date hist)
              (hist-totl hist) (hist-navs hist))))

(defun hist-totl-comp (hold navs)
  "Compute the correct total from the hist records and holdings list."
  (declare (list hold navs))
  (loop for ho of-type pfl in hold for vl of-type double-float in navs
        sum (* vl (pfl-nums ho)) double-float))

(defun read-data-file (file)
  "Return 2 values: the list of holdings and the history list."
  (with-open-file (fl file :direction :input :if-does-not-exist nil)
    (unless fl
      (return-from read-data-file
        (format t " *** Cannot open file `~a' for reading~%" file)))
    (values (read-list-from-stream fl #'read-pfl *hist-data-file-sep*)
            (read-list-from-stream fl #'read-hist))))

(defun save-data (file hold hist)
  "Save the history into the file.
If the file doesn't exist, it is created. If it exists,
only the data after `*hist-data-file-sep*' is changed."
  (declare (list hold hist))
  (with-open-file (outst file :direction :io :if-exists :overwrite)
    (cond ((read outst nil nil)         ; file existed
           (format t "File `~a' exists.  Appending...~%" file)
           (do (zz)
               ((or (eq zz +eof+) (eq zz *hist-data-file-sep*))
                (when (eq zz +eof+)
                  (error "File `~a' is corrupted: `~a' not found.~%"
                         file *hist-data-file-sep*)))
             (setq zz (read outst nil +eof+)))
           (terpri outst)
           (write-list-to-stream hist outst))
          (t                            ; new file
           (format t "File `~a' does not exists. Creating...~%" file)
           (princ *hist-data-file-header* outst)
           (terpri outst)
           (write-list-to-stream hold outst)
           (terpri outst) (princ *hist-data-file-sep* outst) (terpri outst)
           (write-list-to-stream hist outst))))
  (format t "~d record~:p about ~r holding~:p written.~%"
          (length hist) (length hold)))

(defun gq-fix-hist (hold hist hhh)
  "Fix the values in the history. Return T if something was fixed."
  (declare (list hist hold hhh))
  (let ((fixed nil))
    (when (listp hhh)
      (setq hhh
            (map-sorted
             'list
             (lambda (hi hs)
               (when hs
                 (cond (hi
                        (unless (equal (hist-navs hi) (hist-navs hs))
                          (format t "Incorrect NAVs on ~a:
~5tOriginal:~15t~{~7,2f~}~%~5tActual:~15t~{~7,2f~}~%"
                                  (hist-date hi) (hist-navs hi) (hist-navs hs))
                          (setf (hist-navs hi) (hist-navs hs)
                                (hist-totl hi)
                                (hist-totl-comp hold (hist-navs hs))
                                fixed t)))
                       (t
                        (setf (hist-totl hs)
                              (hist-totl-comp hold (hist-navs hs)))
                        (format t "Missing record added: [~a].~%" hs)
                        (setq hi hs fixed t))))
               hi)
             #'date< hist hhh :ckey #'hist-date))
      ;; modify hist by side effect
      (setf (car hist) (car hhh) (cdr hist) (cdr hhh)))
    (let (ntot)
      (dolist (hr hist)
        (setq ntot (hist-totl-comp hold (hist-navs hr)))
        (unless (approx= ntot (hist-totl hr) 0.01d0 0.00001d0)
          (format t "Incorrect total (~a): ~15,5f; correct: ~15,5f~%"
                  (hist-date hr) (hist-totl hr) ntot)
          (setf (hist-totl hr) ntot fixed t))))
    fixed))

(defun pr-res (str pref v0 v1 per apy v2)
  (format str "~aP/L: total:~15t~2,7/comma/ - ~2,7/comma/ = ~2,5/comma/~
~55t[~7,3f% APY:~8,3f%]
        today:~15t~2,7/comma/ - ~2,7/comma/ = ~2,5/comma/~55t[~7,3f%]~2%"
          pref v0 v1 (- v0 v1) per apy v0 v2 (- v0 v2) (percent-change v2 v0)))

(defun process-results (hold hist srv res yea &optional (str t))
  "Process the results, update the history.
Return non-nil if the history was updated and needs to be saved.
RES is a list of SERVER, DATE and DAILY-RECS.
YEA is the list of 1, 3, 5, and 10-year returns."
  (declare (list hold hist res))
  (unless res
    (return-from process-results (format t "no results - timed out?~%")))
  (let* ((lh (last hist 2)) cv ov pv (ctot 0.0d0) (otot 0.0d0) (ptot 0.0d0)
         (begd (hist-date (car hist))) pers apy db
         (nnavs (mapcar #'dd-nav (cdr res))) (*print-case* :upcase)
         (pnavs (mapcar #'dd-pre (cdr res))))
    (declare (double-float ctot otot ptot) (type date begd))
    (format str "~2%~72:@<~a results [~a]:~>
~72:@<<[~a -- ~a (~:d days)]>~>~2%"
            srv (current-time nil) begd (car res)
            (setq db (days-between begd (car res))))
    (apply #'mapc
           (lambda (hl dd &optional ye)
             (format str "Fund: ~a [~a]~48t~7,3f * ~6,2f = ~2,6:/comma/~%"
                    (pfl-tick hl) (pfl-name hl) (pfl-nums hl) (dd-nav dd)
                    (setq cv (* (pfl-nums hl) (dd-nav dd))))
             (when ye
               (format str "~10t[1ye:~6,2f%; 3ye:~6,2f%; 5ye:~6,2f%; ~
10ye:~6,2f%]~%" (first ye) (second ye) (third ye) (fourth ye)))
             (prin1 dd str)
             (setq ov (* (pfl-nums hl) (pfl-bprc hl))
                   pv (* (pfl-nums hl)
                         (cond ((not (zerop (dd-pre dd))) (dd-pre dd))
                               ((not (zerop (dd-chg dd)))
                                (- (dd-nav dd) (dd-chg dd)))
                               (t (nth (position (pfl-tick hl) hold :test #'eq
                                                 :key #'pfl-tick)
                                       (hist-navs (cadr lh)))))))
             (incf ctot cv) (incf otot ov) (incf ptot pv)
             (setf (values pers apy) (percent-change ov cv db))
             (pr-res str "   " cv ov pers apy pv))
           hold (rest res) (if yea (list yea) nil))
    (setf (values pers apy) (percent-change otot ctot db))
    (pr-res str " * " ctot otot pers apy ptot)
    (cond ((date> (car res) (hist-date (cadr lh)))
           (unless (or ;; who cares whether this is the next day or not?!
                    ;; (date= (tomorrow (hist-date (cadr lh))) (car res))
                    (some #'zerop pnavs)
                    (equal (hist-navs (cadr lh)) pnavs)
                    (and (equal (hist-navs (car lh)) pnavs)
                         (equal (hist-navs (cadr lh)) nnavs)))
             (format str "A discrepancy found:~% last record:~15t~{~7,2f~}~% ~
previous day:~15t~{~7,2f~}~%Added an extra record~%~5t~{~a~}~%"
                     (hist-navs (cadr lh)) pnavs
                     (setf (cddr lh)
                           (list (make-hist :date (yesterday (car res))
                                            :navs pnavs :totl
                                            (hist-totl-comp hold pnavs)))))
             (setq lh (cdr lh)))
           (cond ((equal nnavs (hist-navs (cadr lh)))
                  (format str "Same NAVs as on ~a, no record added.~%"
                          (hist-date (cadr lh))))
                 (t (format str "A record for date ~a added:~%~5t~{~a~}~%"
                            (car res)
                            (setf (cddr lh)
                                  (list (make-hist
                                         :date (car res) :navs nnavs
                                         :totl (hist-totl-comp hold
                                                               nnavs)))))
                    t)))
          (t (format str "Old news [~a].~%" (car res))))))

(defun plot-portfolio (hold hist plot)
  "Plot the portfolio."
  (declare (list hold hist))
  (do ((ii 0 (1+ ii)) (hl hold (cdr hl)) res)
      ((null hl)
       (plot-dated-lists
        (hist-date (car hist)) (hist-date (car (last hist)))
        (cons (make-dated-list :ll hist :date 'hist-date :name
                               "Total Value" :misc 'hist-totl)
              (nreverse res)) :rel t :slot 'misc :grid t
        :title "Portfolio History" :data-style "linespoints"
        :ylabel "Relative Value" :plot plot :legend "top left box"))
    (declare (fixnum ii))
    (push (make-dated-list :ll hist :date 'hist-date :name (pfl-name (car hl))
                           :code (pfl-tick (car hl)) :misc
                           (let ((ii ii))
                             (declare (fixnum ii))
                             (lambda (hs) (nth ii (hist-navs hs))))) res)))

;;;
;;; }}}{{{ Run the thing
;;;

#+win32
(defcustom *gq-log* pathname
  (merge-pathnames "text/getquote.log" (user-homedir-pathname))
  "The log file for the batch processing.")

(defun update-quotes (&key (plot nil plotp) server)
  "Read the history. Update quotes. Plot (optionally),
if PLOT is non-nil, or if it is not given but there was new data.
If PLOT is T, just plot, do not try to update quotes."
  ;; interactive (ia) = works with CLISP and ACL, but not with CMUCL
  (let ((ia (eq *standard-output* *standard-input*))
        (*print-log* (mk-arr 'symbol nil 0)))
    #+win32 (unless ia
              (setq *hist-data-file* (pathname "c:/home/sds/text/invest.txt")))
    (setf (values *holdings* *history*) (read-data-file *hist-data-file*))
    (unless (eq plot t)
      (multiple-value-bind (srv res hhh yea str fixed new)
          (apply #'get-quotes server (mapcar #'pfl-tick *holdings*))
        (setq str (or ia #-win32 t
                      #+win32 (open *gq-log* :direction :output :if-exists
                                    :append :if-does-not-exist :create))
              fixed (gq-fix-hist *holdings* *history* hhh)
              new (process-results *holdings* *history* srv res yea str))
        (top-bottom-fl *history* :val #'hist-totl :label #'hist-date)
        (when (or fixed new)
          (save-data *hist-data-file* *holdings* *history*)
          (unless plotp (setq plot t)))
        (unless (eq t str) (close str))))
    (when plot (plot-portfolio *holdings* *history* (if ia :plot :wait)))))

(provide "gq")
;;; }}} gq.lisp ends here
