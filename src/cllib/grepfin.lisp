;;; Grep Financial Data
;;;
;;; Copyright (C) 2010 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :cllib-csv (translate-logical-pathname "cllib:csv")))

(in-package :cllib)

(export '(*data-dir* *fund-dir* *stock-file* *funds-db*
          query-funds query-stocks))

(defcustom *data-dir* pathname (merge-pathnames "data/" (user-homedir-pathname))
  "The location of all financial data.")
(defcustom *fund-dir* pathname (merge-pathnames "funds/" *data-dir*)
  "The location of the tree of funds data from MFFAIS.")

(defpackage "FIN" (:use))

;; export from http://www.mffais.com/
(defcsv fund (:package "FIN")
  (("Fullname" fullname string)
   ("Symbol" symbol symbol)
   ("Country" country symbol)
   ("Industry" industry symbol)
   ("Shares" shares integer)
   ("Activity" activity symbol)
   ("Shares Change" shares-change integer)
   ("Shares Pct Change" shares-change-% float)
   ("Previous Shares" previous-shares integer)
   ("Today Pct. Portfolio" today-portfolio-% float)
   ("As-Of/On Price" as-of/on-price float)
   ("As-Of/On Value" as-of/on-value float)
   ("Today Price" today-price float)
   ("Today Value" today-value float)
   ("Amount Of Chg." amount-of-chg float)
   ("Return" return float)
   ("Result Of Changed Shares Only" result-of-changed-shares-only float)
   ("Lifetime Buy Total" lifetime-buy-total float)
   ("Lifetime Buy Avg. Price" lifetime-buy-avg-price float)
   ("Lifetime Sell Total " lifetime-sell-total float)
   ("Lifetime Sell Avg. Price" lifetime-sell-avg-price float)
   ("Lifetime Avg. Return Pct" lifetime-avg-return-% float)))

(defvar *funds* (make-hash-table :test 'equalp))

(defun read-fund (file)
  (setf (gethash (pathname-name file) *funds*) (csv-read 'fund file)))
(defun read-funds (&key (dir *fund-dir*))
  (mapc #'read-fund (directory (merge-pathnames "**/*.csv" dir))))
(defvar *funds-db* (merge-pathnames "funds.sexp" *data-dir*))
(defun save-funds (&key (file *funds-db*))
  (write-to-file *funds* file))
(defun load-funds (&key (file *funds-db*))
  (setq *funds* (read-from-file file)))
(defun ensure-funds (&key (dir *fund-dir*))
  ;; make sure *funds-db* is up to date
  (let* ((fund-files (directory (merge-pathnames "**/*.csv" dir)
                                #+clisp :full #+clisp t))
         (latest (loop :for ff :in fund-files :maximize
                   #+clisp (apply #'encode-universal-time (third ff))
                   #-clisp (file-write-date ff)))
         (db-fwd (file-write-date *funds-db*)))
    (cond ((> latest db-fwd)
           (format t "~&Latest fund file(~A) is newer than the fund DB file(~A), rebuilding...~%"
                   (dttm->string latest :format :short)
                   (dttm->string db-fwd :format :short))
           (clrhash *funds*)
           (dolist (ff fund-files)
             (read-fund #+clisp (car ff) #-clisp ff))
           (save-funds))
          (t (load-funds)))
    (values)))                  ; avoid lengthy output of the return value

;; export from http://finviz.com/
(defcsv stock (:package "FIN")
  (("No." no integer)
   ("Ticker" ticker symbol)
   ("Company" company string)
   ("Sector" sector symbol)
   ("Industry" industry symbol)
   ("Country" country symbol)
   ("Market Cap" market-cap float)
   ("P/E" p/e float)
   ("Price" price float)
   ("Change" change float%)
   ("Volume" volume integer)))

(defcustom *stock-file* pathname
  (merge-pathnames "stocksunder300m.csv" *data-dir*)
  "The stock universe from finviz.")
(defvar *stocks* ())
(defun read-stocks (&key (file *stock-file*))
  (dolist (stock (setq *stocks* (csv-read 'stock file)))
    (setf (symbol-value (stock-ticker stock)) stock)))

(defun show-readers (type)
  (dolist (dslot (port:class-direct-slots (find-class type)))
    (let ((f (car (port:slot-definition-readers dslot))))
      (format t "~& ~20A   ~A~%" f (documentation f t)))))

(defgeneric query-stocks (query)
  (:method ((query (eql :help)))
    (format t "~&Print stocks which satisfy a certain condition.
The atomic queries are:~%")
    (show-readers 'stock)
    (format t "~&The atomic queries can be combined, e.g.:~%~S~%"
            '(AND (< 10 (STOCK-P/E STOCK)) (EQ (STOCK-COUNTRY STOCK)
                                            'FIN::USA))))
  (:method ((query function))
    (dolist (stock *stocks*)
      (let ((res (funcall query stock)))
        (when res
          (format t "~7A: ~S~%" (stock-ticker stock) res)))))
  (:method ((query cons))
    (query-stocks (compile nil `(lambda (stock) ,query))))
  (:method ((query pathname)) (query-stocks (read-from-file query)))
  (:method ((query string)) (query-stocks (read-from-file query))))

(defgeneric query-funds (query)
  (:method ((query function))
    (maphash (lambda (file fund)
               (let ((res (funcall query fund)))
                 (when res
                   (format t "~A: ~S~%" file res))))
             *funds*))
  (:method ((query cons))
    (query-funds (compile nil `(lambda (fund) ,query))))
  (:method ((query pathname)) (query-funds (read-from-file query)))
  (:method ((query string)) (query-funds (read-from-file query))))

(provide :grepfin)
;;; file grepfin.lisp ends here
