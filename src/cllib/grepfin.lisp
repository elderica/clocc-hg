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
  (require :cllib-miscprint (translate-logical-pathname "cllib:miscprint"))
  (require :cllib-csv (translate-logical-pathname "cllib:csv")))

(defpackage #:grepfin
  (:nicknames #:gf)
  (:use #:cl #:port #:cllib)
  (:export #:*data-dir* #:*fund-dir* #:*stock-file* #:*funds-db*
           #:ensure-data #:query-funds #:query-stocks #:init))

(in-package #:grepfin)

;;; * data

(defcustom *data-dir* pathname (merge-pathnames "data/" (user-homedir-pathname))
  "The location of all financial data.")
(defcustom *fund-dir* pathname (merge-pathnames "funds/" *data-dir*)
  "The location of the tree of funds data from MFFAIS.")

(defpackage "FIN" (:use))

;; export from http://www.mffais.com/
(defcsv holding (:package "FIN")
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
   ("Lifetime Sell Total" lifetime-sell-total float)
   ("Lifetime Sell Avg. Price" lifetime-sell-avg-price float)
   ("Lifetime Avg. Return Pct" lifetime-avg-return-% float)))

;; extra accessors (don't forget to show them in query-funds@:help!)
(defun holdings-total-value (holdings)
  "The total value of holdings (sum of TODAY-VALUE's.)"
  (reduce #'+ holdings :key #'holding-today-value))
(defun holding-stock (holding)
  "Return the STOCK object corresponding to the HOLDING or NIL if not known."
  (let ((symbol (holding-symbol holding)))
    (and (boundp symbol) (symbol-value symbol))))

(defvar *funds* (make-hash-table :test 'equalp))

(defun read-fund (file)
  (setf (gethash (pathname-name file) *funds*) (csv-read 'holding file)))
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
         (db-fwd (or (ignore-errors (file-write-date *funds-db*)) 0)))
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

(defun ensure-data ()
  (read-stocks)
  (ensure-funds)
  ;; make sure that we know all stocks mentioned in funds and they match
  (let ((unknown (make-hash-table :test 'eq)))
    (maphash (lambda (file holdings)
               (dolist (holding holdings)
                 (unless (boundp (holding-symbol holding))
                   (push file (gethash (holding-symbol holding) unknown)))))
             *funds*)
    (format t "~&~:D unknown stock~:P and ~:D known stock~:P in ~:D fund~:P~%"
            (hash-table-count unknown) (length *stocks*)
            (hash-table-count *funds*)))
  ;; report counts of some symbols
  (format t "~& ## stock counts:~%")
  (dolist (slot '(sector industry country))
    (format t "~& == ~A" slot)
    (let ((ht (make-hash-table :test 'eq)))
      (dolist (stock *stocks*)
        (incf (gethash (slot-value stock slot) ht 0)))
      (print-counts ht)))
  (format t "~& ## fund counts:~%")
  (dolist (slot '(activity industry country))
    (format t "~& == ~A" slot)
    (let ((ht (make-hash-table :test 'eq)))
      (maphash (lambda (file holdings)
                 (declare (ignore file))
                 (dolist (holding holdings)
                   (incf (gethash (slot-value holding slot) ht 0))))
               *funds*)
      (print-counts ht))))

;;; * queries

(defun show-readers (type)
  "Print the list of slot accessors for a TYPE defined with DEFCSV."
  (dolist (dslot (port:class-direct-slots (find-class type)))
    (let ((f (car (port:slot-definition-readers dslot))))
      (format t "~& ~20A   ~A~%" f (documentation f t)))))
(defun show-extras (extras)
  "Print the list of extra accessor functions."
  (dolist (f extras)
    (format t "~& ~A~%  ~A~%" f (documentation f 'function))))

(defgeneric query-stocks (query)
  (:method ((query (eql :help)))
    (format t "~&~S prints stocks which satisfy a certain condition
on the variable ~S of type ~S.
The atomic queries are:~%" 'query-stocks 'STOCK 'STOCK)
    (show-readers 'stock)
    (format t "~&The atomic queries can be combined, e.g.:~%~S
to list all ~A stocks with p/e>10."
            '(AND (< 10 (STOCK-P/E STOCK))
              (EQ (STOCK-COUNTRY STOCK) 'FIN::USA)
              (STOCK-P/E STOCK))
            'FIN::USA))
  (:method ((query function))
    (dolist (stock *stocks*)
      (let ((res (funcall query stock)))
        (when res
          (format t "~7A: ~A~%" (stock-ticker stock) res)))))
  (:method ((query cons))
    (query-stocks (compile nil `(lambda (stock) ,query))))
  (:method ((query pathname)) (query-stocks (read-from-file query)))
  (:method ((query string)) (query-stocks (read-from-file query))))

(defgeneric query-funds (query)
  (:method ((query (eql :help)))
    (format t "~&~S prints funds which satisfy a certain condition
on the variable ~S of type ~S.
The atomic queries are:~%" 'query-funds 'HOLDINGS '(LIST HOLDING))
    (show-readers 'holding)
    (format t "~&Extra functions:~%")
    (show-extras '(holdings-total-value holding-stock))
    (format t "~&The atomic queries can be combined, e.g.:~%~S
to list all the funds who invest more that 5%
in a stock with market cap less than 100M;
or~%~S~%to list all the funds who hold more than 10% of a known stock.
\(STOCK-MARKET-CAP is in millions of dollars,
 while HOLDING-TODAY-VALUE is in dollars).~%"
            '(LET ((THRESHOLD (/ (HOLDINGS-TOTAL-VALUE HOLDINGS) 20)))
              (MAPCAR #'HOLDING-SYMBOL
               (REMOVE-IF-NOT
                (LAMBDA (HOLDING)
                  (AND (> (HOLDING-TODAY-VALUE HOLDING) THRESHOLD)
                       (LET ((STOCK (HOLDING-STOCK HOLDING)))
                         (AND STOCK (< (STOCK-MARKET-CAP STOCK) 100)))))
                HOLDINGS)))
            '(MAPCAR #'HOLDING-SYMBOL
              (REMOVE-IF-NOT
               (LAMBDA (HOLDING)
                 (LET ((STOCK (HOLDING-STOCK HOLDING)))
                   (AND STOCK
                        (< (* 1d6 (STOCK-MARKET-CAP STOCK))
                           (* 1d1 (HOLDING-TODAY-VALUE HOLDING))))))
               HOLDINGS))))
  (:method ((query function))
    (maphash (lambda (file holdings)
               (let ((res (funcall query holdings)))
                 (when res
                   (format t "~A: ~A~%" file res))))
             *funds*))
  (:method ((query cons))
    (query-funds (compile nil `(lambda (holdings) ,query))))
  (:method ((query pathname)) (query-funds (read-from-file query)))
  (:method ((query string)) (query-funds (read-from-file query))))

(defun init ()
  (ensure-data)
  (format t "~%~40~~%")
  (query-stocks :help)
  (format t "~%~40~~%")
  (query-funds :help))

(provide :grepfin)
;;; file grepfin.lisp ends here
