;;; File: <gq.lisp - 1997-10-31 Fri 16:46:38 EST - sds@WINTERMUTE.eagle>
;;;
;;; GetQuote
;;; get stock/mutual fund quotes from the Internet
;;; via the WWW using HTTP/1.0, save into a file, plot.
;;;
;;; $Id$
;;; $Source$
;;; $Log$
;;; Revision 1.1  1997/10/15 15:49:26  sds
;;; Initial revision
;;;
;;;
;;; To run regularly (s/a gq.bat)
;;; at 19:00 /every:M,T,W,Th,F "c:\bin\clisp\lisp.exe -M c:\bin\clisp\lispinit.mem -q -i c:/home/sds/lisp/gq -x \"(update-quotes nil)\""

(unless (boundp '*month-names*)
  (load "c:/home/sds/fx/util"))

;;;
;;; {{{ HTML parsing
;;;

;(setq *read-eval* nil *read-suppress* t) ; for parsing
;(setq *read-eval* t *read-suppress* nil) ; original

(defvar *html-readtable* (copy-readtable nil)
  "The readtable for HTML parsing.")
(defvar *html-tag* (list '*html-tag*)
  "*The car of any html tag that is read.")
(defvar *html-parse-tags* nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defvar *whitespace* '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page)
  "*The whitespace characters.")

(defun strip-html-markup (str)
  "Return a new stirng, sans HTML."
  (declare (string str))
  (apply #'concatenate 'string
	 (do* ((p0 (position #\< str) (position #\< str :start p1))
	       (res (list (subseq str 0 p0)))
	       (p1 (position #\> str) (position #\> str :start (or p0 0))))
	      ((or (null p0) (null p1)) (nreverse res))
	   (push (subseq str (1+ p1) (position #\< str :start p1)) res))))

(defun read-html-markup (stream char)
  "Skip through the HTML markup. CHAR=`<'"
  (declare (ignore char))
  (if *html-parse-tags*
      (cons *html-tag* (read-delimited-list #\> stream t))
      (do () ((char= (read-char stream t nil t) #\>)))))

(defun html-tag-p (obj)
  "Return T if the object is an HTML tag."
  (if *html-parse-tags* (and (consp obj) (eq (car obj) *html-tag*))
      (null obj)))

(set-macro-character #\< #'read-html-markup nil *html-readtable*)
(set-macro-character #\> (get-macro-character #\) nil) nil *html-readtable*)
(set-syntax-from-char #\# #\a *html-readtable*)
(set-syntax-from-char #\: #\a *html-readtable*)
(set-syntax-from-char #\; #\a *html-readtable*)
;(set-macro-character #\: (get-macro-character #\)) nil *html-readtable*)
;(set-macro-character #\, (get-macro-character #\a) nil *html-readtable*)

;;;
;;; }}}{{{ URL handling
;;;

(defstruct (url (:print-function print-url))
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot "" :type string)		; protocol
  (user "" :type string)		; username
  (pass "" :type string)		; password
  (host "" :type string)		; hostname
  (port 0  :type fixnum)		; port number
  (path "/" :type string))		; pathname

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded trere,
guess from the protocol."
  (unless (url-p url) (setq url (parse-url url)))
  (if (zerop (url-port url)) (socket-service-port (url-prot url))
      (url-prot url)))

(defun url-path-dir (url)
  "Return the dir part of the url's path, droping the file name."
  (unless (url-p url) (setq url (parse-url url)))
  (subseq (url-path url) 0 (1+ (position #\/ (url-path url) :from-end t))))

(defun url-path-file (url)
  "Return the file part of the url's path, droping the dir."
  (unless (url-p url) (setq url (parse-url url)))
  (subseq (url-path url) (1+ (position #\/ (url-path url) :from-end t))))

(defun print-url (url &optional (stream t) depth)
  "Print the URL in the standard form."
  (declare (ignore depth) (type url url))
  (let ((str (or stream (make-string-output-stream))))
    (princ (url-prot url) str) (princ "://" str)
    (unless (equal "" (url-user url))
      (princ (url-user url) str)
      (unless (equal "" (url-pass url))
	(princ "#" str) (princ (url-pass url) str))
      (princ "@" str))
    (princ (url-host url) str)
    (unless (zerop (url-port url))
      (princ ":" str) (princ (url-port url) str))
    (unless (or (zerop (length (url-path url)))
		(eq #\/ (aref (url-path url) 0)))
      (princ "/" str))
    (princ (url-path url) str)
    (unless stream (get-output-stream-string str))))

(defvar *url-special-chars* "#%&*+,-./:=?@_~"
  "*The string consisting of non-alpanumeric charactes allowed in a URL.")

(defun url-constituent (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
	   (find char *url-special-chars* :test #'char=))))

(defun read-url (&optional (str t))
  "Read a URL from the stream."
  (parse-url
   (with-output-to-string (st)
     (do (zz) ((not (url-constituent (setq zz (read-char str)))) st)
       (princ zz st)))))

(defun parse-url (string &key (start 0) end)
  "Parse a string into a new URL."
  (declare (string string))
  (setq string (string-trim *whitespace* string))
  (let ((idx (search "://" string :start2 start :end2 end :test #'char=))
	idx0 (url (make-url)))
    (when idx
      (setf (url-prot url) (subseq string start idx))
      (setq start (+ idx 3)))
    (setq idx (position #\@ string :start start :test #'char=))
    (when idx
      (setq idx0 (position #\# string :start start :test #'char=))
      (if idx0 (setf (url-pass url) (subseq string (1+ idx0) idx)
		     (url-user url) (subseq string start idx0))
	  (setf (url-user url) (subseq string start idx)))
      (setq start (1+ idx)))
    (setq idx (position #\: string :start start :test #'char=))
    (setq idx0 (position #\/ string :start start :test #'char=))
    (when idx
      (setf (url-port url) (parse-integer string :start (1+ idx) :end idx0)))
    (when idx0
      (setf (url-path url) (subseq string idx0)))
    (setf (url-host url) (subseq string start (or idx idx0)))
    (when (equal "" (url-prot url))
      (cond ((not (mismatch "www" (url-host url) :test #'char= :end2 3))
	     (setf (url-prot url) "http"))
	    ((not (mismatch "ftp" (url-host url) :test #'char= :end2 3))
	     (setf (url-prot url) "ftp"))))
    url))

;(defmacro parse-url (string &rest keys)
;  "Read a URL from the string."
;  (let ((st (getsym "url")))
;    `(with-input-from-string (,st ,string ,@keys)
;      (read-url ,st))))

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping `:'."
  (sock nil)				; socket to read from
  (buff "" :type string)		; buffer string
  (posn 0 :type fixnum))		; position in the buffer

(defmacro with-open-url ((socket url &optional (rt '*html-readtable*) err)
			 &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
The *readtable* is temporarily set to RT (defaults to *html-readtable*).
If this is an HTTP URL, also issue the GET command.
If this is an FTP URL, cd and get (if there is a file part).
ERR is the stream for information messages."
  (let ((rt-old (gensym "WOU")))
    `(let ((,socket (socket-connect (url-get-port ,url) (url-host ,url)))
	   (,rt-old *readtable*))
      (setq *readtable* (or ,rt *readtable*))
      (unwind-protect
	   (progn
	     (cond ((equal "http" (url-prot ,url))
		    (format ,socket "GET ~a HTTP/1.0~%~%" (url-path ,url))
		    (let ((sym (read ,socket)) (res (read ,socket)))
		      (when (string-equal sym "http/1.1")
			(when (>= res 400) ; error
			  (error "~d: ~a~%" res (read-line ,socket)))
			(when (= 302 res) ; redirection
			  (setq res (read-line ,socket))
			  (read-line ,socket) (read-line ,socket)
			  (setq sym (read-line ,socket)
				sym (parse-url (subseq sym (1+ (position
								#\: sym)))))
			  (close ,socket)
			  (format ,err " *** redirected to `~a' [~a]~%"
				  sym res)
			  (setq ,socket (socket-connect (url-get-port sym)
							(url-host sym)))
			  (format ,socket "GET ~a HTTP/1.0~%~%"
				  (url-path sym))))))
		   ((equal "ftp" (url-prot ,url))
		    (format ,socket "cd ~a~%" (url-path-dir ,url))
		    (unless (equal "" (url-path-file ,url))
		      (format "get ~a~%" (url-path-file ,url))))
		   ((equal "whois" (url-prot ,url))
		    (format ,socket "~a~%" (url-path-file ,url))))
	     ,@body)
	(setq *readtable* ,rt-old)
	(close ,socket)))))

(defun read-next (ts)
  "Read the next something from TS - a text stream."
  (do (str tok pos) (nil)
    (when (or (typep pos 'error) (>= (ts-posn ts) (length (ts-buff ts))))
      (unless (typep pos 'error) (setf (ts-posn ts) 0))
      (setf str (read-line (ts-sock ts) nil *eof*))
      (when (eq str *eof*)
	(if (typep pos 'error) (error pos) (return-from read-next *eof*)))
      (setq str (nsubstitute #\space #\: str)
	    str (nsubstitute #\space #\, str))
      ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
      (do ((beg -1)) ((null (setq beg (position #\. str :start (1+ beg)))))
	(if (or (digit-char-p (elt str (1- beg)))
		(digit-char-p (elt str (1+ beg))))
	    (incf beg) (setf (elt str beg) #\Space)))
      (setf (ts-buff ts) (if (typep pos 'error)
			     (concatenate 'string (ts-buff ts) str) str)))
    (multiple-value-setq (tok pos)
      (ignore-errors
       (read-from-string (ts-buff ts) nil *eof* :start (ts-posn ts))))
    (unless (typep pos 'error) (setf (ts-posn ts) pos))
    (unless (or (typep pos 'error) (eq tok *eof*))
      (return-from read-next tok))))

;(defun read-next (ts) (read (ts-sock ts) nil *eof*))

(defsubst next-token (ts &optional (num 1))
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt) (dotimes (ii num tt)
	      (do () ((not (html-tag-p (setq tt (read-next ts)))))))))

(defsubst next-number (ts &optional (num 1))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt) (dotimes (ii num tt)
	      (do () ((numberp (setq tt (next-token ts))))))))

(defsubst skip-tokens (ts end &key (test #'eql) (key #'identity))
  "Skip tokens until END, i.e., until (test (key tocken) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts))) end) tt)))

(defun dump-url (url &optional (fmt "~3d: ~a~%") (out t))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream. Defaults to T."
  (setq url (if (url-p url) url (parse-url (string url))))
  (with-open-url (sock url *readtable* t)
    (do (rr (ii 0 (1+ ii)))
	((eq *eof* (setq rr (read-line sock nil *eof*))))
      (format out fmt ii rr))))

(defun whois (host)
  "Get the whois information on a host."
  (dump-url (make-url :prot "whois" :host "rs.internic.net"
		      :path (concatenate 'string "/" (string host)))
	    "~*~a~%"))

(defun dump-url-tockens (url &optional (fmt "~3d: ~a~%") (out t))
  "Dump the URL tocken by tocken.
See `dump-url' about the optional parameters."
  (setq url (if (url-p url) url (parse-url (string url))))
  (with-open-url (sock url *html-readtable* t)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
	((eq *eof* (setq rr (read-next ts))))
      (format out fmt ii rr))))

(defun gq-complete-url (url &rest ticks)
  "Complete URL to get the quotes for TICKS."
  (setq url (if (url-p url) (copy-url url) (parse-url (string url))))
  (setf (url-path url) (format nil "~a~{+~:@(~a~)~}" (url-path url) ticks))
  url)

(defun parse-geo-coord (st)
  "Return the number parsed from latitude or longitude (dd:mm:ss[nsew])
read from the stream."
  (let* ((sig 1) (cc (+ (read st) (/ (read st) 60.0d0)))
	 (lt (read st)) se nn)
    (if (numberp lt) (setq se lt nn 0 lt (string (read st)))
	(multiple-value-setq (se nn)
	  (parse-integer (setq lt (string lt)) :junk-allowed t)))
    (unless se (error "Cannot parse the seconds from `~a'~%" lt))
    (setq sig (cond ((or (char-equal (elt lt nn) #\n)
			 (char-equal (elt lt nn) #\e)) 1)
		    ((or (char-equal (elt lt nn) #\s)
			 (char-equal (elt lt nn) #\w)) -1)
		    (t (error "Wrong sign designation: `~a'. ~
Must be one of [N]orth, [S]outh, [E]ast or [W]est.~%" (elt lt nn)))))
    (* sig (+ cc (/ se 3600d0)))))

(defun geo-location (str &optional (num 2))
  "Return the latitude and longitude as two numbers from a string of
type \"48:51:00N 2:20:00E\". The optional second argument says how many
coordinates to read (default 2). If it is 1, return just one number, if
more than 1, return the list."
  (setq str (nsubstitute #\Space #\: (string str)))
  (with-input-from-string (st str)
    (if (= num 1) (parse-geo-coord st)
	(do ((ii 0 (1+ ii)) res) ((= ii num) (nreverse res))
	  (push (parse-geo-coord st) res)))))

;;;
;;; }}}{{{ Geo-Data
;;;

(defstruct (geo-data (:print-function print-geod) (:conc-name geod-))
  (name "??" :type string)		; the name of the place
  (pop 0 :type (real 0 *))		; population
  (lat 0.0 :type float)			; latitude
  (lon 0.0 :type float)			; longitude
  (zip nil :type list))			; list of zip codes.

(defun print-geod (gd &optional stream depth)
  "Print the geo-data."
  (declare (ignore depth))
  (format stream "Place: ~a~%Population: ~12:d;~30t~
Location: ~9,5f ~a  ~9,5f ~a~%Zip Code~p:~{ ~d~}~%"
	  (geod-name gd) (geod-pop gd)
	  (geod-lat gd) (if (minusp (geod-lat gd)) #\S #\N)
	  (geod-lon gd) (if (minusp (geod-lat gd)) #\W #\E)
	  (length (geod-zip gd)) (geod-zip gd)))

(defvar *census-gazetteer-url* (make-url :prot "http" :host "www.census.gov"
					 :path "/cgi-bin/gazetteer?")
  "*The URL to use to get the cite information.")

(defun cite-info (url &key city state zip (out t))
  "Get the cite info from the U.S. Gazetteer.
Print the results to the stream OUT (defaults to T) and return a list
of geo-data."
  (setq url (if (url-p url) (copy-url url) (parse-url (string url)))
	city (if city (substitute #\+ #\Space (string city)) "")
	state (if state (substitute #\+ #\Space (string state)) ""))
  (setf (url-path url) (format nil "~acity=~a&state=~a&zip=~a"
			       (url-path url) city state (or zip "")))
  (with-open-url (sock url *readtable* t)
    (do () ((search "<ul>" (read-line sock))))
    (do ((str "") res gd (ii 1 (1+ ii)))
	((or (search "</ul>" str)
	     (search "</ul>" (setq str (read-line sock))))
	 (nreverse res))
      ;; name
      (setq gd (make-geo-data :name (strip-html-markup str))
	    str (read-line sock))
      ;; population
      (setf (geod-pop gd) (parse-integer str :junk-allowed t :start
					 (1+ (position #\: str))))
      ;; location
      (setq str (nsubstitute #\Space #\: (read-line sock))
	    str (nsubstitute #\Space #\, str)
	    str (nsubstitute #\Space #\< str))
      (with-input-from-string (st str)
	(read st)
	(setf (geod-lat gd) (* (read st) (if (eq 'n (read st)) 1 -1))
	      (geod-lon gd) (* (read st) (if (eq 'w (read st)) 1 -1))))
      ;; zip
      (setq str (read-line sock))
      (setf (geod-zip gd)
	    (if (search "Zip Code" str)
		(with-input-from-string (st str :start (1+ (position #\: str)))
		  (do (rr re) ((null (setq rr (read st nil nil)))
			       (nreverse re))
		    (when (numberp rr) (push rr re))))
		(list zip)))
      (read-line sock) (setq str (read-line sock))
      (push gd res) (format out "~%~:d. " ii) (print-geod gd out))))

;;;
;;; }}}{{{ Daily Data
;;;

(defstruct (daily-data (:print-function print-dd) (:conc-name dd-))
  (nav 0.0 :type float)
  (chg 0.0 :type float)
  (prc 0.0 :type float)
  (bid 0.0 :type float)
  (ask 0.0 :type float)
  (pre 0.0 :type float)
  (low 0.0 :type float)
  (hgh 0.0 :type float))

(defun print-dd (dd &optional stream depth)
  "Print the daily datum."
  (declare (ignore depth) (type daily-data dd))
  (format stream
	  "price:~15t~7,2f~35tbid:~45t~7,2f
previous:~15t~7,2f~35task:~45t~7,2f
change:~15t~7,2f~35thigh:~45t~7,2f
%:~15t~7,2f~35tlow:~45t~7,2f~%"
	  (dd-nav dd) (dd-bid dd) (dd-pre dd) (dd-ask dd)
	  (dd-chg dd) (dd-hgh dd) (dd-prc dd) (dd-low dd)))

(defun get-quotes-apl (url &rest ticks)
  "Get the data from the APL WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks))
    (do (zz res dt (ts (make-text-stream :sock sock)))
	((or (null ticks) (eq (setq zz (next-token ts)) *eof*))
	 (cons dt (nreverse res)))
      (when (eq (car ticks) zz)
	(pop ticks)
	(push (make-daily-data
	       :nav (next-number ts) :chg (next-token ts) :prc (next-token ts)
	       :bid (next-token ts 2) :ask (next-token ts)
	       :pre (next-token ts 3) :low (next-token ts 2)
	       :hgh (next-token ts)) res)
	(setq dt (infer-date (next-token ts 3) (next-token ts)))))))

(defun get-quotes-pf (url &rest ticks)
  "Get the data from the PathFinder WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks))
    (do (zz res dt (ts (make-text-stream :sock sock)))
	((or (null ticks) (eq (setq zz (next-token ts)) *eof*))
	 (cons dt (nreverse res)))
      (if dt
	  (when (eq (car ticks) zz)
	    (pop ticks)
	    (push (make-daily-data :nav (next-number ts 6)) res))
	  (when (and (eq zz 'latest) (eq (next-token ts) 'prices))
	    (setq dt (infer-date (next-token ts) (next-token ts))))))))

(defun get-quotes-sm (url &rest ticks)
  "Get the data from the StockMaster WWW server."
  (do ((ti ticks (cdr ti)) ds vs hh ar res dt ts)
      ((null ti)
       (values (cons dt (nreverse res))
	       (apply #'mapcar
		      (lambda (&rest cns)
			(unless (every (lambda (cn)
					 (equalp (caar cns) (car cn)))
				       (cdr cns))
			  (error "Date mismatch: ~a~%" cns))
			(make-hist :date (caar cns) :navs (mapcar #'cdr cns)))
		      (nreverse hh))
	       (nreverse ar)))
    (with-open-url (sock (gq-complete-url url (car ti)))
      (skip-tokens (setq ts (make-text-stream :sock sock))
		   (car ti) :key (safe-fun #'car #'consp))
      (push (make-daily-data :nav (next-number ts) :chg (next-number ts)
			     :prc (next-number ts)) res)
      (setq dt (infer-date (next-token ts 4) (next-token ts)))
      (push (list (next-number ts 5) (next-number ts)
		  (next-number ts) (next-number ts)) ar)
      (next-token ts)
      (setq ds nil vs nil)
      (push (mapcar
	     #'cons
	     (dotimes (ii 10 (nreverse ds))
	       (push (infer-date (next-token ts) (next-token ts)) ds))
	     (dotimes (ii 10 (nreverse vs))
	       (push (next-number ts) vs))) hh))))

(defvar *get-quote-url-list*
  (list (list (make-url :prot "http" :host "qs.secapl.com"
			:path "/cgi-bin/qs?ticks=")
	      #'get-quotes-apl "APL")
	(list (make-url :prot "http" :host "quote.pathfinder.com"
			:path "/money/quote/qc?symbols=")
	      #'get-quotes-pf "PathFinder")
	(list (make-url :prot "http" :host "www.stockmaster.com"
			:path "/cgi-bin/graph?sym=")
	      #'get-quotes-sm "StockMaster"))
  "*The list of known URL templates for getting quotes.")

(defun get-quotes (server &rest ticks)
  "Get the quotes from one of `*get-quote-url-list*'.
The first arg, SERVER, when non-nil, specifies which server to use."
  (if server
      (let ((qq (if (numberp server) (elt *get-quote-url-list* server)
		    (find server *get-quote-url-list* :key #'third
			  :test #'string-equal))))
	(values-list (cons (third qq)
			   (multiple-value-list
			       (apply (second qq) (first qq) ticks)))))
      (do ((ql *get-quote-url-list* (cdr ql)) res name)
	  ((car res) (values-list (cons name res)))
	(setq name (car (last (car ql)))
	      res (multiple-value-list
		      (ignore-errors (apply (cadar ql) (caar ql) ticks)))))))

(defun infer-date (mon day)
  "Guess what the date is.
Return the most recent date with these month and day."
  (multiple-value-bind (se mi ho da mo ye) (get-decoded-time)
    (declare (ignore se mi ho da))
    (setq mon (if (numberp mon) mon
		  (1+ (position mon *month-names* :test
				(lambda (s0 s1)
				  (string-equal s0 s1 :start1 0 :end1 2
						:start2 0 :end2 2))))))
    (make-date :da day :mo mon :ye (if (<= mon mo) ye (1- ye)))))

;;;
;;; }}}{{{ Holdings
;;;

(defvar *hist-data-file*
  (merge-pathnames "text/invest.txt" (user-homedir-pathname))
  "*The file with the historical data.")
(defvar *hist-data-file-header*
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
(defvar *hist-data-file-sep* '~
  "*The separator between the portfolio and its history")
(defvar *holdings* nil "The holdings, to be read from `*hist-data-file*'.")
(defvar *history* nil "The history, to be read from `*hist-data-file*'.")

(defstruct (pfl (:print-function print-pfl))
  (tick nil :type symbol)
  (nums 0.0 :type float)
  (bprc 0.0 :type float)
  (name "" :type string))

(defun read-pfl (stream ra)
  "Read a PFL from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (values (make-pfl :tick ra :nums (read stream) :bprc (read stream)
		    :name (read stream)) (read stream nil *eof*)))

(defun print-pfl (pfl &optional stream depth)
  "Print the PFL struct."
  (declare (type pfl pfl) (ignore depth))
  (format stream "~:@(~a~) ~8,3f ~7,2f ~s" (pfl-tick pfl) (pfl-nums pfl)
	  (pfl-bprc pfl) (pfl-name pfl)))

(defstruct (hist (:print-function print-hist))
  (date (make-date) :type date)
  (totl 0.0 :type float)
  (navs nil :type list))

(defun read-hist (stream ra)
  "Read a HIST from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (do ((hist (make-hist :date (parse-date ra) :totl (read stream)))
       (vl (read stream) (read stream nil *eof*)))
      ((not (numberp vl))
       (progn (nreverse (hist-navs hist)) (values hist vl)))
    (push vl (hist-navs hist))))

(defun print-hist (hist &optional stream depth)
  "Print the HIST struct."
  (declare (type hist hist) (ignore depth))
  (format stream "~a ~15,5f~{ ~7,2f~}" (hist-date hist)
	  (hist-totl hist) (hist-navs hist)))

(defun hist-totl-comp (hold navs)
  "Compute the correct total from the hist rec and holdings list."
  (declare (list hold navs))
  (let ((tot 0))
    (map nil (lambda (ho vl) (incf tot (* vl (pfl-nums ho)))) hold navs)
    tot))

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
  (with-open-file (outst file :direction :io :if-exists :overwrite)
    (cond ((read outst nil nil)		; file existed
	   (format t "File `~a' exists. Appending...~%" file)
	   (do (zz)
	       ((or (eq zz *eof*) (eq zz *hist-data-file-sep*))
		(when (eq zz *eof*)
		  (error "File `~a' is corrupted: `~a' not found.~%"
			 file *hist-data-file-sep*)))
	     (setq zz (read outst nil *eof*)))
	   (terpri outst)
	   (write-list-to-stream hist outst #'print-hist))
	  (t				; new file
	   (format t "File `~a' does not exists. Creating...~%" file)
	   (princ *hist-data-file-header* outst)
	   (terpri outst)
	   (write-list-to-stream hold outst #'print-pfl)
	   (terpri outst) (princ *hist-data-file-sep* outst) (terpri outst)
	   (write-list-to-stream hist outst #'print-hist))))
  (format t "~d record~:p about ~r holding~:p written.~%"
	  (length hist) (length hold)))

(defun gq-fix-hist (hold hist hhh)
  "Fix the values in the history. Return T if something was fixed."
  (declare (list hist hold hhh))
  (let ((fixed nil))
    (when hhh
      (setq hhh
	    (map-sorted
	     'list
	     (lambda (hi hs)
	       (when hs
		 (cond (hi
			(unless (every #'= (hist-navs hi) (hist-navs hs))
			  (format t "Wrong NAVs on ~a:
~5tOriginal:~15t~{~7,2f~}~%~5tActual:~15t~{~7,2f~}~%"
				  (hist-date hi) (hist-navs hi) (hist-navs hs))
			  (setf (hist-navs hi) (hist-navs hs) fixed t)))
		       (t
			(format t "Missing record for ~a. Fixed.~%"
				(hist-date hs))
			(setq hi hs fixed t))))
	       hi)
	     #'date-less-p hist hhh :ckey #'hist-date))
      ;; modify hist by side effect
      (setf (car hist) (car hhh) (cdr hist) (cdr hhh)))
    (let (ntot)
      (dolist (hr hist)
	(setq ntot (hist-totl-comp hold (hist-navs hr)))
	(unless (same-num-p ntot (hist-totl hr) 0.01d0 0.00001d0)
	  (format t "Bad total (~a): ~15,5f; correct: ~15,5f~%"
		  (hist-date hr) (hist-totl hr) ntot)
	  (setf (hist-totl hr) ntot fixed t))))
    fixed))

(defun process-results (hold hist srv res yea &optional (str t))
  "Process the results, update the history.
Return non-nil if the history was updated and needs to be saved.
RES is a list of SERVER, DATE and DAILY-RECS.
YEA is the list of 1, 3, 5, and 10-year returns."
  (declare (list hold hist res))
  (unless res
    (return-from process-results (format t "no results - timed out?~%")))
  (let ((lh (last hist)) cv ov pv (ctot 0.0d0) (otot 0.0d0) (ptot 0.0d0)
	(begd (hist-date (car hist))) pers apy db
	(nnavs (mapcar #'dd-nav (cdr res)))
	(pnavs (mapcar #'dd-pre (cdr res))))
    (declare (float ctot otot))
    (format str "~2%~72:@<~a results [~a]:~>
~72:@<<[~a -- ~a (~:d days)]>~>~2%"
	    srv (current-time nil) begd (car res)
	    (setq db (days-between begd (car res))))
    (apply #'mapc
	   (lambda (hl dd &optional ye)
	     (format str "Fund: ~:@(~a~) [~a]~48t~7,3f * ~6,2f = $~a~%"
		    (pfl-tick hl) (pfl-name hl) (pfl-nums hl) (dd-nav dd)
		    (commas (setq cv (* (pfl-nums hl) (dd-nav dd))) 2 6))
	     (when ye
	       (format str "~10t[1ye:~6,2f%; 3ye:~6,2f%; 5ye:~6,2f%; ~
10ye:~6,2f%]~%" (first ye) (second ye) (third ye) (fourth ye)))
	     (print-dd dd str)
	     (setq ov (* (pfl-nums hl) (pfl-bprc hl))
		   pv (* (pfl-nums hl)
			 (cond ((not (zerop (dd-pre dd))) (dd-pre dd))
			       ((not (zerop (dd-chg dd)))
				(- (dd-nav dd) (dd-chg dd)))
			       (t (elt (hist-navs (car lh))
				       (position (pfl-tick hl) hold :test #'eq
						 :key #'pfl-tick))))))
	     (incf ctot cv) (incf otot ov) (incf ptot pv)
	     (multiple-value-setq (pers apy) (percent-change ov cv db))
	     (format str "   P/L: total:~15t~a - ~a = ~a~55t[~7,3f% APY:~8,3f%]
        today:~15t~a - ~a = ~a~55t[~7,3f%]~2%"
		     (commas cv 2 7) (commas ov 2 7) (commas (- cv ov) 2 5)
		     pers apy (commas cv 2 7) (commas pv 2 7)
		     (commas (- cv pv) 2 5) (percent-change pv cv)))
	   hold (rest res) (if yea (list yea) nil))
    (multiple-value-setq (pers apy) (percent-change otot ctot db))
    (format str " * P/L: total:~15t~a - ~a = ~a~55t[~7,3f% APY:~8,3f%]
        today:~15t~a - ~a = ~a~55t[~7,3f%]~2%"
	    (commas ctot 2 7) (commas otot 2 7) (commas (- ctot otot) 2 5)
	    pers apy (commas ctot 2 7) (commas ptot 2 7)
	    (commas (- ctot ptot) 2 5) (percent-change ptot ctot))
    (cond ((date-more-p (car res) (hist-date (car lh)))
	   (unless (or (equalp (tomorrow (hist-date (car lh))) (car res))
		       (every (lambda (cv pv)
				(or (zerop pv)
				    (same-num-p cv pv 0.01d0 0.001d0)))
			      (hist-navs (car lh)) pnavs))
	     (format str "A discrepancy found:~% last record:~15t~{~7,2f~}~% ~
previous day:~15t~{~7,2f~}~%Added an extra record~%~5t~{~a~}~%"
		     (hist-navs (car lh)) pnavs
		     (setf (cdr lh)
			   (list (make-hist :date (tomorrow (car res) -1)
					    :navs pnavs :totl
					    (hist-totl-comp hold pnavs)))))
	     (setq lh (cdr lh)))
	   (cond ((equalp nnavs (hist-navs (car lh)))
		  (format str "Same NAVs as on ~a, no record added.~%"
			  (hist-date (car lh))))
		 (t (format str "A record for date ~a added:~%~5t~{~a~}~%"
			    (car res)
			    (setf (cdr lh)
				  (list (make-hist
					 :date (car res) :navs nnavs
					 :totl (hist-totl-comp hold
							       nnavs)))))
		    t)))
	  (t (format str "Old news [~a].~%" (car res))))))

(defun plot-portfolio (hold hist)
  "Plot the portfolio."
  (declare (list hold hist))
  (do ((i 0 (1+ i)) (hl hold (cdr hl)) res)
      ((null hl)
       (plot-dated-lists
	(hist-date (car hist)) (hist-date (car (last hist)))
	(cons (make-dated-list :ll hist :date #'hist-date :name
			       "Total Value" :misc #'hist-totl)
	      (nreverse res)) :rel t :slot 'misc
	:title "Portfolio History" :data-style "linespoints"
	:ylabel "Relative Value"))
    (push (make-dated-list
	   :ll hist :date #'hist-date :name (pfl-name (car hl))
	   :code (pfl-tick (car hl)) :misc
	   (let ((i i)) (lambda (hs) (elt (hist-navs hs) i)))) res)))

;;;
;;; }}}{{{ Run the thing
;;;

(defvar *gq-log* (pathname "c:/home/sds/text/getquote.log")
  "The log file for the batch processing.")

(defun update-quotes (&optional (plot nil plotp) server)
  "Read the history. Update quotes. Plot (optionally),
if PLOT is non-nil, or if it is not given but there was new data.
If PLOT is T, just plot, do not try to update quotes."
  (unless (eq *standard-output* *standard-input*)
    (setq *hist-data-file* (pathname "c:/home/sds/text/invest.txt")))
  (multiple-value-setq (*holdings* *history*)
    (read-data-file *hist-data-file*))
  (unless (eq plot t)
    (multiple-value-bind (srv res hhh yea str fixed new)
	(apply #'get-quotes server (mapcar #'pfl-tick *holdings*))
      (setq str (or (eq *standard-output* *standard-input*)
		    (open *gq-log* :direction :output :if-exists
			  :append :if-does-not-exist :create))
	    fixed (gq-fix-hist *holdings* *history* hhh)
	    new (process-results *holdings* *history* srv res yea str))
      (when (or fixed new)
	(save-data *hist-data-file* *holdings* *history*)
	(unless plotp (setq plot t)))
      (unless (eq t str) (close str))))
  (when plot (plot-portfolio *holdings* *history*)))

;;; }}} File gq.lisp ends here
