;;; File: <url.lisp - 1999-2-1 Mon 18:17:42 EST sds@eho.eaglets.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; Copyright (C) 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id$
;;; $Source$
;;; $Log$
;;; Revision 1.16  1999/01/13 20:43:24  sds
;;; Replaced top-level `*html-readtable*' creation forms with a new
;;; function, `make-html-readtable'.
;;;
;;; Revision 1.15  1999/01/13 18:27:03  sds
;;; `read-html-markup' now handles #\;, #\: and #\, so it is not necessary
;;; now to remove these characters from buffer in `ts-pull-next'.
;;;
;;; Revision 1.14  1999/01/09 22:15:42  sds
;;; Extracted `ts-pull-next' from `read-next'.
;;;
;;; Revision 1.13  1999/01/08 17:15:25  sds
;;; Made `read-html-markup' skip `*html-specials*'.
;;; Added `with-timeout' for CMUCL, `socket-to-file', `*ts-kill*' (used in
;;; `read-next'), `url-get' (unifies all `url-get-*' functions).
;;;
;;; Revision 1.12  1999/01/07 03:58:08  sds
;;; Use `index-t' instead of (unsigned-byte 20).
;;; Use `file-size-t' instead of (unsigned-byte 32).
;;;
;;; Revision 1.11  1998/12/29 17:12:14  sds
;;; Added `*nntpserver*', `url-get-host', `*url-default-sleep*',
;;; `*url-default-timeout*', `sleep-mesg', `with-timeout',
;;; `y-or-n-p-timeout', `finger'.
;;; Added news URL handling.
;;; Added `bin' argument to `ftp-get-passive-socket'.
;;;
;;; Revision 1.10  1998/12/07 16:53:22  sds
;;; Added MAILTO handling; made :prot a keyword.
;;; New function: `send-mail'.
;;; Renamed `ftp-ask' to `url-ask'.
;;;
;;; Revision 1.9  1998/11/21 21:01:43  sds
;;; Added `throw-timeout' to `open-socket-retry' and `open-url'.
;;;
;;; Revision 1.8  1998/11/20 21:52:11  sds
;;; Added reget functionality to `ftp-get-file'.
;;;
;;; Revision 1.7  1998/11/20 03:15:50  sds
;;; Added `open-socket-retry'.
;;;
;;; Revision 1.6  1998/11/19 20:19:37  sds
;;; Added ftp handling: `ftp-ask', `ftp-parse-sextuple', `url-open-ftp',
;;; `ftp-get-passive-socket', `ftp-get-file', `*buf-size*', `ftp-list'.
;;; Separated `open-url' from `open-socket' and made sure that the former
;;; does indeed opens a socket.
;;;
;;; Revision 1.5  1998/10/30 20:55:40  sds
;;; Replaced `parse-url' with a generic function.
;;; Added `*html-specials*', `html-translate-specials',
;;; `*hyperspec-root*' and `hyperspec-snarf-examples'.
;;;
;;; Revision 1.4  1998/07/31 16:53:21  sds
;;; Declared `stream' as a stream in `print-*'.
;;;
;;; Revision 1.3  1998/06/30 13:48:08  sds
;;; Switched to `print-object'.
;;;
;;; Revision 1.2  1998/05/26 20:19:35  sds
;;; Adopted to work with ACL 5beta.
;;;
;;; Revision 1.1  1998/03/10 18:31:44  sds
;;; Initial revision
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print")
  (sds-require "util") (sds-require "date")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; {{{ HTML parsing
;;;

;;(setq *read-eval* nil *read-suppress* t) ; for parsing
;;(setq *read-eval* t *read-suppress* nil) ; original

(defcustom *html-parse-tags* (member t nil) nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defcustom *html-verbose* (member t nil) nil "*Be verbose while parsing.")
(defstruct html-tag data)
(defcustom *html-specials* list
  '(("&gt;" . #\>) ("&lt;" . #\<) ("&quot;" . #\") ("&amp;" . #\&)
    ("&nbsp;" . #\Space))
  "Alist of translations of HTML specials like `&*'.")

(defun strip-html-markup (str)
  "Return a new string, sans HTML."
  (declare (simple-string str))
  (do* ((p0 (position #\< str) (position #\< str :start p1))
        (res (list (subseq str 0 p0)))
        (p1 (position #\> str) (position #\> str :start (or p0 0))))
       ((or (null p0) (null p1))
        (apply #'concatenate 'string (nreverse res)))
    (push (subseq str (1+ p1) (position #\< str :start p1)) res)))

(defun read-html-markup (stream char)
  "Skip through the HTML markup. CHAR=`<'"
  (declare (stream stream) (character char))
  (ecase char
    (#\; #\;) (#\, #\,) (#\: #\:)
    (#\< (make-html-tag
          :data (if *html-parse-tags* (read-delimited-list #\> stream t)
                    (do () ((char= (read-char stream t nil t) #\>))))))
    (#\&
     (do ((cc (read-char stream nil nil t) (read-char stream nil nil t)) rr)
         ((or (null cc) (char= cc #\;) (char= cc #\#))
          (if (null cc) (error "`&' must be terminated with `;' or `#'")
              (if *html-parse-tags*
                  (or (cdr (assoc (coerce (cons #\& (nreverse (push cc rr)))
                                          'string)
                                  *html-specials* :test #'string-equal))
                      #\Space)
                  #\Space)))
       (when *html-parse-tags* (push cc rr))))))

(defun make-html-readtable ()
  "Make the readtable for parsing HTML."
  (let ((rt (copy-readtable)))
    (set-macro-character #\< #'read-html-markup nil rt)
    (set-macro-character #\& #'read-html-markup nil rt)
    (set-macro-character #\> (get-macro-character #\)) nil rt)
    (set-syntax-from-char #\; #\a rt)
    ;;(set-macro-character #\; #'read-html-markup nil rt)
    (set-syntax-from-char #\# #\a rt)
    (set-syntax-from-char #\: #\a rt)
    (set-macro-character #\: #'read-html-markup nil rt)
    (set-syntax-from-char #\, #\a rt)
    (set-macro-character #\, #'read-html-markup nil rt)
    rt))

(defcustom *html-readtable* readtable (make-html-readtable)
  "The readtable for HTML parsing.")

;;;
;;; }}}{{{ URL handling
;;;

(defstruct (url)
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot nil :type symbol)       ; protocol
  (user "" :type simple-string) ; username
  (pass "" :type simple-string) ; password
  (host "" :type simple-string) ; hostname
  (port 0  :type fixnum)        ; port number
  (path "" :type simple-string)) ; pathname

(defun socket-service-port (protocol)
  "Return the port number of the PROTOCOL."
  (let ((prot (string-downcase (string protocol))))
    (declare (simple-string prot))
    #+clisp (let ((port (lisp:socket-service-port prot)))
              (if (/= -1 port) port nil))
    #-clisp
    (with-open-file (fl #+unix "/etc/services" #+win32
                        (concatenate 'string (system:getenv "windir")
                                     "/system32/etc/services")
                        :direction :input)
      (do ((st (read-line fl nil +eof+) (read-line fl nil +eof+))
           res pos srv)
          ((or res (eq st +eof+)) res)
        (unless (or (equal "" st) (char= #\# (schar st 0)))
          (setf (values srv pos) (read-from-string st nil ""))
          (when (string-equal prot srv)
            (nsubstitute #\space #\/ st)
            (setq res (read-from-string st nil nil :start pos))))))))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (declare (type url url))
  (if (zerop (url-port url))
      (or (case (url-prot url)
            (:mailto (socket-service-port "smtp"))
            (:news (socket-service-port "nntp")))
          (socket-service-port (url-prot url)))
      (url-port url)))

(defcustom *nntpserver* simple-string
  (or (getenv "NNTPSERVER") "localhost")
  ;; (setq *nntpserver* "news0-alterdial.uu.net")
  "*The NNTP server to be user for `news' URLs.")

(defun url-get-host (url)
  "Get the right host for the URL: if it is a `news', use `*nntpserver*'."
  (declare (type url url))
  (case (url-prot url)
    ((:news :nntp) *nntpserver*)
    (t (url-host url))))

(defun url-path-dir (url)
  "Return the dir part of the url's path, dropping the file name."
  (setq url (url url))
  (subseq (url-path url) 0
          (1+ (or (position #\/ (url-path url) :from-end t) -1))))

(defun url-path-file (url)
  "Return the file part of the url's path, dropping the dir."
  (setq url (url url))
  (subseq (url-path url)
          (1+ (or (position #\/ (url-path url) :from-end t) -1))))

(defmethod print-object ((url url) (stream stream))
  "Print the URL in the standard form."
  (when *print-readably* (return-from print-object (call-next-method)))
  (when *print-escape* (write-string "\"" stream))
  (let ((*print-escape* nil))
    (write (url-prot url) :stream stream :case :downcase)
    (case (url-prot url)
      ((:mailto :news :nntp :file) (write-string ":" stream))
      (t (write-string "://" stream)))
    (unless (string= "" (url-user url))
      (write (url-user url) :stream stream)
      (unless (string= "" (url-pass url))
        (write-string "#" stream) (write (url-pass url) :stream stream))
      (write-string "@" stream))
    (write (url-host url) :stream stream)
    (unless (zerop (url-port url))
      (write-string ":" stream) (write (url-port url) :stream stream))
    (assert (or (zerop (length (url-path url)))
                (eq #\/ (aref (url-path url) 0)))
            ((url-path url))
            "non-absolute path in url: `~a'" (url-path url))
    (write (url-path url) :stream stream))
  (when *print-escape* (write-string "\"" stream)))

(defcustom *url-special-chars* simple-string "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent-p (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
           (find char *url-special-chars* :test #'char=))))

(fmakunbound 'url)
(defgeneric url (xx)
  (:documentation "Convert the object into URL.
The argument can be:
   - a URL - returned untouched;
   - a string - it is non-destructively parsed;
   - a symbol - it is uninterned and its name is non-destructively parsed;
   - a stream - read from.")
  (:method ((xx url)) xx)
  (:method ((xx symbol)) (unintern xx) (url (symbol-name xx)))
  (:method ((xx stream))
           (url (with-output-to-string (st)
                  (loop :for zz :of-type character = (read-char xx)
                        :while (url-constituent-p zz)
                        :do (write zz :stream st)))))
  (:method ((xx pathname)) (make-url :prot :file :path (namestring xx))))
(declaim (ftype (function (t) url) url))
(defcustom *url-guess-protocol* list
  '(("www" . :http) ("web" . :http) ("w3" . :http)
    ("ftp" . :ftp) ("news" . :news) ("nntp" . :nntp))
  "*The alist of (\"string\" . protocol) to guess the protocol from the host.")
(defmethod url ((xx string))
  (let* ((string (string-trim +whitespace+ xx)) (url (make-url))
         (idx (position #\: string :test #'char=)) (start 0) idx0)
    (declare (simple-string string) (type index-t start) (type url url)
             (type (or null index-t) idx))
    (when (char= #\/ (char string 0))
      (return-from url
        (progn (setf (url-prot url) :file (url-path url) string) url)))
    (when idx
      (setf (url-prot url) (kwd (string-upcase (subseq string 0 idx))))
      (setq start (position #\/ string :start (1+ idx) :test #'char/=)))
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
    (unless (url-prot url)
      (cond ((let ((pa (assoc (url-host url) *url-guess-protocol* :test
                              (lambda (ho st)
                                (declare (simple-string ho st))
                                (string-equal ho st :end1 (length st))))))
               (when pa (setf (url-prot url) (cdr pa)))))
            ((position #\@ xx) (setf (url-prot url) :mailto))
            ((error "url: `~a': no protocol specified" xx))))
    url))

;;;
;;; }}}{{{ Sockets
;;;

#+allegro (deftype socket () 'excl::socket-stream)
#+cmu (deftype socket () 'system:fd-stream)
#+clisp (deftype socket () 'stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (simple-string host) (fixnum port) (type boolean bin))
  #+cmu (system:make-fd-stream (ext:connect-to-inet-socket host port)
                               :input t :output t)
  #+clisp (lisp:socket-connect port host :element-type
                               (if bin '(unsigned-byte 8) 'character))
  #+allegro (socket:make-socket :remote-host host :remote-port port
                                :format (if bin :binary :text)))

(defun open-socket-server (sock)
  "Open a `generic' socket server."
  (declare (ignorable sock) (type socket sock))
  #+clisp (lisp:socket-server sock)
  #+allegro (socket:make-socket :connect :passive))

(defun throw-timeout (&rest args)
  "Throw timeout."
  (apply #'format *error-output* args)
  (throw 'timeout nil))

(defcustom *url-default-sleep* (real 0) 20
  "*The number of seconds to sleep when necessary.")
(defcustom *url-default-timeout* (real 0) 86400
  "*The default timeout, in seconds.")

(defun sleep-mesg (sleep out mesg)
  "Sleep for a random period of up to SLEEP seconds.
Print the appropriate message MESG to OUT."
  (declare (type (or null stream) out) (real sleep))
  (let ((sleep (random sleep)))
    (when out
      (format out "~a; sleeping for ~d second~:p..." mesg sleep)
      (force-output out))
    (sleep sleep)
    (when out (format out "done~%"))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  (declare (ignorable seconds timeout-forms))
  #+allegro `(mp:with-timeout (,seconds ,@timeout-forms) ,@body)
  #+cmu
  `(block with-timeout
    (catch 'timer-interrupt
      (let* ((current-process mp:*current-process*)
             (timer-process (mp:make-process
                             (lambda ()
                               (sleep timeout)
                               (mp:process-interrupt
                                current-process
                                (lambda () (throw 'timer-interrupt nil))))
                             :name "Timeout timer")))
        (unwind-protect (return-from with-timeout (progn ,@body))
          (mp:destroy-process timer-process))))
    ,@timeout-forms)
  #+clisp `(progn ,@body))

(defun y-or-n-p-timeout (seconds default &rest args)
  "`y-or-n-p' with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

(defsubst url-prot-bin (prot)
  "Return T if the protocol is binary."
  (declare (symbol prot))
  (eq prot :time))

(defun open-socket-retry (host port &key (err *standard-output*) bin
                          (sleep *url-default-sleep*) max-retry
                          (timeout *url-default-timeout*))
  "Open a socket connection, retrying until success."
  (declare (simple-string host) (fixnum port) (type (or null stream) err)
           (type (or null index-t) max-retry) (type (real 0) sleep timeout))
  (loop :with begt = (get-universal-time) :for ii :upfrom 1 :and sock =
        (multiple-value-bind (sk cond)
            (ignore-errors
              (when err
                (format err "~&Connecting to ~a:~d [timeout ~:d sec]..."
                        host port timeout))
              (with-timeout (timeout
                             (values nil (format nil "timed out [~:d sec]"
                                                 timeout)))
                (open-socket host port bin)))
          (when (and err (null sk))
            (format err "~%Error connecting: ~a~%" cond))
          sk)
        :when (and err sock) :do (format err "done: ~a~%" sock)
        :when (and sock (open-stream-p sock)) :return sock
        :when (and max-retry (> ii max-retry)) :return nil
        :when (>= (- (get-universal-time) begt) timeout)
        :do (throw-timeout "open-socket-retry (~a:~d): timeout (~d sec)~%"
                           host port timeout)
        :do (sleep-mesg sleep err "Error")
        (format err "[~d~@[/~d~]] trying to connect to ~a:~d...~%"
                ii max-retry host port)))

(defun open-url (url &key (err *standard-output*) (sleep *url-default-sleep*)
                 (timeout *url-default-timeout*))
  "Open a socket connection to the URL.
Issue the appropriate initial commands:
 if this is an HTTP URL, also issue the GET command;
 if this is an FTP URL, login and cwd;
 if this is a NEWS/NNTP URL, set group and possibly request article;
 if this is a WHOIS/FINGER URL, ask about the host/user.
If timeout is non-nil, it specifies the number of seconds before
the tag `timeout' is thrown."
  (declare (type url url) (type (real 0) sleep timeout)
           (type (or null stream) err))
  (when (eq (url-prot url) :file)
    (return-from open-url (open (url-path url) :direction :input)))
  (loop :with begt = (get-universal-time)
        :for sock = (open-socket-retry (url-get-host url) (url-get-port url)
                                       :err err :sleep sleep :timeout timeout
                                       :bin (url-prot-bin (url-prot url)))
        :when
        (handler-case
            (with-timeout ((or timeout *url-default-timeout*) nil)
              (ecase (url-prot url)
                (:http (setq sock (url-open-http sock url err)))
                (:ftp (url-ask sock err 220)
                      (url-login-ftp sock url err))
                (:telnet (dolist (word (split-string (url-path url) "/") t)
                           (format sock "~a~%" word)))
                ((:whois :finger :cfinger)
                 (format sock "~a~%" (url-path-file url)) t)
                (:mailto (url-ask sock err 220))
                ((:news :nntp)
                 (url-ask sock err 200)
                 (url-ask sock err 211 "group ~a" (url-host url))
                 (unless (zerop (length (url-path url)))
                   (url-ask sock err 220 "article ~a"
                            (subseq (url-path url) 1)))
                 t)
                ((:time :daytime) t)))
          (error (co)
            (when err (format err "Connection to `~a' dropped: `~a'~%"
                              url co))))
        :return sock :when sock :do (close sock)
        :when (> (- (get-universal-time) begt) timeout)
        :do (throw-timeout "open-url (~a): timeout (~d sec)~%" url timeout)
        :do (sleep-mesg sleep err "Connection dropped")
        (format err "Trying to connect to `~a'...~%" url)))

(defmacro with-open-url ((socket url &key (rt '*readtable*) err
                                 (timeout '*url-default-timeout*))
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
`*readtable*' is temporarily set to RT (defaults to `*readtable*').
ERR is the stream for information messages or NIL for none."
  (let ((uuu (gensym "WOU")))
    `(let* ((,uuu (url ,url)) (*readtable* ,rt)
            (,socket (open-url ,uuu :err ,err :timeout ,timeout)))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn ,@body)
        (case (url-prot ,uuu)
          ((:ftp :mailto) (url-ask ,socket ,err 221 "quit"))
          ((:news :nntp) (url-ask ,socket ,err 205 "quit")))
        (close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (format sock "GET ~a HTTP/1.0~%~%" (url-path url))
  (do ((sk sock) (stat 302) sym res) ((not (eql stat 302)) sk)
    (declare (fixnum stat))
    (setq sym (read sk) stat (read sk))
    (when (string-equal sym "http/1.1")
      (when (>= stat 400) (error "~d: ~a~%" res (read-line sk))) ; error
      (when (= stat 302)        ; redirection
        (setq res (read-line sk)) (read-line sk) (read-line sk)
        (setq sym (read-line sk)
              sym (url (subseq sym (1+ (position #\: sym)))))
        (when (equal "" (url-host sym)) (setf (url-host sym) (url-host url)))
        (format err " *** redirected to `~a' [~a]~%" sym res)
        (when *html-verbose*
          (do (st) ((eq +eof+ (setq st (read-line sk nil +eof+))))
            (format t "~a~%" st)))
        (close sk) (setq sk (open-url sym :err err))
        (format sk "GET ~a HTTP/1.0~%~%" (url-path sym))))))

(defun url-ask (sock out end &rest req)
  "Send a request; read the response."
  (declare (type socket sock) (type (or null stream) out)
           (type (unsigned-byte 10) end))
  (when req
    (apply #'format sock req) (fresh-line sock)
    (when out (apply #'format out "~&url-ask[~d]: `~@?'~%" end req)))
  (loop :for ln :of-type simple-string = (read-line sock)
        :and code :of-type (unsigned-byte 10) = 0
        :when out :do (format out "~&url-ask[~d]: ~a~%" end
                              (setq ln (string-right-trim +whitespace+ ln)))
        :while (or (< (length ln) 3) (char/= #\Space (schar ln 3))
                   (progn (setq code (or (parse-integer
                                          ln :end 3 :junk-allowed t) 0))
                          (and (< code 400) (/= end code))))
        :finally (return (values ln code))))

(defun ftp-parse-sextuple (line)
  "Convert a0,a1,a2,a3,b0,b1 to HOST and PORT."
  (declare (simple-string line))
  (let* ((p0 (position #\) line :from-end t))
         (p1 (position #\, line :from-end t :end p0))
         (p2 (position #\, line :from-end t :end (1- p1))))
    (declare (type index-t p1 p2))
    (setf (schar line p1) #\Space (schar line p2) #\Space)
    (nsubstitute #\. #\, line)
    (values (subseq line (1+ (or (position #\( line :from-end t) -1)) p2)
            (+ (ash (parse-integer line :start p2 :end p1) 8)
               (parse-integer line :start p1 :end p0)))))

(defun ftp-get-passive-socket (sock out bin timeout)
  "Get a passive socket."
  (declare (type socket sock) (values socket))
  (loop :for sck =
        (multiple-value-bind (st cd) (url-ask sock out 227 "pasv")
          (when (>= cd 400)
            (throw-timeout "Cannot create data connection: ~a~%" st))
          (multiple-value-call #'open-socket-retry (ftp-parse-sextuple st)
                               :err out :max-retry 5 :bin bin
                               :timeout timeout))
        :when sck :return sck))

(defun url-login-ftp (sock url err)
  "Login and cd to the FTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (and (> 400 (nth-value 1 (url-ask sock err 331 "user ~a"
                                    (if (zerop (length (url-user url)))
                                        "anonymous" (url-user url)))))
       (url-ask sock err 230 "pass ~a" (if (zerop (length (url-pass url)))
                                           "ftp@ftp.net" (url-pass url)))
       (url-ask sock err 215 "syst")
       ;; (url-ask sock err 200 "type i")
       (url-ask sock err 211 "stat")
       (url-ask sock err 250 "cwd ~a" (url-path-dir url))))

(defcustom *buffer* (simple-array (unsigned-byte 8) (10240))
  (make-array 10240 :element-type '(unsigned-byte 8))
  "The download buffer - simple array of bytes.
The reasonable value for it's length is determined by your connection speed.
I recommend 10240 for 112kbps ISDN and 2048 for 28.8kbps, i.e.,
approximately, the number of bytes you can receive per second.")

(defun socket-to-file (data path &key rest (log *standard-output*))
  "Read from a binary socket to a file.
Read until the end, then close the socket."
  (declare (type socket data) (type pathname path) (type (or null stream) log))
  (with-open-file (fl path :direction :output :element-type 'unsigned-byte
                      :if-exists (if rest :append :supersede))
    ;; (break "entered `socket-to-file'")
    (loop :for pos :of-type index-t = (read-sequence *buffer* data)
          :do (write-sequence *buffer* fl :end pos)
          :sum pos :of-type file-size-t
          :when log :do (princ "." log) (force-output log)
          :while (= pos (length *buffer*))
          :finally (when log (terpri log)) :finally (close data))))

#+nil (progn

(url-ftp-get
 (url "ftp://contrib.redhat.com/libc6/i386/ctags-3.1.2-1.i386.rpm") "~/z/")
(map 'string #'code-char new-buf)
(coerce *buffer* 'simple-string)
(map-into buf #'code-char *buffer*)
(map-into buf #'code-char new-buf)
(map-into buf #'identity list-buf)
(map-into buf #'identity string-buf)
(map-into buf #'code-char list-buf)
(peek-char nil data)
(setq buf (make-string 10))
(setq list-buf (make-list 10240))
(setq string-buf (make-string 10240))
(setq new-buf (make-array 1024 :element-type '(unsigned-byte 8)))
(lisp:read-byte-sequence new-buf data)
(lisp:read-char-sequence buf data)
(lisp:read-byte-sequence list-buf data)
(lisp:read-char-sequence list-buf data)
(lisp:read-byte-sequence string-buf data)
(lisp:read-char-sequence string-buf data)
(setq data (open "/var/tmp/o/RedHat/RPMS/ctags-3.1.2-1.i386.rpm"
                 :direction :input :element-type 'unsigned-byte))
(close data)

)

(defun ftp-get-file (sock rmt loc &key (log *standard-output*) (reget t)
                     (bin t) (retry 2) (timeout *url-default-timeout*))
  "Get the remote file RMT from the FTP socket SOCK,
writing it into the local directory LOC.  Log to LOG.
Append if the file exists and REGET is non-nil.
Use binary mode if BIN is non-nil (default).
Retry (+ 1 RETRY) times if the file length doesn't match the expected."
  (declare (type socket sock) (type index-t retry) (type (or null stream) log)
           (simple-string rmt) (values file-size-t double-float simple-string))
  (let* ((data (ftp-get-passive-socket sock log t timeout)) (tot 0)
         (bt (get-float-time nil)) (path (merge-pathnames rmt loc))
         (rest (when (and reget (probe-file path))
                 (let ((sz (file-size path)))
                   (when log (format log "File `~a' exists (~:d bytes), ~
~:[appending~;overwriting~]...~%" path sz (zerop sz)))
                   (unless (zerop sz)
                     (url-ask sock log 350 "rest ~d" sz)
                     sz))))
         (line (progn (url-ask sock log 200 "type ~:[a~;i~]" bin)
                      (url-ask sock log 150 "retr ~a" rmt)))
         (pos (position #\( line :from-end t))
         (len (when pos (read-from-string line nil nil :start (1+ pos)))))
    (declare (type socket data) (type file-size-t tot) (type pathname path)
             (double-float bt) (type (or null file-size-t) rest len))
    ;; (when rest (decf len rest))
    (when log
      (if len (format log "Expect ~:d dot~:p for ~:d bytes~%"
                      (ceiling (1+ len) (length *buffer*)) len)
          (format log "File lenth unknown.~%")))
    (setq tot (socket-to-file data path :rest rest :log log))
    (url-ask sock log 226)
    (cond ((or (null len) (= tot len))
           (multiple-value-call #'values tot (elapsed bt nil t)))
          ((plusp retry)
           (when log
             (format log "### Wrong file length: ~:d (expected: ~:d [~@:d]) ###
 +++ ~r more attempt~:p +++~%" tot len (- tot len) retry))
           (ftp-get-file sock rmt loc :log log :reget nil :bin bin
                         :retry (1- retry) :timeout timeout))
          ((error "Wrong file length: ~:d (expected: ~:d [~@:d])"
                  tot len (- tot len))))))

(defun url-ftp-get (url loc &rest opts &key (log *standard-output*)
                    (timeout *url-default-timeout*) &allow-other-keys)
  "Get the file specified by the URL, writing it into a local file.
The local file is located in directory LOC and has the same name
as the remote one."
  (declare (type url url) (type (or null stream) log))
  (format t "~& *** getting `~a'...~%" url)
  (with-open-url (sock url :err log :timeout timeout)
    (multiple-value-bind (tot el st)
        (apply #'ftp-get-file sock (url-path-file url) loc opts)
      (format t " *** done [~:d bytes, ~a, ~:d bytes/sec]~%" tot st
              (round tot el)))))

(defun ftp-list (sock &key (out *standard-output*)
                 (timeout *url-default-timeout*))
  "Get the file list."
  (declare (type socket sock) (type (or null stream) out))
  (let ((data (ftp-get-passive-socket sock t nil timeout)))
    (url-ask sock out 150 "list")
    (loop :for line = (read-line data nil nil) :while line :when out :do
          (format out "~a~%" (string-right-trim +whitespace+ line)))
    (url-ask sock out 226)))

;;; Mail

(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance))) (subseq st 0 (position #\Space st)))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (getenv "USER") "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

(defun url-send-mail (url &key (out *standard-output*)
                      (text (current-time nil))
                      (helo *mail-host-address*)
                      (from *user-mail-address*))
  "Send TEXT to URL (which should be a MAILTO)."
  (declare (type url url) (type (or null stream) out)
           (simple-string text helo from))
  (assert (eq :mailto (url-prot url)) (url)
          "url-send-mail: `~a' is not a `mailto'" url)
  (with-open-url (sock url :err out)
    (url-ask sock out 250 "helo ~a" helo)
    (url-ask sock out 250 "mail from: ~a" from)
    (url-ask sock out 250 "rcpt to: ~a" (url-user url))
    (url-ask sock out 354 "data")
    (url-ask sock out 250 "~a~%." text)))

;;; News

(defstruct (article)
  (numb 0 :type (unsigned-byte 32)) ; article number
  (subj "" :type simple-string) ; subject
  (auth "" :type simple-string) ; author
  (dttm 0 :type (integer 0))    ; date/time
  (msid "" :type simple-string) ; message-ID
  (msid1 "" :type simple-string) ; ????
  (bytes 0 :type file-size-t)   ; size in bytes
  (lines 0 :type index-t)       ; size in lines
  (xref nil :type list))

(defmethod print-object ((art article) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~:[~;\"~]~d~c~a~c~a~c~a~c~a~c~d~c~d~cXref:~{ ~a~}~:[~;\"~]"
              *print-escape* (article-numb art) #\Tab (article-subj art) #\Tab
              (article-auth art) #\Tab (dttm->string (article-dttm art)) #\Tab
              (article-msid art) #\Tab (article-bytes art) #\Tab
              (article-lines art) #\Tab (article-xref art) *print-escape*)))

(defun string->article (string)
  "Parse the string as returned by `xover'."
  (declare (simple-string string))
  (multiple-value-bind (numb subj auth dttm msid msid1 bytes lines xref)
      (values-list (split-string string '(#\Tab) :strict t))
    (make-article :numb (parse-integer numb) :subj subj :auth auth
                  :dttm (string->dttm dttm) :msid msid :msid1 msid1
                  :bytes (parse-integer bytes) :lines (parse-integer lines)
                  :xref (cdr (split-string xref " ")))))

(defsubst read-trim (stream)
  "Read a line from stream and trim it."
  (declare (type stream stream) (values simple-string))
  (string-trim +whitespace+ (read-line stream nil ".")))

(defun url-dump-to-dot (sock &key (out *standard-output*) collect)
  "Read from SOCK until dot."
  (declare (type socket sock) (type (or string stream) out))
  (let ((str (if (streamp out) out
                 (open out :direction :output :if-exists :supersede))))
    (declare (stream str))
    (unwind-protect
         (loop :for st :of-type simple-string = (read-trim sock)
               :until (string= "." st) :do (write-string st str) (terpri str)
               :when collect :collect st)
      (unless (streamp out) (close str)))))

(defun url-get-news (url loc &key (out *standard-output*) re)
  "Get the news article to the OUT stream.
When RE is supplied, articles whose subject match it are retrieved."
  (declare (type url url) (stream out))
  (assert (or (eq :nntp (url-prot url)) (eq :news (url-prot url))) (url)
          "url-get-news: `~a' is not a `news'" url)
  (flet ((out (st) (if loc (merge-pathnames st loc) out)))
    (with-open-url (sock url :err out)
      (if (zerop (length (url-path url)))
          (multiple-value-bind (na a1 a2)
              (values-list
               (string-tokens (url-ask sock nil 211 "group ~a" (url-host url))
                              :start 3 :max 3))
            (let ((nm (format nil "~d-~d" a1 a2)))
              (format out "~:d articles, from ~:d to ~:d~%" na a1 a2)
              (url-ask sock out 224 "xover ~a" nm)
              (let ((ls (map-in #'string->article
                                (url-dump-to-dot sock :out (out nm)
                                                 :collect t))))
                (when re
                  (dolist (art ls)
                    (when (search re (article-subj art))
                      (url-ask sock out 220 "article ~d" (article-numb art))
                      (url-dump-to-dot sock :out (out (article-numb art))))))
                ls)))
          (url-dump-to-dot sock :out (out (url-path-file url)))))))

(defcustom *time-servers* list
  '("clock.psu.edu" "black-ice.cc.vt.edu" "clock1.unc.edu" "ntp0.cornell.edu"
    "clock-1.cs.cmu.edu" "time-b.timefreq.bldrdoc.gov" "time-b.nist.gov")
  "Public NTP servers (secondary).
For additional servers see http://www.eecis.udel.edu/~mills/ntp/servers.htm")

(defun url-time (&optional (url *time-servers*) (out *standard-output*))
  "Get the time out of the date/time url."
  (declare (stream out))
  (etypecase url
    (string (multiple-value-call #'values
              (url-time (make-url :prot :time :host url))
              (url-time (make-url :prot :daytime :host url))))
    (sequence
     (map 'list (lambda (uu)
                  (format out "~&~a:" uu) (force-output out)
                  (let ((val (multiple-value-list (url-time uu))))
                    (format out "~{~30t[~a -- ~a]~%~}" val)
                    val))
          url))
    (url (with-open-url (sock url)
           (ecase (url-prot url)
             (:time
              (let ((nn (+ (ash (read-byte sock) 24) (ash (read-byte sock) 16)
                           (ash (read-byte sock) 8) (read-byte sock))))
                (values nn (dttm->string nn))))
             (:daytime
              (let ((str (read-line sock)))
                (if (zerop (length str)) (values)
                    (values (string->dttm (copy-seq str)) str)))))))))

;;;
;;; }}}{{{ HTML parsing
;;;

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping junk."
  (sock nil)                    ; socket to read from
  (buff "" :type simple-string) ; buffer string
  (posn 0 :type fixnum))        ; position in the buffer

(defcustom *ts-kill* list nil "*The list of extra characters to kill.")

(defun ts-pull-next (ts &optional (concat-p t) (kill *ts-kill*))
  "Read the next line from the socket, put it into the buffer.
If CONCAT-P is non-NIL, the new line is appended,
otherwise the buffer is replaced.
Return the new buffer or NIL on EOF."
  (declare (type text-stream ts))
  (let ((str (or (read-line (ts-sock ts) nil nil)
                 (return-from ts-pull-next nil))))
    (declare (type simple-string str))
    (when kill
      (dolist (ch (to-list kill))
        (setq str (nsubstitute #\Space ch str))))
    ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
    (do ((beg -1) (len (1- (length str))))
        ((or (= beg len)
             (null (setq beg (position #\. str :start (1+ beg))))))
      (declare (type (signed-byte 21) beg len))
      (if (or (and (plusp beg) (alphanumericp (schar str (1- beg))))
              (and (< beg len) (alphanumericp (schar str (1+ beg)))))
          (incf beg) (setf (schar str beg) #\Space)))
    (if concat-p
        (setf (ts-buff ts) (concatenate 'string (ts-buff ts) str))
        (setf (ts-posn ts) 0 (ts-buff ts) str))))

(defun read-next (ts &key errorp (kill *ts-kill*) skip)
  "Read the next something from TS - a text stream."
  (declare (type text-stream ts) (type (or null function) skip))
  (loop :with tok :and pos
        :when (and (or (typep pos 'error)
                       (>= (ts-posn ts) (length (ts-buff ts))))
                   (null (ts-pull-next ts (typep pos 'error) kill)))
        :do (if (typep pos 'error) (error pos)
                (if errorp (error "EOF on ~a" ts)
                    (return-from read-next +eof+)))
        :do (setf (values tok pos)
                  (ignore-errors (read-from-string (ts-buff ts) nil +eof+
                                                   :start (ts-posn ts))))
        :unless (typep pos 'error) :do (setf (ts-posn ts) pos)
        :unless (or (typep pos 'error) (eq tok +eof+))
        :return (if (and skip (funcall skip tok))
                    (read-next ts :errorp errorp :kill kill :skip skip)
                    tok)))

(defun next-token (ts &key (num 1) type dflt (kill *ts-kill*))
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num (if (and type (not (typep tt type))) dflt tt))
      (declare (type index-t ii))
      (do () ((not (html-tag-p (setq tt (read-next ts :errorp t :kill kill))))
              (mesg :log t "token: ~a~%" tt))
        (mesg :log t "tag: ~a~%" tt)))))

(defun next-number (ts &key (num 1) (kill *ts-kill*))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num tt)
      (declare (type index-t ii))
      (do () ((numberp (setq tt (next-token ts :kill kill))))))))

(defun skip-tokens (ts end &key (test #'eql) (key #'identity) kill)
  "Skip tokens until END, i.e., until (test (key token) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts :kill kill)))
                     end)
            tt)))

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

(defcustom *browsers* list
  '((netscape "/usr/bin/netscape" "-remote" "openURL(~a)")
    (emacs-w3 "/usr/bin/gnudoit" "(w3-fetch \"~a\")"))
  "The ALIST of browsers.")

(defun view-url (url &optional (bro 'netscape))
  "Lounch a browser to view a url."
  (declare (type url url))
  (let ((br (copy-list (assoc bro *browsers* :test #'eq))) pos)
    (assert (consp br) (br) "Unknown browser ~a. Must be one of~?." bro
            (list-format "~a") (mapcar #'car *browsers*))
    (setq pos (1+ (position "~a" (cdr br) :test #'search)))
    (setf (nth pos br) (format nil (nth pos br) url))
    (run-prog (second br) :args (cddr br))
    (format t "launched ~a with args ~a~%" (car br) (cddr br))))

(defun dump-url (url &key (fmt "~3d: ~a~%") (out *standard-output*)
                 (timeout *url-default-timeout*) (proc #'identity))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to `*STANDARD-OUTPUT*'."
  (declare (type url url) (stream out) (function identity))
  (format out "Opening URL: `~a'...~%" url)
  (catch 'timeout
    (with-open-url (sock url :err out :timeout timeout)
      (loop :for ii :of-type index-t :from  1
            :and rr = (read-line sock nil +eof+) :until (eq +eof+ rr)
            :do (format out fmt ii
                        (funcall proc (string-right-trim +whitespace+ rr)))))))

(defun url-get (url loc &key (timeout *url-default-timeout*)
                (log *standard-output*))
  "Get the URL."
  (declare (type url url) (stream log))
  (ecase (url-prot url)
    (:ftp (url-ftp-get url loc :timeout timeout :log log))
    ((:nntp :news) (url-get-news url loc :log log))
    (:http
     (let* ((path (merge-pathnames (url-path-file url) loc))
            (bt (get-float-time nil))
            (size (with-open-url (sock url :err log :timeout timeout)
                    (socket-to-file sock path :log log))))
       (declare (type file-size-t size))
       (multiple-value-bind (el st) (elapsed bt nil t)
         (declare (double-float el))
         (format log "Wrote `~a' [~:d bytes, ~a, ~:d bytes/sec]."
                 path size st (round size el)))))))

(defun whois (host &rest keys)
  "Get the whois information on a host."
  (apply #'dump-url (make-url :prot :whois :host "rs.internic.net"
                              :path (concatenate 'string "/" (string host)))
         :fmt "~*~a~%" keys))

(defun finger (address &rest keys &key gnu &allow-other-keys)
  "Finger the mail address."
  (let* ((str-address (string address))
         (pos (position #\@ str-address :test #'char=)))
    (declare (simple-string str-address) (type (unsigned-byte 10) pos))
    (remf keys :gnu)
    (apply #'dump-url
           (make-url :prot (if gnu :cfinger :finger)
                     :host (subseq str-address (1+ pos))
                     :path (concatenate 'string "/"
                                        (subseq str-address 0 pos)))
           :fmt "~*~a~%" keys)))

(defun dump-url-tokens (url &key (fmt "~3d: ~a~%") (out *standard-output*))
  "Dump the URL token by token.
See `dump-url' about the optional parameters."
  (declare (stream out))
  (setq url (url url))
  (with-open-url (sock url :rt *html-readtable* :err *standard-output*)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
        ((eq +eof+ (setq rr (read-next ts))))
      (declare (type index-t ii))
      (format out fmt ii rr))))

(provide "url")
;;; url.lisp ends here
