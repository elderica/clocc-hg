;;; File: <url.lisp - 2000-01-24 Mon 19:01:12 EST sds@ksp.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; Copyright (C) 1998-1999 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id$
;;; $Source$
;;; $Log$
;;; Revision 1.33  2000/01/19 18:08:47  sds
;;; (resolve-host-ipaddr): fixed for CLISP/syscalls
;;;
;;; Revision 1.32  1999/10/19 18:47:57  sds
;;; (ts-skip-scripts): new function.
;;;
;;; Revision 1.31  1999/10/12  15:44:04  sds
;;; (socket-service-port): corrected `services' path under win32.
;;; (url-ask): use `*url-replies*'.
;;; (url-login-ftp): fixed the error message.
;;; (html-stream): new Gray stream.
;;; (next-token, next-number): more verbose.
;;;
;;; Revision 1.30  1999/06/03 20:41:11  sds
;;; (+bad-url+): new constant.
;;; (*rfc-base*): new variable.
;;; (protocol-rfc): renamed from `url-rfc'.
;;; (socket-server &c): support CMUCL & LispWorks.
;;; (*url-replies*): new variable.
;;; (url-ask): use it; `END' can be symbolic now.
;;; (cl-server): nascent CL server stuff (ripped from CMUCL).
;;;
;;; Revision 1.29  1999/05/05 21:04:22  sds
;;; LispWorks compatibility:
;;; (open-socket, resolve-host-ipaddr, open-socket-server):
;;; work with LispWorks; added the `not-implemented' condition.
;;; (socket-host, socket-port): added the `not-implemented' condition.
;;; (code, case-error): moved to base.lisp.
;;;
;;; Revision 1.28  1999/05/03 18:20:40  sds
;;; (login): new `network' condition.
;;; (open-socket-retry): return a socket or signal an error.
;;; (open-url): simplified.
;;; (url-ask): signal an error.
;;; (ftp-get-passive-socket): removed the loop.
;;; (*ftp-anonymous-passwords*): new variable.
;;; (url-login-ftp): use it; signal a `login' error on falure.
;;;
;;; Revision 1.27  1999/04/26 17:18:10  sds
;;; (with-open-html): moved `meta' to `head'; added `link'.
;;;
;;; Revision 1.26  1999/04/20 16:21:08  sds
;;; (url string): handle URL with cgi having confusing arguments.
;;;
;;; Revision 1.25  1999/04/19 23:40:28  sds
;;; (url-time): print the time difference.
;;; (open-url, url-get): fixed the call to (error 'code).
;;;
;;; Revision 1.24  1999/04/19 15:56:12  sds
;;; (*url-default-max-retry*): new user variable, the
;;; default for the `max-retry' key.
;;;
;;; Revision 1.23  1999/04/16 16:01:18  sds
;;; (with-tag): added `value' key; better default for `terpri'.
;;; (with-open-html): added `head', `comment' and `footer' keys;
;;; fixed `doctype' key.
;;; (directory-index): added `&rest opts' for comment.
;;; use `value' key when calling `with-tag'.
;;;
;;; Revision 1.22  1999/04/11 19:58:00  sds
;;; Added `*html-output*' and `with-tag'.
;;; (with-open-html): bind `*html-output*'.  use `with-tag'.
;;; (directory-index): use `with-tag'.
;;;
;;; Revision 1.21  1999/04/06 21:57:33  sds
;;; Added `directory-index' and `with-open-html'.
;;;
;;; Revision 1.20  1999/03/24 17:01:54  sds
;;; More `*html-specials*'.  Added `url-rfc'.
;;; `socket-service-port' returns 4 values now.
;;; Added `*url-bytes-transferred*', `*url-opening-time*' and `url-eta'.
;;; Added two new keyword arguments to `ftp-list' - :name and :log.
;;;
;;; Revision 1.19  1999/02/09 23:19:58  sds
;;; Removed `throw-timeout'.
;;; Added `socket-host', `socket-port' and a condition `timeout'.
;;;
;;; Revision 1.18  1999/02/08 20:45:37  sds
;;; Updated for cmucl 18b.
;;;
;;; Revision 1.17  1999/02/02 00:02:33  sds
;;; Moved `html-translate-specials' &c to clhs.lisp
;;; Expanded `url-time'.
;;;
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
  #+lispworks (require "comm")
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

;; ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MISC/SGML.TXT
(defcustom *html-specials* list
  '(("gt" . #\>) ("lt" . #\<) ("quot" . #\") ("amp" . #\&) ("nbsp" . #\Space)
    ("acute" . #\') ("ast" . #\*) ("colon" . #\:) ("comma" . #\,)
    ("commat" . #\@) ("copy" . "(C)") ("curren" . #\$) ("divide" . #\/)
    ("dollar" . #\$) ("equals" . #\=) ("excl" . #\!) ("grave" . #\`)
    ("half" . "1/2") ("hyphen" . #\-) ("lowbar" . #\_) ("lpar" . #\()
    ("rpar" . #\)) ("lsqb" . #\[) ("rsqb" . #\]) ("num" . #\#) ("period" . #\.)
    ("plus" . #\+) ("plusmn" . "+-") ("pound" . #\#) ("quest" . #\?)
    ("laquo" . "<<") ("raquo" . ">>") ("lcub" . #\{) ("rcub" . #\})
    ("semi" . #\;) ("shy" . #\-) ("times" . #\*) ("verbar" . #\|))
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
              (or (and *html-parse-tags*
                       (cdr (assoc (coerce (nreverse rr) 'string)
                                   *html-specials* :test #'string-equal)))
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

(eval-when (load compile eval)  ; acl compile warning
(defstruct (url #+cmu (:print-function print-struct-object))
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot nil :type symbol)       ; protocol
  (user "" :type simple-string) ; username
  (pass "" :type simple-string) ; password
  (host "" :type simple-string) ; hostname
  (port 0  :type fixnum)        ; port number
  (path "" :type simple-string)) ; pathname
)

(defconst +bad-url+ url (make-url) "*The convenient constant for init.")

(defcustom *rfc-base* (or null string)
  "http://www.cis.ohio-state.edu/htbin/rfc/rfc~d.html"
  "*The format string used to generate the URL in `protocol-rfc'.
When NIL, just the RFC numbers are returned.")

(defun protocol-rfc (protocol)
  "Return the RFC url for the given protocol.
See <http://www.cis.ohio-state.edu/hypertext/information/rfc.html>
<http://www.internic.net/wp>, <ftp://ds.internic.net/rfc> and `*rfc-base*'."
  (let* ((prot (typecase protocol
                 (symbol (if (keywordp protocol) protocol (kwd protocol)))
                 (string (kwd protocol)) (url (url-prot protocol))
                 (t (error 'case-error :proc 'protocol-rfc :args
                           (list 'protocol protocol 'symbol 'string 'url)))))
         (rfcs (case prot
                 ((:http :www) '(1945 2068))
                 (:ftp '(959))
                 ((:smtp :mailto) '(821))
                 (:telnet '(1205))
                 (:whois '(954 2167))
                 (:finger '(1288))
                 (:time '(1305))
                 ((:nntp :news) '(977))
                 (t (error 'code :proc 'protocol-rfc :args (list prot)
                           :mesg "Cannot handle protocol ~s")))))
    (maplist (lambda (cc)
               (setf (car cc)
                     (if *rfc-base*
                         (url (format nil *rfc-base* (car cc))) (car cc))))
             rfcs)))

(defun socket-service-port (&optional service (protocol "tcp"))
  "Return the port number of the SERVICE."
  ;; #+clisp (lisp:socket-service-port service protocol)
  ;; #-clisp
  (flet ((parse (str)
           (let ((tok (string-tokens
                       (nsubstitute
                        #\Space #\/ (subseq str 0 (or (position #\# str)
                                                      (length str)))))))
             (values (string-downcase (string (first tok)))
                     (mapcar (compose string-downcase string) (cdddr tok))
                     (second tok)
                     (string-downcase (string (third tok)))))))
    (with-open-file (fl #+unix "/etc/services" #+win32
                        (concatenate 'string (getenv "windir")
                                     "/system32/drivers/etc/services")
                        :direction :input)
      (loop :with name :and alis :and port :and prot
            :for st = (read-line fl nil +eof+)
            :until (eq st +eof+)
            :unless (or (equal "" st) (char= #\# (schar st 0)))
              :do (setf (values name alis port prot) (parse st)) :and
              :if service
                :when (and (string-equal protocol prot)
                           (or (string-equal service name)
                               (member service alis :test #'string-equal)))
                  :return (values name alis port prot) :end
                :else :collect (vector name alis port prot) :end :end
            :finally (when service
                       (error "service ~s is not found for protocol ~s"
                              service protocol))))))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (declare (type url url))
  (if (zerop (url-port url))
      (flet ((ssp (st) (ignore-errors (nth-value 2 (socket-service-port st)))))
        (or (ssp (string-downcase (string (url-prot url))))
            (ssp (case (url-prot url)
                   (:mailto "smtp") (:news "nntp") (:www "http")))
            (error 'code :proc 'url-get-port :args (list url)
                   :mesg "Cannot guess the port for ~s")))
      (url-port url)))

(defcustom *nntpserver* simple-string
  (or (getenv "NNTPSERVER") "localhost")
  ;; (setq *nntpserver* "news0-alterdial.uu.net")
  "*The NNTP server to be user for `news' URLs.")

(defun url-get-host (url)
  "Get the right host for the URL: if it is a `news', use `*nntpserver*'."
  (declare (type url url))
  (if (plusp (length (url-host url))) (url-host url)
      (case (url-prot url) ((:news :nntp) *nntpserver*))))

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

(defmethod print-object ((url url) (out stream))
  "Print the URL in the standard form."
  (when *print-readably* (return-from print-object (call-next-method)))
  (when *print-escape* (write-string "\"" out))
  (let ((*print-escape* nil))
    (write (url-prot url) :stream out :case :downcase)
    (write-string ":" out)
    (unless (or (eq :mailto (url-prot url)) (eq :file (url-prot url))
                (zerop (length (url-host url))))
      (write-string "//" out))
    (unless (zerop (length (url-user url)))
      (write (url-user url) :stream out)
      (unless (zerop (length (url-pass url)))
        (write-string "#" out) (write (url-pass url) :stream out))
      (write-string "@" out))
    (unless (zerop (length (url-host url)))
      (write (url-host url) :stream out)
      (unless (zerop (url-port url))
        (write-string ":" out) (write (url-port url) :stream out)))
    (assert (or (zerop (length (url-path url)))
                (eq (url-prot url) :news) (eq (url-prot url) :nntp)
                (char= #\/ (aref (url-path url) 0)))
            ((url-path url))
            "non-absolute path in url: `~a'" (url-path url))
    (when (and (not (zerop (length (url-host url))))
               (or (eq (url-prot url) :news) (eq (url-prot url) :nntp))
               (not (zerop (length (url-path url))))
               (not (char= #\/ (aref (url-path url) 0))))
      (write-string "/" out))
    (write-string (url-path url) out))
  (when *print-escape* (write-string "\"" out)))

(defcustom *url-special-chars* simple-string "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent-p (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
           (find char *url-special-chars* :test #'char=))))

(eval-when (load compile eval) (fmakunbound 'url))
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
           (peek-char t xx) ; skip whitespace
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
  (let* ((string (string-trim +whitespace+ xx)) (url (make-url)) slashp
         (idx (position #\: string :test #'char=)) (start 0) idx0
         (end (position #\? string :test #'char=)))
    (declare (simple-string string) (type index-t start) (type url url)
             (type (or null index-t) idx))
    (when (char= #\/ (char string 0))
      (return-from url
        (progn (setf (url-prot url) :file (url-path url) string) url)))
    (when idx
      (setf (url-prot url) (kwd (nstring-upcase (subseq string 0 idx))))
      (setq start (position #\/ string :start (1+ idx) :test #'char/= :end end)
            slashp (/= (1+ idx) start)))
    (setq idx (position #\@ string :start start :test #'char= :end end))
    (when idx
      (setq idx0 (position #\# string :start start :test #'char= :end end))
      (if idx0 (setf (url-pass url) (subseq string (1+ idx0) idx)
                     (url-user url) (subseq string start idx0))
          (setf (url-user url) (subseq string start idx)))
      (setq start (1+ idx)))
    (setq idx (position #\: string :start start :test #'char= :end end))
    (setq idx0 (position #\/ string :start start :test #'char= :end end))
    (when idx
      (setf (url-port url) (parse-integer string :start (1+ idx) :end idx0)))
    (when idx0
      (setf (url-path url) (subseq string idx0)))
    (if (and (not slashp)
             (or (eq (url-prot url) :nntp) (eq (url-prot url) :news)))
        (setf (url-path url)
              (concatenate 'string (subseq string start (or idx idx0))
                           (url-path url)))
        (setf (url-host url) (subseq string start (or idx idx0))))
    (unless (url-prot url)
      (cond ((let ((pa (assoc (url-host url) *url-guess-protocol* :test
                              (lambda (ho st)
                                (declare (simple-string ho st))
                                (string-beg-with st ho)))))
               (when pa (setf (url-prot url) (cdr pa)))))
            ((position #\@ string :test #'char= :end end)
             (setf (url-prot url) :mailto))
            ((error "url: `~a': no protocol specified" string))))
    url))

;;;
;;; name resulution
;;;

(defun resolve-host-ipaddr (host)
  "Call gethostbyname(3) or gethostbyaddr()."
  #+allegro
  (etypecase host
    (string
     (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch))) host)
         (resolve-host-ipaddr (dotted-to-ipaddr host))
         (values host nil (socket:lookup-hostname host) 2)))
    (integer (values (socket:ipaddr-to-hostname host) nil
                     (ipaddr-to-dotted host) 2)))
  #+(and clisp syscalls)
  (let ((he (posix:resolve-host-ipaddr host)))
    (values (posix::hostent-name he) (posix::hostent-aliases he)
            (posix::hostent-addr-list he) (posix::hostent-addrtype he)))
  #+cmu (let ((he (ext:lookup-host-entry host)))
          (values (ext:host-entry-name he)
                  (ext:host-entry-aliases he)
                  (mapcar #'ipaddr-to-dotted (ext:host-entry-addr-list he))
                  (ext::host-entry-addr-type he)))
  #+lispworks
  (let ((he (fli:dereference (comm::gethostbyname host))))
    (values (fli:convert-from-foreign-string
             (fli:foreign-slot-value he 'comm::h_name))
            (loop :with pp = (fli:foreign-slot-value he 'comm::h_aliases)
                  :for cp = (fli:dereference pp)
                  :until (fli:null-pointer-p cp)
                  :collect (fli:convert-from-foreign-string cp)
                  :do (fli:incf-pointer pp))
            (loop :with pp = (fli:foreign-slot-value he 'comm::h_addr_list)
                  :for cp = (fli:dereference pp :type '(:unsigned :long))
                  :until (zerop cp) ; broken !!!
                  :collect (ipaddr-to-dotted cp)
                  :do (fli:incf-pointer pp))
            (fli:foreign-slot-value he 'comm::h_addrtype)
            (fli:foreign-slot-value he 'comm::h_length)))
  ;; #+gcl
  #-(or allegro cmu lispworks (and clisp syscalls))
  (error 'not-implemented :proc (list 'resolve-host-ipaddr host)))

(defun ipaddr-to-dotted (ipaddr)
  "Number --> string."
  (declare (type (unsigned-byte 32) ipaddr) (values simple-string))
  #+allegro (socket:ipaddr-to-dotted ipaddr)
  #-allegro
  (format nil "~d.~d.~d.~d"
          (logand #xff (ash ipaddr -24)) (logand #xff (ash ipaddr -16))
          (logand #xff (ash ipaddr -8)) (logand #xff ipaddr)))

(defun dotted-to-ipaddr (dotted)
  "String --> number."
  (declare (string dotted) (values (unsigned-byte 32)))
  #+allegro (socket:dotted-to-ipaddr dotted)
  #-allegro
  (let ((ll (string-tokens (substitute #\Space #\. dotted))))
    (+ (ash (first ll) 24) (ash (second ll) 16)
       (ash (third ll) 8) (fourth ll))))

;;;
;;; }}}{{{ Sockets
;;;

(deftype socket ()
  #+allegro 'excl::socket-stream
  #+clisp 'stream
  #+cmu 'system:fd-stream
  #+lispworks 'comm:socket-stream
  #-(or allegro clisp cmucl lispworks) 'stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host) (integer (resolve-host-ipaddr host)))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (lisp:socket-connect port host :element-type
                                 (if bin '(unsigned-byte 8) 'character))
    #+cmu (system:make-fd-stream (ext:connect-to-inet-socket host port)
                                 :input t :output t :element-type
                                 (if bin '(unsigned-byte 8) 'character))
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    ;; #+gcl
    #-(or allegro clisp cmu lispworks)
    (error 'not-implemented :proc (list 'open-socket host port bin))))

(defun socket-host (sock)
  "Return the remote host name."
  (declare (type socket sock))
  #+clisp (lisp:socket-stream-host sock)
  #+allegro (socket:ipaddr-to-dotted (socket:remote-host sock))
  #+cmu (ext::gethostbyaddr (ext:get-socket-host-and-port sock))
  ;; #+gcl #+lispworks
  #-(or allegro clisp cmu)
  (error 'not-implemented :proc (list 'socket-host sock)))

(defun socket-port (sock)
  "Return the remote port number."
  (declare (type socket sock))
  #+clisp (lisp:socket-stream-port sock)
  #+allegro (socket:remote-port sock)
  #+cmu (nth-value 1 (ext:get-socket-host-and-port sock))
  ;; #+gcl #+lispworks
  #-(or allegro clisp cmu)
  (error 'not-implemented :proc (list 'socket-port sock)))

(deftype socket-server ()
  #+allegro 'acl-socket::socket-stream-internet-passive
  #+clisp 'lisp:socket-server
  #+cmu 'integer
  #+lispworks 'comm:socket-stream ; FIXME
  #-(or allegro clisp cmucl lispworks) t)

(defun socket-accept (serv &optional bin)
  "Accept a connection on a socket server (passive socket)."
  (declare (type socket-server serv) (values socket))
  #+allegro (socket:accept-connection serv)
  #+clisp (lisp:socket-accept serv :element-type
                              (if bin '(unsigned-byte 8) 'character))
  #+cmu (progn
          (mp:process-wait-until-fd-usable serv :input)
          (system:make-fd-stream (ext:accept-tcp-connection serv)
                                 :input t :output t :element-type
                                 (if bin '(unsigned-byte 8) 'character)))
  #+lispworks (comm:-something serv) ; FIXME
  ;; #+gcl
  #-(or allegro clisp cmu lispworks)
  (error 'not-implemented :proc (list 'socket-accept serv bin)))

(defun open-socket-server (&optional port)
  "Open a `generic' socket server."
  (declare (type (or null integer socket) port) (values socket-server))
  #+allegro (socket:make-socket :connect :passive :local-port
                                (when (integerp port) port))
  #+clisp (lisp:socket-server port)
  #+cmu (ext:create-inet-listener port)
  #+lispworks (comm:start-up-server port) ; FIXME
  ;; #+gcl
  #-(or allegro clisp cmu lispworks)
  (error 'not-implemented :proc (list 'open-socket-server port)))

(defun socket-server-close (server)
  "Close the server."
  (declare (type socket-server server))
  #+clisp (lisp:socket-server-close server)
  #+cmu (unix:unix-close server)
  #-(or clisp cmu) (close server))

(define-condition network (error)
  ((proc :type symbol :reader net-proc :initarg :proc)
   (host :type simple-string :reader net-host :initarg :host)
   (port :type (unsigned-byte 16) :reader net-port :initarg :port)
   (mesg :type simple-string :reader net-mesg :initarg :mesg)
   (args :type list :reader net-args :initarg :args))
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s] ~s:~d~@[ ~?~]"
                     (net-proc cc) (net-host cc) (net-port cc)
                     (and (slot-boundp cc 'mesg) (net-mesg cc))
                     (and (slot-boundp cc 'args) (net-args cc))))))

(define-condition timeout (network)
  ((time :type (real 0) :reader timeout-time :initarg :time))
  (:report (lambda (cc out)
             (declare (stream out))
             (call-next-method)
             (when (slot-boundp cc 'time)
               (format out " [timeout ~a sec]" (timeout-time cc))))))

(define-condition login (network) ())

;; (defun upgrade (obj class)
;;   (let ((slots (nintersection (class-slot-list obj nil)
;;                               (class-slot-list class nil) :test #'eq))
;;         (nobj (make-instance class)))
;;     (dolist (sl slots nobj)
;;       (when (slot-boundp obj sl)
;;         (setf (slot-value nobj sl) (slot-value obj sl))))))

(defcustom *url-default-sleep* (real 0) 30
  "*The number of seconds to sleep when necessary.")
(defcustom *url-default-timeout* (real 0) 86400
  "*The default timeout, in seconds.")
(defcustom *url-default-max-retry* (or null index-t) nil
  "*The default value of max-retry.
If nil, retry ad infinitum, otherwise a positive fixnum.")

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
  #+(or allegro cmu) `(mp:with-timeout (,seconds ,@timeout-forms) ,@body)
  #-(or mp multiprocessing) `(progn ,@body))

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
                          (sleep *url-default-sleep*)
                          (max-retry *url-default-max-retry*)
                          (timeout *url-default-timeout*))
  "Open a socket connection, retrying until success."
  (declare (simple-string host) (fixnum port) (type (or null stream) err)
           (type (or null index-t) max-retry) (type (real 0) sleep timeout)
           (values socket))
  (loop :with begt = (get-universal-time) :and err-cond
        :for ii :of-type index-t :upfrom 1
        :for sock :of-type (or null socket) =
        (handler-case
            (progn
              (mesg :log err
                    "~&[~d~@[/~d~]] Connecting to ~a:~d [timeout ~:d sec]..."
                    ii max-retry host port timeout)
              (with-timeout (timeout
                             (error 'timeout :proc 'open-socket-retry :host
                                    host :port port :time timeout))
                (open-socket host port bin)))
          (error (co)
            (setq err-cond co)
            (mesg :log err "~%Error connecting: ~a~%" co)))
        :when sock :do (mesg :log err "done:~% [~a]~%" sock)
        :when (and sock (open-stream-p sock)) :return sock
        :when (and max-retry (>= ii max-retry))
        :do (error 'network :proc 'open-socket-retry :host host :port port
                   :mesg "max-retry [~a] exceeded~@[~% - last error: ~a~]"
                   :args (list max-retry err-cond))
        :when (>= (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-socket-retry :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-socket-retry] Error")))

(defun open-url (url &key (err *error-output*) (sleep *url-default-sleep*)
                 (timeout *url-default-timeout*)
                 (max-retry *url-default-max-retry*))
  "Open a socket connection to the URL.
Issue the appropriate initial commands:
 if this is an HTTP URL, also issue the GET command;
 if this is an FTP URL, login and cwd;
 if this is a NEWS/NNTP URL, set group and possibly request article;
 if this is a WHOIS/FINGER URL, ask about the host/user.
If timeout is non-nil, it specifies the number of seconds before
the error `timeout' is signaled."
  (declare (type url url) (type (real 0) sleep timeout)
           (type (or null stream) err))
  (when (eq (url-prot url) :file)
    (return-from open-url (open (url-path url) :direction :input)))
  (loop :with begt = (get-universal-time) :and host = (url-get-host url)
        :and port = (url-get-port url)
        :for sock :of-type socket =
        (open-socket-retry host port :err err :sleep sleep :timeout timeout
                           :bin (url-prot-bin (url-prot url))
                           :max-retry max-retry)
        :when
        (handler-case
            (with-timeout (timeout nil)
              (case (url-prot url)
                ((:http :www) (setq sock (url-open-http sock url err)))
                (:ftp (url-ask sock err :conn)
                      (url-login-ftp sock url err))
                (:telnet (dolist (word (split-string (url-path url) "/") t)
                           (format sock "~a~%" word)))
                ((:whois :finger :cfinger)
                 (format sock "~a~%" (url-path-file url)) t)
                (:mailto (url-ask sock err :conn))
                ((:news :nntp)
                 (url-ask sock err :noop)
                 (unless (zerop (length (url-path url)))
                   (let ((strs (split-string (url-path url) "/")))
                     (url-ask sock err :group "group ~a" (car strs))
                     (when (cadr strs)
                       (url-ask sock err :article "article ~a" (cadr strs)))))
                 t)
                ((:time :daytime) t)
                (t (error 'code :proc 'open-url :args (list (url-prot url))
                          :mesg "Cannot handle protocol ~s"))))
          (code (co) (error co))
          (login (co) (error co))
          (error (co)
            (mesg :err err "Connection to <~a> dropped:~% - ~a~%" url co)))
        :return sock
        :when sock :do (close sock)
        :when (> (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-url :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-url] Connection dropped")))

(defcustom *url-bytes-transferred* integer 0
  "The number of bytes transferred during the current connection.")
(makunbound '*url-bytes-transferred*)
(defcustom *url-opening-time* double-float 0.0
  "The time when the current connection was open.")
(makunbound '*url-opening-time*)

(defmacro with-open-url ((socket url &key (rt '*readtable*) err
                                 (max-retry '*url-default-max-retry*)
                                 (timeout '*url-default-timeout*))
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
`*readtable*' is temporarily set to RT (defaults to `*readtable*').
ERR is the stream for information messages or NIL for none."
  (with-gensyms ("WOU-" uuu)
    `(let* ((,uuu (url ,url)) (*readtable* ,rt)
            (,socket (open-url ,uuu :err ,err :timeout ,timeout
                               :max-retry ,max-retry))
            (*url-opening-time* (get-float-time nil))
            (*url-bytes-transferred* 0))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn ,@body)
        (case (url-prot ,uuu)
          ((:ftp :mailto) (url-ask ,socket ,err :quit "quit"))
          ((:news :nntp) (url-ask ,socket ,err :nntp-quit "quit")))
        (close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (format sock "GET ~a HTTP/1.0~2%" (url-path url))
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
        (format sk "GET ~a HTTP/1.0~2%" (url-path sym))))))

(defcustom *url-replies* hash-table
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (cc '(((:user) 331 332) ((:pass) 230 332) ((:acct) 230 202)
                  ((:syst) 215) ((:stat) 211 212 213) ((:abor) 225 226)
                  ((:cwd :rnto :dele :rmd) 250) ((:mkd :pwd) 257)
                  ((:cdup :mode :type :stru :port :noop) 200) ((:quit) 221)
                  ((:help) 211 214) ((:smnt) 202 250) ((:rein) 120 220)
                  ((:pasv) 227) ((:allo :site) 200 202) ((:rest :rnfr) 350)
                  ((:stor :list :stou :retr) 125 150 226 250) ((:xover) 224)
                  ((:group) 211) ((:article) 220) ((:nntp-quit) 205)
                  ((:nntp-list) 215) ((:smtp-data) 250 354)
                  ((:smtp-helo :mail-from :rcpt-to) 250))
             ht)
      (dolist (re (car cc)) (setf (gethash re ht) (cdr cc)))))
  "*The table of URL requests and replies, for use in `url-ask'.
See RFC959 (FTP) &c.")

(defun url-ask (sock out end &rest req)
  "Send a request; read the response."
  (declare (type socket sock) (type (or null stream) out)
           (type (or (unsigned-byte 10) symbol list) end))
  (when req
    (apply #'format sock req) (fresh-line sock)
    (when out (apply #'format out "~&url-ask[~s]: `~@?'~%" end req)))
  (loop :with endl :of-type list =
        (typecase end
          (integer (to-list end)) (list end)
          (symbol (gethash end *url-replies*))
          (t (error 'case-error :proc 'url-ask
                    :args (list 'end end 'integer 'list 'symbol))))
        :for ln :of-type simple-string =
        (string-right-trim +whitespace+ (read-line sock))
        :for len :of-type index-t = (length ln)
        :and code :of-type (or null (unsigned-byte 10)) = nil
        :when out :do (format out "~&url-ask[~s]: ~a~%" end ln)
        :while (or (< len 3) (and (> len 3) (char/= #\Space (schar ln 3)))
                   (null (setq code (parse-integer ln :end 3 :junk-allowed t)))
                   (and (< code 400) endl (not (member code endl :test #'=))))
        :finally (if (< code 400) (return (values ln code))
                     (error 'network :proc 'url-ask :host (socket-host sock)
                            :port (socket-port sock) :mesg ln))))

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

(defun ftp-get-passive-socket (sock err bin timeout)
  "Get a passive socket."
  (declare (type socket sock) (type (or null stream) err) (values socket))
  (multiple-value-call #'open-socket-retry
    (ftp-parse-sextuple (url-ask sock err :pasv "pasv"))
    :err err :max-retry 5 :bin bin :timeout timeout))

(defcustom *ftp-anonymous-passwords* list '("abc@ftp.net" "abc@")
  "*The list of passwords to try with anonymous ftp login.
Some ftp servers do not like `user@host' if `host' is not what they expect.")

(defun url-login-ftp (sock url err)
  "Login and cd to the FTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (let ((host (socket-host sock)) (port (socket-port sock)) co)
    (dolist (pwd *ftp-anonymous-passwords*
             (error 'login :proc 'url-login-ftp :host host :port port
                    :mesg "All passwords failed: ~{ ~s~}~% -- ~a"
                    :args (list *ftp-anonymous-passwords* co)))
      (url-ask sock err :user "user ~a" (if (zerop (length (url-user url)))
                                            "anonymous" (url-user url)))
      (unless (typep (setq co (nth-value
                               1 (ignore-errors
                                   (url-ask sock err :pass "pass ~a"
                                            (if (zerop (length (url-pass url)))
                                                pwd (url-pass url))))))
                     'error)
        (return)))
    (ignore-errors (url-ask sock err nil "syst")) ; :syst
    ;; (url-ask sock err :type "type i")
    (ignore-errors (url-ask sock err nil "stat")) ; :stat
    (handler-bind ((network (lambda (co)
                              (error 'login :proc 'url-login-ftp :host host
                                     :port port :mesg "CWD error:~% - ~a"
                                     :args (list (net-mesg co))))))
      (url-ask sock err :cwd "cwd ~a" (url-path-dir url)))))

(defcustom *buffer* (simple-array (unsigned-byte 8) (10240))
  (make-array 10240 :element-type '(unsigned-byte 8))
  "The download buffer - simple array of bytes.
The reasonable value for it's length is determined by your connection speed.
I recommend 10240 for 112kbps ISDN and 2048 for 28.8kbps, i.e.,
approximately, the number of bytes you can receive per second.")

(defun socket-to-file (data path &key rest (out *standard-output*))
  "Read from a binary socket to a file.
Read until the end, then close the socket."
  (declare (type socket data) (type pathname path) (type (or null stream) out))
  (with-open-file (fl path :direction :output :element-type 'unsigned-byte
                      :if-exists (if rest :append :supersede))
    (loop :for pos :of-type index-t = (read-sequence *buffer* data)
          :do (write-sequence *buffer* fl :end pos)
          :sum pos :of-type file-size-t
          :when out :do (princ "." out) (force-output out)
          :while (= pos (length *buffer*))
          :finally (when out (terpri out)) :finally (close data))))

(defun url-eta (len)
  "Return the ETA for the given length or nil or cannot determine."
  (declare (type (or null real) len))
  (and len (boundp '*url-bytes-transferred*)
       (not (zerop *url-bytes-transferred*))
       (/ (* len (elapsed *url-opening-time* nil))
          *url-bytes-transferred*)))

(defun ftp-get-file (sock rmt loc &key (out *standard-output*) (reget t)
                     (bin t) (retry 2) (timeout *url-default-timeout*)
                     (err *error-output*))
  "Get the remote file RMT from the FTP socket SOCK,
writing it into the local directory LOC.  Log to OUT.
Append if the file exists and REGET is non-nil.
Use binary mode if BIN is non-nil (default).
Retry (+ 1 RETRY) times if the file length doesn't match the expected."
  (declare (type socket sock) (type index-t retry)
           (type (or null stream) out err) (simple-string rmt)
           (values file-size-t double-float simple-string pathname))
  (let* ((data (ftp-get-passive-socket sock err t timeout)) (tot 0)
         (bt (get-float-time nil)) (path (merge-pathnames rmt loc))
         (rest (when (and reget (probe-file path))
                 (let ((sz (file-size path)))
                   (mesg :log out "File `~a' exists (~:d bytes), ~
~:[appending~;overwriting~]...~%" path sz (zerop sz))
                   (unless (zerop sz)
                     (url-ask sock err :rest "rest ~d" sz)
                     sz))))
         (line (progn (url-ask sock err :type "type ~:[a~;i~]" bin)
                      (url-ask sock err :retr "retr ~a" rmt))) ; 150
         (pos (position #\( line :from-end t))
         (len (when pos (read-from-string line nil nil :start (1+ pos)))))
    (declare (type socket data) (type file-size-t tot) (type pathname path)
             (double-float bt) (type (or null file-size-t) rest len))
    ;; (when rest (decf len rest))
    (if (null len) (mesg :log out "File lenth unknown.~%")
        (mesg :log out "Expect ~:d dot~:p for ~:d bytes~@[ [~/pr-secs/]~]~%"
              (ceiling (1+ len) (length *buffer*)) len (url-eta len)))
    (setq tot (socket-to-file data path :rest rest :out out))
    (url-ask sock err :retr)      ; 226
    (cond ((or (null len) (= tot len))
           (when (boundp '*url-bytes-transferred*)
             (incf *url-bytes-transferred* tot))
           (multiple-value-call #'values tot (elapsed bt nil t) path))
          ((plusp retry)
           (mesg :log out "### Wrong file length: ~:d (expected: ~:d [~@:d])
 +++ ~r more attempt~:p +++~%" tot len (- tot len) retry)
           (ftp-get-file sock rmt loc :out out :err err :reget nil :bin bin
                         :retry (1- retry) :timeout timeout))
          ((error "Wrong file length: ~:d (expected: ~:d [~@:d])"
                  tot len (- tot len))))))

(defun url-ftp-get (url loc &rest opts &key (out *standard-output*)
                    (err *error-output*) (max-retry *url-default-max-retry*)
                    (timeout *url-default-timeout*) &allow-other-keys)
  "Get the file specified by the URL, writing it into a local file.
The local file is located in directory LOC and has the same name
as the remote one."
  (declare (type url url) (type (or null stream) out err))
  (mesg :log out "~& *** getting `~a'...~%" url)
  (remf opts :max-retry)
  (with-open-url (sock url :err err :timeout timeout :max-retry max-retry)
    (multiple-value-bind (tot el st)
        (apply #'ftp-get-file sock (url-path-file url) loc opts)
      (mesg :log out " *** done [~:d bytes, ~a, ~:d bytes/sec]~%" tot st
            (round tot el)))))

(defun ftp-list (sock &key name (out *standard-output*) (err *error-output*)
                 (timeout *url-default-timeout*))
  "Get the file list."
  (declare (type socket sock) (type (or null stream) out err))
  (let ((data (ftp-get-passive-socket sock err nil timeout)))
    (url-ask sock err :list "list~@[ ~a~]" name) ; 150
    (loop :for line = (read-line data nil nil) :while line :when out :do
          (format out "~a~%" (string-right-trim +whitespace+ line)))
    (url-ask sock err :list)))  ; 226

;;; Mail

(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance))) (subseq st 0 (position #\Space st)))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (getenv "USER") "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

(defun url-send-mail (url &key (err *error-output*)
                      (text (current-time nil))
                      (helo *mail-host-address*)
                      (from *user-mail-address*))
  "Send TEXT to URL (which should be a MAILTO)."
  (declare (type url url) (type (or null stream) err)
           (simple-string text helo from))
  (assert (eq :mailto (url-prot url)) (url)
          "url-send-mail: `~a' is not a `mailto'" url)
  (with-open-url (sock url :err err)
    (url-ask sock err :smtp-helo "helo ~a" helo) ; 250
    (url-ask sock err :mail-from "mail from: ~a" from) ; 250
    (url-ask sock err :rcpt-to "rcpt to: ~a" (url-user url)) ; 250
    (url-ask sock err :smtp-data "data") ; 354
    (url-ask sock err :smtp-date "~a~%." text))) ; 250

;;; News

(defstruct (article #+cmu (:print-function print-struct-object))
  (numb 0 :type (unsigned-byte 32)) ; article number
  (subj "" :type simple-string) ; subject
  (auth "" :type simple-string) ; author
  (dttm 0 :type (integer 0))    ; date/time
  (msid "" :type simple-string) ; message-ID
  (msid1 "" :type simple-string) ; ????
  (bytes 0 :type file-size-t)   ; size in bytes
  (lines 0 :type index-t)       ; size in lines
  (xref nil :type list))        ; list of cross-references

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

(defun url-get-news (url loc &key (out *standard-output*) (err *error-output*)
                     (max-retry *url-default-max-retry*) re)
  "Get the news article to the OUT stream.
When RE is supplied, articles whose subject match it are retrieved."
  (declare (type url url) (stream out))
  (assert (or (eq :nntp (url-prot url)) (eq :news (url-prot url))) (url)
          "url-get-news: `~a' is not a `news'" url)
  (flet ((out (st) (if loc (merge-pathnames st loc) out)))
    (with-open-url (sock url :err err :max-retry max-retry)
      (let ((spl (split-string (url-path url) "/")))
        (cond ((cadr spl)       ; group and article
               (url-dump-to-dot sock :out (out (cadr spl))))
              ((car spl)        ; group only
               (multiple-value-bind (na a1 a2)
                   (values-list
                    (string-tokens (url-ask sock err :group "group ~a"
                                            (car spl)) ; 211
                                   :start 3 :max 3))
                 (let ((nm (format nil "~d-~d" a1 a2)))
                   (format out "~:d articles, from ~:d to ~:d~%" na a1 a2)
                   (url-ask sock err :xover "xover ~a" nm) ; 224
                   (let ((ls (map-in #'string->article
                                     (url-dump-to-dot sock :out (out nm)
                                                      :collect t))))
                     (when re
                       (dolist (art ls)
                         (when (search re (article-subj art))
                           (url-ask sock err :article "article ~d" ; 220
                                    (article-numb art))
                           (url-dump-to-dot sock :out
                                            (out (article-numb art))))))
                     ls))))
              (t               ; not even group, just host
               (url-ask sock err :nntp-list "list active") ; 215
               (url-dump-to-dot sock :out (out "active"))))))))

(defcustom *time-servers* list
  '("clock.psu.edu" "black-ice.cc.vt.edu" "clock1.unc.edu" "time-b.nist.gov"
    "time-b.timefreq.bldrdoc.gov" "clock-1.cs.cmu.edu" "ntp0.cornell.edu")
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
                    (format out "~{~30t[~a -- ~a~@[ [~d]~]]~%~}" val)
                    val))
          url))
    (url (with-open-url (sock url)
           (ecase (url-prot url)
             (:time
              (let ((nn (+ (ash (read-byte sock) 24) (ash (read-byte sock) 16)
                           (ash (read-byte sock) 8) (read-byte sock))))
                (values nn (dttm->string nn) (- nn (get-universal-time)))))
             (:daytime
              (let ((str (read-line sock)))
                (if (zerop (length str)) (values)
                    (values (string->dttm (copy-seq str)) str nil)))))))))

;;;
;;; }}}{{{ HTML streams
;;;

#+(or clisp acl cmu) (progn

(defclass html-stream (#+acl excl:fundamental-character-input-stream
                       #+clisp lisp:fundamental-character-input-stream
                       #+cmu ext:fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :reader html-in))
  (:documentation "The input stream for reading HTML."))

(defcustom *html-unterminated-tags* list '(:p :li :dd :dt :tr :td :th)
   "*The list of tags without the corresponding `/' tag.")

(defun html-end-tag (tag)
  (if (member tag *html-unterminated-tags* :test #'eq) tag
      (keyword-concat "/" tag)))

(defmethod stream-read-char ((in html-stream)) (read-char (html-in in)))
(defmethod stream-unread-char ((in html-stream)) (unread-char (html-in in)))
(defmethod stream-read-char-no-hang ((in html-stream))
  (read-char-no-hang (html-in in)))
;; (defmethod stream-peak-char ((in html-stream)) (peak-char (html-in in)))
(defmethod stream-listen ((in html-stream)) (listen (html-in in)))
(defmethod stream-read-line ((in html-stream)) (read-line (html-in in)))
(defmethod stream-clear-output ((in html-stream)) (clear-input (html-in in)))

)

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
    ;; ' .. ' is an error and
    ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
    (do ((beg -1) (len (1- (length str))))
        ((or (>= beg len)
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

(defun ts-skip-scripts (ts)
  "Read from the text stream one script."
  (declare (type text-stream ts))
  (let ((*html-parse-tags* t) pos)
    (do ((tok (read-next ts) (read-next ts)))
        ((and (html-tag-p tok) (eq (car (html-tag-data tok)) 'script))))
    (do () ((setq pos (search "</script>" (ts-buff ts) :test #'char-equal)))
      (ts-pull-next ts))
    (setf (ts-buff ts) (subseq (ts-buff ts) (+ pos (length "</script>"))))))

(defun next-token (ts &key (num 1) type dflt (kill *ts-kill*))
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num)
      (declare (type index-t ii))
      (do () ((not (html-tag-p (setq tt (read-next ts :errorp t :kill kill))))
              (mesg :log t "~d token (~s): ~s~%" ii (type-of tt) tt))
        (mesg :log t "tag: ~s~%" tt)))
    (if (and type (not (typep tt type))) dflt tt)))

(defun next-number (ts &key (num 1) (kill *ts-kill*))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (pushnew #\% kill :test #'char=)
  (let (tt)
    (dotimes (ii num)
      (declare (type index-t ii))
      (do () ((numberp (setq tt (next-token ts :kill kill)))))
      (mesg :log t "~d - number: ~a~%" ii tt))
    (mesg :log t " -><- number: ~a~%" tt)
    tt))

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
  '((netscape "/usr/bin/netscape" "-remote" "openURL(~a,new-window)")
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
                 (err *error-output*) (timeout *url-default-timeout*)
                 (proc #'identity) (max-retry *url-default-max-retry*))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to `*standard-output*'.
This is mostly a debugging function, to be called interactively."
  (declare (type url url) (stream out) (function proc))
  (format out "Opening URL: `~a'...~%" url)
  (with-open-url (sock url :err err :timeout timeout :max-retry max-retry)
    (loop :for ii :of-type index-t :from  1
          :and rr = (read-line sock nil +eof+) :until (eq +eof+ rr)
          :do (format out fmt ii
                      (funcall proc (string-right-trim +whitespace+ rr))))))

(defun url-get (url loc &key (timeout *url-default-timeout*)
                (max-retry *url-default-max-retry*)
                (err *error-output*) (out *standard-output*))
  "Get the URL.
This is the function to be called in programs.
Arguments: URL - what to get, LOC - where to place it.
Keywords: `timeout', `max-retry', `out', `err'."
  (declare (type url url) (type (or stream null) err out))
  (case (url-prot url)
    (:ftp (url-ftp-get url loc :timeout timeout :err err :out out
                       :max-retry max-retry))
    ((:nntp :news) (url-get-news url loc :out out :max-retry max-retry))
    ((:http :www)
     (let* ((path (merge-pathnames (url-path-file url) loc))
            (bt (get-float-time nil))
            (size (with-open-url (sock url :err err :timeout timeout
                                       :max-retry max-retry)
                    (socket-to-file sock path :out out))))
       (declare (type file-size-t size))
       (multiple-value-bind (el st) (elapsed bt nil t)
         (declare (double-float el))
         (mesg :log out "Wrote `~a' [~:d bytes, ~a, ~:d bytes/sec]."
               path size st (round size el)))))
    (t (error 'code :proc 'url-get :mesg "Cannot handle protocol ~s"
              :args (list (url-prot url))))))

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

(defun dump-url-tokens (url &key (fmt "~3d: ~a~%")
                        (out *standard-output*) (err *error-output*)
                        (max-retry *url-default-max-retry*))
  "Dump the URL token by token.
See `dump-url' about the optional parameters.
This is mostly a debugging function, to be called interactively."
  (declare (stream out) (simple-string fmt))
  (setq url (url url))
  (with-open-url (sock url :rt *html-readtable* :err err :max-retry max-retry)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
        ((eq +eof+ (setq rr (read-next ts))))
      (declare (type index-t ii))
      (format out fmt ii rr))))

;;;
;;; }}}{{{ HTML generation
;;;

(defcustom *html-output* stream *standard-output*
  "The stream where the HTML is printed by `with-tag' and `with-open-html'.
It is bound only by `with-open-html'.")
(makunbound '*html-output*)

(defmacro with-tag ((tag &rest options &key (close t) value
                         (terpri (and close (not value))) &allow-other-keys)
                    &body forms)
  (remf options :close) (remf options :terpri) (remf options :value)
  `(progn
    (when ,terpri (fresh-line *html-output*))
    (format *html-output* "<~a~@{ ~a=~s~}>" ,tag ,@options)
    (when ,value (princ ,value *html-output*))
    (when ,terpri (terpri *html-output*))
    ,@forms
    (when ,close (format *html-output* "~@[~&~*~]</~a>" ,terpri ,tag))))

(defmacro with-open-html (((&rest open-pars)
                           (&key (doctype ''(html public
                                             "-//W3C//DTD HTML 3.2//EN"))
                                 (meta '(:http-equiv "Content-Type"
                                         :content "text/html"))
                                 base comment (title "untitled") (footer t)
                                 head))
                          &body body)
  "Output HTML to a file."
  (with-gensyms ("WOH-MAILTO-" mailto)
    `(let ((,mailto (concatenate 'string "mailto:" *user-mail-address*)))
      (with-open-file (*html-output* ,@open-pars)
        (format *html-output* "<!doctype~{ ~s~}>~%" ,doctype)
        ;; print the comment
        (format *html-output* "<!--~% File: <~a - " ,(car open-pars))
        (current-time *html-output*)
        (format *html-output*
         " ~a@~a>~% Created by `with-open-html'~% Lisp: ~a ~a~@[~%~a~]~% -->~%"
         (getenv "USER") (machine-instance) (lisp-implementation-type)
         (lisp-implementation-version) ,comment)
        (terpri *html-output*)
        (when ,base
          (with-tag (:base :close nil :href ,base))
          (terpri *html-output*))
        (with-tag (:html)
          (terpri *html-output*)
          (with-tag (:head ,@head)
            (with-tag (:meta :close nil ,@meta))
            (terpri *html-output*)
            (with-tag (:link :close nil :rev 'made :href ,mailto))
            (terpri *html-output*)
            (with-tag (:title :value ,title)))
          (terpri *html-output*) (terpri *html-output*)
          (with-tag (:body)
            ,@body
            (when ,footer
              (terpri *html-output*) (terpri *html-output*)
              (with-tag (:p :terpri nil)
                (with-tag (:hr :close nil))
                (with-tag (:address :terpri nil)
                  (with-tag (:a :href ,mailto :value *user-mail-address*)))
                (terpri *html-output*) (with-tag (:br :close nil))
                (with-tag (:strong :value (current-time nil)))))))
        (terpri *html-output*)))))

;;; this is an example on how to use `with-open-html' and `with-tag'.
(defun directory-index (dir file &rest opts
                        &key (title (format nil "Index of ~a" dir)))
  "Output the index for a directory."
  ;; (directory-index "/etc/*" "/tmp/z.html")
  (with-open-html ((file :direction :output)
                   (:title title :comment
                    (format nil " Called: (directory-index ~s ~s~{ ~s~})"
                            dir file opts)))
    (with-tag (:h1 :terpri nil) (format *html-output* "Index of ~a" dir))
    (terpri *html-output*) (terpri *html-output*)
    (with-tag (:table :border 1)
      (dolist (fi (sort (directory dir #+cmu :follow-links #+cmu nil)
                        #'string< :key #'namestring))
        (with-tag (:tr)
          (with-tag (:th :align "left" :terpri nil)
            (with-tag (:a :href (namestring fi) :value fi)))
          (terpri *html-output*)
          (with-tag (:td :align "right" :terpri nil)
            (format *html-output* "~:d" (ignore-errors (file-size fi))))
          (terpri *html-output*)
          (with-tag (:td :align "right" :value
                         (ignore-errors (dttm->string (file-write-date
                                                       fi))))))))))

;;;
;;; CL server
;;;

(defcustom *cl-server-port* integer 453
  "*The default port for `cl-server'.
Defaults to (parse-integer \"cl\" :radix 36) ==> 453")

(defcustom *cl-server-password* (or null simple-string) "ansi-cl"
  "*The simple authentication.
Set to NIL to disable.")

(defcustom *cl-server-quit* list '("bye" "quit" "exit")
  "*The list of `quit' commands.")

(defun cl-server (&key (port *cl-server-port*) (password *cl-server-password*))
  "Establish a connection and answer questions."
  (declare (integer port))
  (let ((serv (open-socket-server port)))
    (declare (type socket-server serv))
    (unwind-protect
       (loop :for sock :of-type socket = (socket-accept serv) :with pwd :do
             (format t "[~a] connected:~60t~s:~d~%"
                     (current-time nil) (socket-host sock) (socket-port sock))
             (let ((*standard-output* sock)) (sysinfo))
             :if (or (null password)
                     (progn
                       (format sock "Enter password: ") (force-output sock)
                       (let ((str (read-line sock nil "")))
                         (declare (simple-string str))
                         (setq pwd (subseq str 0 (1- (length str))))
                         (string= pwd password))))
             :do (format sock "password ok~%to quit, type one of~?.~%"
                         (list-format "~s") *cl-server-quit*)
             (loop :for ii :of-type index-t :upfrom 1 :do
                   (format sock "~a[~d]: > " (package-short-name *package*) ii)
                   (force-output sock)
                   (handler-case
                       (let ((form (read sock)))
                         (when (and (or (stringp form) (symbolp form))
                                    (member form *cl-server-quit*
                                            :test #'string-equal))
                           (return))
                         (format sock "~{~a~^ ;~%~}~%"
                                 (multiple-value-list (eval form))))
                     (error (co)
                       (format sock "error:~%~a~%...flushed...~%" co))))
             (format sock "goodbye~%")
             (format t "[~a] connection closed:~60t~s:~d~%" (current-time nil)
                     (socket-host sock) (socket-port sock))
             :else :do (format sock "wrong password~%")
             (format t "[~a] access denied [~a]:~60t~s:~d~%" (current-time nil)
                     pwd (socket-host sock) (socket-port sock))
             :end :do (close sock))
      (socket-server-close serv))))

;;; cmucl/src/code/multi-proc.lisp
#+cmu
(defun start-lisp-connection-listener (&key (port 1025)
                                       (password (random (expt 2 24))))
  (declare (type (unsigned-byte 16) port))
  "Create a Lisp connection listener, listening on a TCP port for new
connections and starting a new top-level loop for each. If a password
is not given then one will be generated and reported."
  (labels (;; The session top level read eval. loop.
	   (start-top-level (stream)
             (unwind-protect
                  (let* ((*terminal-io* stream)
                         (*standard-input*
                          (make-synonym-stream '*terminal-io*))
                         (*standard-output* *standard-input*)
                         (*error-output* *standard-input*)
                         (*debug-io* *standard-input*)
                         (*query-io* *standard-input*)
                         (*trace-output* *standard-input*))
                    (format t "Enter password: ")
                    (finish-output)
                    (let* ((*read-eval* nil)
                           (read-password
                            (handler-case
                                (read)
                              (error () (return-from start-top-level)))))
                      (unless (equal read-password password)
                        (return-from start-top-level)))
                    (sysinfo)
                    (mp::top-level))
               (handler-case
                   (close stream)
                 (error ()))))
	   ;; The body of the connection listener.
	   (listener ()
	     (declare (optimize (speed 3)))
	     (let ((serv (open-socket-server port)))
	       (unwind-protect
		    (progn
		      (setf (process-name mp::*current-process*)
			    (format nil "Lisp connection listener on port ~d"
				    port))
		      (format t "~&;;; Started lisp connection listener on ~
 				  port ~d with password ~d~%"
			      port password)
		      (loop
		       ;; Wait for new connections.
                       (let ((sock (socket-accept serv)))
                         (make-process #'(lambda () (start-top-level sock))
                                       :name (format
                                              nil "Lisp session from ~s:~d"
                                              (socket-host sock)
                                              (socket-port sock))))))
		 ;; Close the listener stream.
		 (when serv (socket-server-close serv))))))
    ;; Make the listening thread.
    (make-process #'listener)))

(provide "url")
;;; }}} url.lisp ends here

#|
In Linux 2.2.X with the binmisc module, any .x86f file is a binary file.

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/register-lisp-as-executables.sh
#!/bin/sh
echo ':lisp:E::x86f::/usr/bin/lisp-start:'  >
/proc/sys/fs/binfmt_misc/register # this should only work for root
under linux 2.1.XX or later
# now you can do "chmod a+x hello.x86f" and
# ./hello.x86f
# from your favorite shell.

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/lisp-start
#!/bin/sh
/usr/bin/lisp -load $1

There is even a demo of how to use a lisp-server to wait for
commands, so you avoid the startup-delay of cmucl...

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/Start-up-server.lisp
(in-package :user)

(format t "THIS A A HUDGE SECURITY RISC. CHANGE THE PASSWORD!!!~%~%")
(setf mp::*idle-process* mp::*initial-process*)
(mp::start-lisp-connection-listener :port 6789 :password "Clara")
|#
