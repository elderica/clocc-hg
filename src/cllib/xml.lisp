;;; XML parsing
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `+whitespace+', `whitespace-char-p'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `substitute-subseq-if'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; print CLOS objects readably
  (require :closio (translate-logical-pathname "cllib:closio"))
  ;; `with-timing', `mesg'
  (require :log (translate-logical-pathname "cllib:log"))
  ;; `read-from-stream'
  (require :fileio (translate-logical-pathname "cllib:fileio"))
  (require :gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(export
 '(*xml-readtable* *xml-print-xml*
   with-xml-input with-xml-file xml-read-from-file))

;;;
;;; Entities
;;;

(defpackage xml-tags (:use))
(defcustom *xml-pack* package (find-package :xml-tags)
  "The package with all the XML tags and entities.")
(defcustom *xml-amp* hash-table (make-hash-table :test 'equal)
  "The `&' entities")
(defcustom *xml-per* hash-table (make-hash-table :test 'equal)
  "The `%' entities")
(defcustom *xml-ent-file* pathname
  (translate-logical-pathname "cllib:entities.xml")
  "*The file with the default entities, like &gt; and &amp;.
See <http://www.w3.org/TR/WD-html40-970708/sgml/entities.html>.")
(defcustom *xml-keep-comments* boolean nil
  "*When non-nil, keep the comments inside the XML-OBJ structure.")

(defun xml-init-entities ()
  "Clear both `*xml-amp*' and `*xml-per*' and then read `*xml-ent-file*'."
  (clrhash *xml-amp*) (clrhash *xml-per*)
  (xml-read-from-file *xml-ent-file* :reset-ent nil))

(defun xml-read-entity (stream)
  "read <!ENTITY ....>"
  (let ((ch (peek-char t stream)) (ht *xml-amp*) ent type data def)
    (when (char= ch #\%)
      (read-char stream) (peek-char t stream) (setq ht *xml-per*))
    (setq ent (xml-read-text stream #'whitespace-char-p :clean nil)
          type (read stream)
          data (if (symbolp type) (read stream) type))
    (check-type data string)
    (setf def
          (cond
            ((eq type 'xml-tags::system)
             (lambda (&optional junk)
               (declare (ignore junk))
               (let ((str (handler-case
                              (open (merge-pathnames data (xml-path stream))
                                    :direction :input)
                            (error (err)
                              (mesg :err t "cannot open file [~s]/[~s]~%"
                                    data (xml-path stream t))
                              (error err)))))
                 (mesg :log t "~& * [~a ~:d bytes]..." data (file-length str))
                 str)))
            ((eq type 'xml-tags::cdata)
             (lambda (&optional string)
               (if string data (make-string-input-stream data))))
            ((symbolp type)
             (error 'code :proc 'xml-read-entity :mesg
                    "[~a]: not implemented: type: ~s data: ~s"
                    :args (list stream type data)))
            ((setq data (nsubstitute #\" #\' data))
             (lambda (&optional string)
               (if string data (make-string-input-stream data))))))
    ;; finish reading - till #\>
    (xml-read-text stream #\> :clean nil) (read-char stream)
    (multiple-value-bind (val fp) (gethash ent ht)
      (when (and fp (not (equalp val def)))
        (warn "[~s]: redefining ~s [~c]" ; from ~s to ~s
              'xml-read-entity ent (if ch #\& #\%)))) ; val def
    (setf (gethash ent ht) def)
    ent))

(defun xml-entity (ent hash type &key (proc 'xml-entity) string)
  "Find the entity in the hash table."
  (multiple-value-bind (val fp) (gethash ent hash)
    (unless fp
      (warn "[~s]: ~c entity ~s undefined" proc type ent))
    (typecase val
      (function (funcall val string))
      (null "??")
      (t (error 'code :proc proc :args (list type ent val) :mesg
                "~c entity ~s was defined as ~s" )))))

(defun xml-expand-entities (string &key (start 0) end)
  "Substitute the expansions of all the entities in the string."
  (substitute-subseq-if
   string (lambda (seq beg fin)
            (position #\& seq :start beg :end fin :test #'char=))
   (lambda (seq beg fin) (1+ (position #\; seq :start beg :end fin)))
   (lambda (seq beg fin type)
     (declare (ignore type))
     (xml-entity (subseq seq (1+ beg) (1- fin)) *xml-amp* #\&
                 :proc 'xml-expand-entities :string t))
   :start start :end end))

;;;
;;; XML objects
;;;

(defcustom *xml-print-xml* symbol nil
  "*Set to non-NIL to print XML-OBJ for future XML parsing.
Note that the Unicode characters will NOT be printed as &#nnnn;.
If this is `:sgml', use maximum SGML compatibility.")

(defcustom *xml-pre-namespaces* hash-table (make-hash-table :test 'equal)
  "The mapping from prefixes to namespaces.")

(defcustom *xml-uri-namespaces* hash-table (make-hash-table :test 'equal)
  "The mapping from URIs to namespaces.")

(eval-when (compile load eval)  ; CMUCL
(defstruct (xml-comment #+cmu (:print-function print-struct-object))
  (data "" :type string))

(defstruct (xml-namespace (:conc-name xmlns-)
                          #+cmu (:print-function print-struct-object))
  (uri "" :type string)
  (pre (princ-to-string (gensym "NS")) :type string)
  (nht (make-hash-table :test 'equal) :type hash-table)) ; names
)

(defun xmlns-get (uri &rest opts &key pre-tmp &allow-other-keys)
  "Get the XML namespace or create a new one.
Add it to `*xml-pre-namespaces*' and `*xml-uri-namespaces*'."
  (multiple-value-bind (ns oldp)
      (typecase uri
        (xml-namespace (values uri t))
        (string (gethash uri *xml-uri-namespaces*)))
    (unless oldp
      (remf opts :pre-tmp)
      (setq ns (apply #'make-xml-namespace :uri uri opts))
      ;; only newly created namespaces have to be reported to the user
      (mesg :log t "~& * added XML namespace: ~s~@[ [prefix ~s]~]~%"
            ns pre-tmp))
    (setf (gethash (xmlns-uri ns) *xml-uri-namespaces*) ns)
    (push ns (gethash (xmlns-pre ns) *xml-pre-namespaces*))
    (when pre-tmp
      (push ns (gethash pre-tmp *xml-pre-namespaces*)))
    ns))

(defconst +xml-namespace-xml+ xml-namespace
  (xmlns-get "http://www.w3.org/XML/1998/" :pre "xml")
  "The XML namespace, as per 'Namespaces in XML' '4: Using Qualified Names'.
<URL:http://www.w3.org/TR/REC-xml-names/#ns-using>.")
(defconst +xml-namespace-none+ xml-namespace (xmlns-get "" :pre "")
  "The namespace for unqualified names.")
(defcustom *xml-default-namespace* xml-namespace +xml-namespace-none+
  "The default namespace.")

(eval-when (compile load eval)  ; CMUCL
(defstruct (xml-name (:conc-name xmln-)
                     #+cmu (:print-function print-struct-object))
  (ln "" :type string)          ; local name
  (ns +xml-namespace-none+ :type xml-namespace))
)

(defun xmln-get (name namespace)
  "Create an XML name and add it to the appropriate hashtable.
If such a name already exists, re-use it."
  (declare (type string name))
  (let ((ns (typecase namespace
              (xml-namespace namespace)
              (t (or (car (gethash namespace *xml-pre-namespaces*))
                     (error 'code :proc 'xmln-get :args (list namespace)
                            :mesg "~s does not name a namespace"))))))
    (or (gethash name (xmlns-nht ns))
        (let ((nm (make-xml-name :ln name :ns ns)))
          (setf (gethash (xmln-ln nm) (xmlns-nht (xmln-ns nm))) nm)
          (mesg :log t "~& * new XML name: ~s~%" nm)
          nm))))

(defun xmlns-print-all (&key (out *standard-output*))
  (declare (stream out))
  (format out " * ~:d XML namespaces present:~%"
          (hash-table-count *xml-uri-namespaces*))
  (let ((ii 0))
    (with-hash-table-iterator (iter *xml-uri-namespaces*)
      (loop (multiple-value-bind (re key val) (iter)
              (unless re (return))
              (format out " [~d] ~s -> ~s~%" (incf ii) key val))))))

(defun xmlns-reset ()
  (xmlns-print-all)
  (clrhash *xml-uri-namespaces*)
  (clrhash *xml-pre-namespaces*)
  (clrhash (xmlns-nht +xml-namespace-none+))
  (clrhash (xmlns-nht +xml-namespace-xml+))
  (xmlns-get +xml-namespace-none+)
  (xmlns-get +xml-namespace-xml+)
  (xmlns-print-all))

(defsubst xmln-prefix (xmln)
  "Return the prefix for the name."
  (declare (type xml-name xmln))
  (let ((ns (xmln-ns xmln)))
    (unless (eq ns +xml-namespace-none+) (xmlns-pre ns))))

(defmethod print-object ((cmt xml-comment) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~&<!-- ~a -->~%" (xml-comment-data cmt))))

(defmethod print-object ((ns xml-namespace) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (ns out :type t :identity t)
        (format out "~s ~s ~:d" (xmlns-uri ns) (xmlns-pre ns)
                (hash-table-count (xmlns-nht ns))))))

(defmethod print-object ((xmln xml-name) (out stream))
  (cond (*print-readably* (call-next-method))
        ((null (xmln-ns xmln)) (princ (xmln-ln xmln) out))
        (*xml-print-xml*
         (format out "~@[~a:~]~a" (xmln-prefix xmln) (xmln-ln xmln)))
        ((format out "{~a}:~a" (xmln-ns xmln) (xmln-ln xmln)))))

(eval-when (compile load eval)  ; CMUCL
(defstruct (xml-tag (:conc-name xmlt-)
                    #+cmu (:print-function print-struct-object))
  (name nil :type (or string cons xml-name))
  ;; `string' and `cons' are replaced with `xml-name' during
  ;; `xml-resolve-namespaces'
  (args nil :type list))        ; alist of arg/value
)

(defmethod print-object ((xmlt xml-tag) (out stream))
  (cond (*print-readably* (call-next-method))
        (*xml-print-xml*
         (format out "~a~:{ ~a=~s~}" (xmlt-name xmlt) (xmlt-args xmlt)))
        ((format out "~a [~:{~a=~s~:^ ~}]"
                 (xmlt-name xmlt) (xmlt-args xmlt)))))

(eval-when (compile load eval)  ; CMUCL
(defstruct (xml-decl (:include xml-tag)
                     #+cmu (:print-function print-struct-object)))

(defstruct (xml-obj (:include xml-tag) (:conc-name xmlo-)
                    #+cmu (:print-function print-struct-object))
  (data nil :type list))        ; list of objects in the tag
)

(defmethod print-object ((xml xml-decl) (out stream))
  (cond (*print-readably* (call-next-method))
        (*xml-print-xml* (princ "<?" out) (call-next-method) (princ "?>" out))
        ((print-unreadable-object (xml out :type t :identity t)
           (call-next-method)))))

(defsubst xmlo-long-p (obj)
  (declare (type xml-obj obj))
  (or (xmlo-data obj) (eq *xml-print-xml* :sgml)))

(defun xml-size (obj)
  "Compute the approximate size of the object.
The first number returned is `text sise' (no tags)
the second is `file size' (including tags)."
  (typecase obj
    (string (let ((ll (length obj))) (values ll ll)))
    (symbol (xml-size (symbol-name obj)))
    (sequence (reduce #'+ obj :key #'xml-size))
    (xml-name (+ (length (xmln-ln obj))
                 (if (xmln-ns obj) (+ 1 (length (xmln-prefix obj))) 0)))
    (xml-obj
     (do ((fs (reduce #'+ (xmlt-args obj)
                      :key (lambda (att)
                             (+ (xml-size (car att))
                                (length (cadr att)) 4)) ; #\Space=""
                      :initial-value
                      (let ((ll (xml-size (xmlo-name obj))))
                        (if (xmlo-long-p obj)
                            (+ 5 (* 2 ll)) ; <tag></tag>
                            (+ 3 ll))))) ; <tag/>
          (ts 0) (dl (xmlo-data obj) (cdr dl)))
         ((null dl) (values ts fs))
       (multiple-value-bind (t0 f0) (xml-size (car dl))
         (incf fs f0) (incf ts t0))))
    (t (error 'case-error :proc 'xml-size :args
              (list 'obj obj 'string 'xml-name 'xml-obj)))))

(defun xml-ascii-p (char) (> 256 (char-code char)))

(defun xml-de-unicode (string)
  "Replace the unicode characters with &#NNNN;"
  (substitute-subseq-if
   string
   (lambda (str beg end)
     (position-if (complement #'xml-ascii-p) str :start beg :end end))
   (lambda (str beg end) (declare (ignore str end)) (1+ beg))
   (lambda (str beg end type)
     (declare (ignore end type))
     (format nil "&#~d;" (char-code (char str beg))))))

(defmethod print-object ((xml xml-obj) (out stream))
  (cond (*print-readably* (call-next-method))
        (*xml-print-xml*
         (princ "<" out) (call-next-method)
         (cond ((xmlo-long-p xml)
                (princ ">" out)
                (dolist (dd (xmlo-data xml))
                  (princ (typecase dd (string (xml-de-unicode dd)) (t dd))
                         out))
                (format out "</~a>" (xmlt-name xml)))
               (t (princ "/>" out))))
        ((print-unreadable-object (xml out :type t :identity t)
           (multiple-value-bind (ts fs) (xml-size xml)
             (call-next-method)
             (format out " ~:d object~:p ~:d/~:d chars"
                     (length (xmlo-data xml)) ts fs))))))

(defun xml-push (new xml)
  "Add NEW to data in XML."
  (typecase new
    (xml-obj (push new (xmlo-data xml)))
    (string (unless (zerop (length new))
              (if (stringp (car (xmlo-data xml)))
                  (let ((last (pop (xmlo-data xml))))
                    (push (concatenate 'string last new) (xmlo-data xml)))
                  (push new (xmlo-data xml)))))
    (xml-comment (when *xml-keep-comments* (push new (xmlo-data xml))))
    (cons (assert (equal (car new) (xmlo-name xml)) (new)
                  "~s: ~s was terminated by ~s" 'xml-push (xmlo-name xml) new)
          (setf (xmlo-data xml) (nreverse (xmlo-data xml))))
    (t (error 'case-error :proc 'xml-push :args
              (list 'new new 'xml-obj 'string 'xml-comment 'cons)))))

;;;
;;; XML streams
;;;

(defclass xml-stream-in (fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :accessor xmlis-st)
   (all-streams :type list :accessor xmlis-all)
   (tag-stack :type list :initform nil :accessor xmlis-stack)
   (size :type integer :initform 0 :accessor xmlis-size)
   (comment :accessor xmlis-comment :documentation
            "nil - outside comment, t - inside, 1 - inside, one `-' read"))
  (:documentation "The input stream for reading XML."))

(defun stream-length (st)
  "A wrap around for `file-stream'."
  (etypecase st
    (file-stream (file-length st))
    (list (reduce #'+ st :key #'stream-length))
    (concatenated-stream (stream-length (concatenated-stream-streams st)))
    (string-stream 0)))         ; can we do any better than this?

(defmethod initialize-instance :after ((str xml-stream-in) &rest junk)
  (declare (ignore junk))
  (cond ((typep (xmlis-st str) 'concatenated-stream)
         (setf (xmlis-all str) (concatenated-stream-streams (xmlis-st str))))
        ((setf (xmlis-all str) (list (xmlis-st str))
               (xmlis-st str) (make-concatenated-stream (xmlis-st str)))))
  (setf (xmlis-size str) (stream-length (xmlis-st str))))

(defmethod stream-read-char ((in xml-stream-in))
  (read-char (xmlis-st in) nil :eof))
(defmethod stream-unread-char ((in xml-stream-in) (char character))
  (unread-char char (xmlis-st in)))
;; the default method is good enough,
;; but in Allegro CL it is not defined
(defmethod stream-read-char-no-hang ((in xml-stream-in))
  (read-char-no-hang (xmlis-st in) nil :eof))
(defmethod stream-peek-char ((in xml-stream-in))
  (peek-char nil (xmlis-st in) nil :eof))
(defmethod stream-listen ((in xml-stream-in))
  (listen (xmlis-st in)))
(defmethod stream-read-line ((in xml-stream-in))
  (read-line (xmlis-st in)))
(defmethod stream-clear-input ((in xml-stream-in))
  (clear-input (xmlis-st in)))
(defmethod close ((in xml-stream-in) &rest opts)
  (dolist (st (xmlis-all in)) (apply #'close st opts))
  (apply #'close (xmlis-st in) opts))
(defun xml-path (str &optional debug-p)
  (declare (type xml-stream-in str))
  (when debug-p
    (format t "~& * All streams:~{~%~s~}~% * Pending:~{~%~s~}~%"
            (xmlis-all str) (concatenated-stream-streams (xmlis-st str))))
  (dolist (st (concatenated-stream-streams (xmlis-st str)))
    (when (typep st 'file-stream)
      (when debug-p (format t " == ~s -> ~s~%" st (truename st)))
      (return (truename st)))))

;;;
;;; Reading
;;;

(defun compress-whitespace (list &optional ends)
  "Replace internal whitespace with #\\Space
and trim (when ENDS is non-NIL) the leading and trailing whitespace."
  (do ((ll list (cdr ll)) good)
      ((null ll)
       (when (and ends good) (setf (cdr good) nil))
       (if ends (member-if (complement #'whitespace-char-p) list) list))
    (if (whitespace-char-p (car ll))
        (loop :initially (setf (car ll) #\Space)
              :while (and (cdr ll) (whitespace-char-p (cadr ll)))
              :do (setf (cdr ll) (cddr ll)))
        (setq good ll))))

(defun xml-read-text (str term &key (clean t) base)
  "Read characters from stream STR until TERM.
Return a string (with whitespace compressed with `compress-whitespace'
if the keyword argument `clean' is non-NIL, which is the default, and
the unicode entities &#nnnn; replaced with the appropriate characters
base the keyword argument BASE, when it is non-NIL, default - NIL)
TERM can be a predicate, a chacacter or a sequence of chacacters."
  (loop :with endp = (etypecase term
                       (function term)
                       (character (lambda (ch) (char= ch term)))
                       (sequence (lambda (ch) (find ch term :test #'char=))))
        :for ch = (read-char str t nil t)
        :until (funcall endp ch) :collect ch :into list
        :finally
        (unread-char ch str)
        (when clean (setq list (compress-whitespace list)))
        (when base
          (do ((ll list (cdr ll))) ((null ll)) ; &#nnnn;
            (when (and (char= #\& (car ll)) (char= #\# (cadr ll)))
              (do* ((l1 (cddr ll) (cdr l1)) (nn 0))
                   ((char= #\; (car l1))
                    (setf (car ll) (code-char nn)
                          (cdr ll) (cdr l1)))
                (setq nn (+ (* nn base)
                            (digit-char-p (car l1) base)))))))
        (return (values (coerce list 'string) ch))))

(defun xml-read-comment (str)
  "We are inside a comment; read it and return as a string."
  (loop :with ch :for data = (xml-read-text str '#\- :clean nil)
        :collect data :into all :do (read-char str t nil t)
        :if (char= #\- (setq ch (read-char str t nil t))) :do
        (setq ch (read-char str t nil t))
        (assert (char= ch #\>) (ch) "~s[~a]: ~s instead of #\>"
                'xml-read-comment str ch)
        (return (reduce (lambda (s0 s1) (concatenate 'string s0 s1))
                        all :initial-value ""))
        :else :collect (concatenate 'string "-" (string ch)) :into all))

(defun xml-list-to-alist (list)
  "(x a #\: b #\= \"c\" d #\= \"e\" y) -->
   (x ((\"A\" \"B\") \"c\") (\"D\" \"e\") y)"
  ;; we do not resolve namespaces here (that is done during the
  ;; post-processing in `xml-resolve-namespaces') because of the
  ;; recursive nature of namespace resolution
  (flet ((tost (xx) (typecase xx (symbol (symbol-name xx)) (t xx))))
    (do ((ll list))
        ((null (cddr ll)) (setf (car list) (tost (car list))) list)
      (if (or (eql (cadr ll) #\:) (eql (cadr ll) #\=))
          (setf (cadr ll) (tost (car ll)) (car ll) (cdr ll) (cdr ll) (cdddr ll)
                (cadar ll) (tost (cadar ll)) (cddar ll) nil)
          (setf (car ll) (tost (car ll)) ll (cdr ll))))))

(defun xml-resolve-namespaces (obj &key recursive-p)
  "Resolve all names in the XML object.
Resolution is done according to `*xml-default-namespace*'
and `*xml-pre-namespaces*'."
  (etypecase obj
    (xml-comment obj) (xml-decl obj) (string obj)
    (sequence (map-in #'xml-resolve-namespaces obj))
    (xml-obj
     (unless recursive-p
       (mesg :log t "~&~s: cleared ~s: ~s~%" 'xml-resolve-namespaces
             '*xml-pre-namespaces* *xml-pre-namespaces*)
       (clrhash *xml-pre-namespaces*))
     (let (pref-list dns)
       ;; 1st pass: define the new namespaces
       (setf (xmlt-args obj)
             (delete-if
              (lambda (att)
                (if (consp (car att))
                    (when (string= "xmlns" (caar att))
                      (format t "ns: ~s~%" att)
                      (when (member (cdar att) pref-list :test #'eq)
                        (error 'code :proc 'xml-resolve-namespaces
                               :mesg "duplicate namespace ~s: ~s"
                               :args (list (cdar att) (xmlt-args obj))))
                      (let ((pref (cadar att)))
                        (push pref pref-list)
                        (xmlns-get (cadr att) :pre-tmp pref)))
                    (when (string= "xmlns" (car att))
                      (format t "dns: ~s~%" att)
                      (when dns
                        (error 'code :proc 'xml-resolve-namespaces
                               :mesg "duplicate default namespace: ~s"
                               :args (list (xmlt-args obj))))
                      (setq dns (xmlns-get (cadr att))))))
              (xmlt-args obj)))
       (let ((*xml-default-namespace* (or dns *xml-default-namespace*)))
         ;; 2nd pass: resolve the names
         (flet ((xmln-cons (obj default-ns)
                  (if (consp obj)
                      (xmln-get (cadr obj) (car obj))
                      (xmln-get obj default-ns))))
           ;; (format t "name: ~s~%" (xmlt-name obj))
           (setf (xmlt-name obj) ; name
                 (xmln-cons (xmlt-name obj) *xml-default-namespace*))
           (do ((attr (xmlt-args obj) (cdr attr))) ; attributes
               ((null attr))
             ;; http://www.w3.org/TR/REC-xml-names/#defaulting
             ;; "Namespaces in XML": 5.2 "Namespace Defaulting"
             ;; "default namespaces do not apply directly to attributes"
             ;; (format t "attr: ~s~%" (car attr))
             (setf (caar attr) (xmln-cons (caar attr) +xml-namespace-none+))))
         ;; 3rd pass: check for identical attributes
         (do ((ll (xmlt-args obj) (cdr ll))) ((null ll))
           (do ((mm (cdr ll) (cdr mm))) ((null mm))
             (when (eq (caar mm) (caar ll))
               (error 'code :proc 'xml-resolve-namespaces
                      :mesg "duplicate attribute ~s: ~s"
                      :args (list (caar ll) (xmlt-args obj))))))
         ;; process data
         (unwind-protect
              (dolist (oo (xmlo-data obj) obj)
                (typecase oo
                  (xml-obj (xml-resolve-namespaces oo :recursive-p t))))
           (dolist (pref pref-list)
             (pop (gethash pref *xml-pre-namespaces*)))))))))

(defun xml-read-tag (str)
  "Read the tag, from <TAG-NAME> to </TAG-NAME>.
The first character to be read is #\T."
  (let ((attribs (xml-list-to-alist (read-delimited-list #\> str t))))
    (if (eq 'xml-tags::/ (car (last attribs)))
        (make-xml-obj :name (car attribs) :args (nbutlast (cdr attribs)))
        (loop :with next
              :and xml = (make-xml-obj :name (car attribs) :args (cdr attribs))
              :initially (push (xmlo-name xml) (xmlis-stack str))
              :while (not (consp next)) :do
              (xml-push (xml-read-text str "<&") xml)
              (xml-push (setq next (read str t nil t)) xml)
              :finally
              (assert (equal (car next) (car (xmlis-stack str))) (next)
                      "~s[~a]: ~s terminated ~s" 'xml-read-tag str
                      next (xmlis-stack str))
              (pop (xmlis-stack str))
              (return xml)))))

(defun read-xml (stream char)
  (ecase char
    (#\<                        ; read tag
     (let ((ch (read-char stream t nil t)) (*package* *xml-pack*))
       (case ch
         (#\/ (let ((tag (xml-list-to-alist
                          (read-delimited-list #\> stream t))))
                (assert (null (cdr tag)) (tag)
                        "~s[~a]: end tag ~s has attributes ~s" 'read-xml stream
                        (car tag) (cdr tag))
                tag))
         (#\? (let ((args (xml-list-to-alist
                           ;; can a declaration have namespaces?
                           (read-delimited-list #\> stream t))))
                (assert (eq 'xml-tags::? (car (last args))) (args)
                        "~s[~a]: <? was terminated by ~s" 'read-xml stream
                        (car (last args)))
                (make-xml-decl :name (car args) :args (nbutlast (cdr args)))))
         (#\! (let ((obj (read stream t nil t)))
                (case obj
                  (xml-tags::-- (make-xml-comment
                                 :data (xml-read-comment stream)))
                  (xml-tags::entity (make-xml-comment
                                     :data (xml-read-entity stream)))
                  ;; FIXME - there might be comments!
                  (t (cons obj (xml-list-to-alist
                                (read-delimited-list #\> stream t)))))))
         (t (unread-char ch stream)
            (xml-read-tag stream)))))
    (#\[ (xml-list-to-alist (read-delimited-list #\] stream t)))
    ;;(#\> (funcall (get-macro-character #\)) stream char)
    ;;     (xml-read-text stream #\<))
    ((#\& #\%)
     (let* ((ent (xml-read-text stream #\;))
            (str (if (and (char= #\& char) (char= #\# (char ent 0)))
                     (let ((code (parse-integer ent :start 1)))
                       (if (< code char-code-limit)
                           (string (code-char code))
                           (concatenate 'string "&" ent)))
                     (xml-entity ent (case char
                                       (#\& *xml-amp*) (#\% *xml-per*))
                                 char :proc 'read-xml))))
       (read-char stream)       ; #\;
       (etypecase str
         (string str)           ; "??" for undefined entities and &#nnnn;
         (stream
          (setf (xmlis-st stream) ; push
                (apply #'make-concatenated-stream str
                       (concatenated-stream-streams (xmlis-st stream)))
                (xmlis-all stream)
                (cons str
                      (delete-if (lambda (st) ; clean up
                                   (when (and (typep st 'string-stream)
                                              (eof-p st))
                                     (close st)))
                                 (xmlis-all stream))))
          (incf (xmlis-size stream) (stream-length str))
          (read stream t nil t)))))
    ((#\: #\=) char)))

;;;
;;; UI
;;;

(defun make-xml-readtable (&optional (rt (copy-readtable)))
  "Return a readtable for reading XML."
  (set-macro-character #\< #'read-xml nil rt)
  (set-macro-character #\[ #'read-xml nil rt)
  (set-macro-character #\& #'read-xml nil rt)
  (set-macro-character #\% #'read-xml nil rt)
  ;; (set-macro-character #\> #'read-xml nil rt)
  (set-macro-character #\> (get-macro-character #\)) nil rt)
  (set-macro-character #\] (get-macro-character #\)) nil rt)
  ;; this is a hack, but it works under Allegro, CLISP and CMUCL
  (set-macro-character #\' (get-macro-character #\") nil rt)
  ;; handle namespaces
  (set-syntax-from-char #\: #\Space rt)
  (set-macro-character #\: #'read-xml nil rt)
  ;; attribute="value"
  (set-syntax-from-char #\= #\Space rt)
  (set-macro-character #\= #'read-xml nil rt)
  ;; do we really need this?
  ;; (set-syntax-from-char #\; #\a rt)
  (setf (readtable-case rt) :preserve)
  rt)

(defcustom *xml-readtable* readtable (make-xml-readtable)
  "The readtable for XML parsing.")

(defmacro with-xml-input ((var stream) &body body)
  "Open the XML stream, evaluate the forms, make sure the stream is closed."
  `(with-open-stream (,var (make-instance 'xml-stream-in :input ,stream))
    (let ((*readtable* *xml-readtable*)) ,@body)))

(defmacro with-xml-file ((var file &key reset-ent (out '*standard-output*))
                         &body body)
  "Open the XML stream to file."
  `(with-timing (:out ,out)
    (when ,reset-ent (xml-init-entities))
    (with-xml-input (,var (open ,file :direction :input))
      (when ,out
        (format ,out "~&[~s]~% * [~a ~:d bytes]..." 'with-xml-input
                file (file-length (car (xmlis-all ,var))))
        (force-output ,out))
      (unwind-protect (progn ,@body)
        (when ,out
          (format ,out "done [entities(%/&): ~:d/~:d] [bytes: ~:d]"
                  (hash-table-count *xml-per*) (hash-table-count *xml-amp*)
                  (xmlis-size ,var)))))))

;;;###autoload
(defun xml-read-from-file (file &key (reset-ent t) (repeat t))
  "Read all XML objects from the file."
  (xml-resolve-namespaces
   (with-xml-file (str file :reset-ent reset-ent)
     (read-from-stream str :repeat repeat))))

(provide :xml)
;;; file xml.lisp ends here
