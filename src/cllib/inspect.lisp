;;; Inspect
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-length-dotted'
  (require :simple (translate-logical-pathname "cllib:simple"))
  ;; `class-slot-list'
  (require :closio (translate-logical-pathname "cllib:closio"))
  ;; `open-socket-server'
  (require :net (translate-logical-pathname "port:net"))
  ;; `browse-url'
  (require :url (translate-logical-pathname "cllib:url"))
  ;; `string-beg-with'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; `with-tag'
  (require :htmlgen (translate-logical-pathname "cllib:htmlgen")))

(in-package :cllib)

(export '(inspect *inspect-frontend* *inspect-print-lines*
          *inspect-print-level* *inspect-print-length* *inspect-length*))

;;;
;;; options
;;;

(defvar *inspect-frontend* :tty) ; the default frontend
(defvar *inspect-print-lines* 5) ; default for `*print-lines*'
(defvar *inspect-print-level* 5) ; default for `*print-level*'
(defvar *inspect-print-length* 10) ; default for `*print-length*'
(defvar *inspect-length* 5)     ; the number of sequence elements to print

;; all `inspection' objects in this session
(defparameter *inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
(defparameter *inspect-debug* 0) ; debug level

;;;
;;; backend
;;;

(defstruct (inspection (:conc-name insp-))
  self
  (id (fill-pointer *inspect-all*) :type fixnum)
  (title "" :type string)
  (blurb nil :type list)
  (up nil :type (or null inspection))
  (num-slots 0 :type fixnum)
  (pos nil :type (or null fixnum)) ; pos in parent
  (nth-slot nil :type (or null (function (integer) (t t)))) ; value & name
  (set-slot nil :type (or null (function (integer t) t))))

(defun insp-last-slot (insp)
  (1- (insp-num-slots insp)))
(defun insp-num-slots-print (insp)
  (min (insp-last-slot insp) *inspect-length*))
(defun insp-left-p (insp) ; check for the presence of a left neighbor
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< 0 pos))))
(defun insp-right-p (insp) ; check for the presence of a right neighbor
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< pos (insp-last-slot up)))))

(defun set-slot-error (ii obj)
  (error "~s: Cannot set the slot number ~s for object ~s"
         'set-slot-error ii obj))

(defmacro with-nth-hash-slot (ht args1 args2 retform)
  (with-gensyms ("WNGS-" ii jj)
    `(lambda (,ii ,@args1)
      (block with-nth-hash-slot
       (let ((,jj -1))
         (maphash (lambda ,args2
                    (declare (ignorable ,@args2))
                    (when (= ,ii (incf ,jj))
                      (return-from with-nth-hash-slot ,retform)))
                  ,ht))))))

(defgeneric inspect-backend (object &rest opts)
  (:method ((obj array) &rest opts)
    (let* ((siz (array-total-size obj)) (type (array-element-type obj))
           (arr (make-array siz :displaced-to obj :element-type type)))
      (apply #'make-inspection :self obj :title
             (typecase obj (string "String") (vector "Vector") (t "Array"))
             :blurb (list (format nil "dimension~p:~{ ~:d~}"
                                  (array-rank obj) (array-dimensions obj))
                          (format nil "element-type: ~s" type)
                          (if (= 1 (array-rank obj))
                              (if (array-has-fill-pointer-p obj)
                                  (format nil "fill-pointer: ~:d"
                                              (fill-pointer obj))
                                  "no fill pointer")
                              (format nil "total size: ~:d" siz))
                          (multiple-value-bind (di off)
                              (array-displacement obj)
                            (if di (format nil "displaced to (~:d): ~s" off di)
                                "not displaced")))
             :num-slots siz :nth-slot (lambda (ii) (aref arr ii))
             :set-slot (lambda (ii val) (setf (aref arr ii) val))
             opts)))
  (:method ((obj hash-table) &rest opts)
    (let ((count (hash-table-count obj)))
      (apply #'make-inspection :self obj :title "Hash Table"
             :blurb (list (format nil "count: ~:d" count)
                          (format nil "size: ~:d" (hash-table-size obj))
                          (format nil "rehash-size: ~:d"
                                  (hash-table-rehash-size obj))
                          (format nil "rehash-threshold: ~:d"
                                  (hash-table-rehash-threshold obj))
                          (format nil "test: ~:d" (hash-table-test obj)))
             :num-slots count :set-slot
             (with-nth-hash-slot obj (val) (kk vv) (setf (gethash kk obj) val))
             :nth-slot
             (with-nth-hash-slot obj nil (kk vv) (values vv kk)) opts)))
  (:method ((obj cons) &rest opts)
    (multiple-value-bind (len dotted-p) (list-length-dotted obj)
      (apply
       #'make-inspection
       :num-slots (if len (if dotted-p (1+ len) len)
                      (1+ (position obj obj :test #'eq)))
       :nth-slot (lambda (ii) (if (and dotted-p (= ii len)) dotted-p
                                  (nth ii obj)))
       :set-slot (lambda (ii val)
                   (if (and dotted-p (= ii len))
                       (setf (cdr (nthcdr (1- ii) obj)) val)
                       (setf (nth ii obj) val)))
       :blurb (list (if len
                        (if dotted-p
                            (if (> len 1)
                                (format nil "a dotted list of length ~:d" len)
                                (format nil "a cons"))
                            (format nil "a list of length ~:d" len))
                        (format nil "a cyclic list")))
       :self obj :title (format nil "Cons") opts)))
  (:method ((obj symbol) &rest opts)
    (apply #'make-inspection :num-slots 2
           :nth-slot (lambda (ii)
                       (case ii
                         (0 (values (if (boundp obj) (symbol-value obj)
                                        nil) ; #<unbound>?
                                    :symbol-value))
                         (1 (values (symbol-plist obj) :symbol-plist))))
           :set-slot (lambda (ii val)
                       (case ii
                         (0 (setf (symbol-value obj) val))
                         (1 (setf (symbol-plist obj) val))))
           :blurb (list (format nil "package: ~s" (symbol-package obj)))
           :self obj :title "Symbol" opts))
  (:method ((obj structure-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (slot-value obj slot) slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj standard-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (slot-value obj slot) slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj ratio) &rest opts)
    (apply #'make-inspection :self obj :title "rational number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (numerator obj) 'numerator)
                 (values (denominator obj) 'denominator)))
           :set-slot #'set-slot-error opts))
  (:method ((obj complex) &rest opts)
    (apply #'make-inspection :self obj :title "complex number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (realpart obj) 'realpart)
                 (values (imagpart obj) 'imagpart)))
           :set-slot #'set-slot-error opts))
  (:method ((obj t) &rest opts)
    (apply #'make-inspection :self obj :title "atom"
           :blurb (list (format nil "type: ~s" (type-of obj))
                        (format nil "class: ~s" (class-of obj))) opts))
  (:method :around ((obj t) &key id &allow-other-keys)
    (or (and (not id) (find obj *inspect-all* :key #'insp-self))
        (let ((insp (call-next-method)))
          (when (> *inspect-debug* 0)
            (format t "~s [id: ~:d, forced: ~s]: ~s~%" 'inspect-backend
                    (insp-id insp) id (insp-self insp)))
          (if id (setf (aref *inspect-all* id) insp)
              (vector-push-extend insp *inspect-all*))
          insp))))

(defun get-insp (id-or-insp com)
  "Get the INSPECTION object from the ID (or inspection object) and COMmand."
  (let ((insp (etypecase id-or-insp
                (inspection id-or-insp)
                (fixnum (aref *inspect-all* id-or-insp)))))
    (unless (eq insp (aref *inspect-all* (insp-id insp)))
      (error "~s: ~s is corrupted (~d->~d):~%~s~%~s~%"
             'get-insp '*inspect-all* (insp-id insp)
             (insp-id (aref *inspect-all* (insp-id insp))) insp
             (aref *inspect-all* (insp-id insp))))
    (when insp
      (case com
        (:q :q)
        (:s ;; re-inspect Self
         (inspect-backend (insp-self insp) :up (insp-up insp)
                          :pos (insp-pos insp) :id (insp-id insp)))
        (:u (insp-up insp))
        (:l (when (insp-pos insp)
              (get-insp (insp-up insp) (1- (insp-pos insp)))))
        (:r (when (insp-pos insp)
              (get-insp (insp-up insp) (1+ (insp-pos insp)))))
        (t (when (and (integerp com) (< -1 com (insp-num-slots insp)))
             (inspect-backend (funcall (insp-nth-slot insp) com)
                              :up insp :pos com)))))))

;;;
;;; To define a frontend, one has to define methods for
;;;  `print-inspection' and `inspect-frontend'

(defgeneric print-inspection (insp out frontend)
  (:method ((insp inspection) (out stream) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'print-inspection frontend)))
(defgeneric inspect-frontend (insp frontend)
  (:method ((insp inspection) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'inspect-frontend frontend)))
(defgeneric inspect-finalize (frontend)
  (:method ((frontend t))
    (dotimes (ii (length *inspect-all*))
      (setf (aref *inspect-all* ii) nil))
    (setf (fill-pointer *inspect-all*) 0)))

(defun inspect-read-clean-eval (insp stream)
  (labels ((clean (form)
             (cond ((eq (car form) :self)
                    (setf (car form) (list 'quote (insp-self insp)))
                    (clean-up (cdr form)))
                   ((eq (car form) :slot)
                    (setf (car form) 'funcall
                          (cdr form) (cons (insp-nth-slot insp) (cdr form)))
                    (clean-up (cddr form)))
                   (t (clean-up (car form))
                      (clean-up (cdr form)))))
           (clean-up (form) (when (consp form) (clean form)) form))
    (eval (clean-up (read stream nil nil)))))

;;;
;;; TTY frontend
;;;

(defmethod print-inspection ((insp inspection) (out stream)
                             (backend (eql :tty)))
  (declare (ignore backend))
  (format out "~s:  ~a~%~{ ~a~%~}" (insp-self insp) (insp-title insp)
          (insp-blurb insp))
  (when (insp-nth-slot insp)
    (loop :for ii :from 0 :to (insp-num-slots-print insp)
          :do (multiple-value-bind (val name) (funcall (insp-nth-slot insp) ii)
                (format out "~d~@[ [~a]~]:  ~s~%" ii name val)))))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :tty)))
  (print-inspection insp *terminal-io* frontend)
  (do (com (id (insp-id insp)))
      ((eq com :q))
    (fresh-line)
    (princ "INSPECT-- type :h for help; :q to return to the REPL ---> ")
    (force-output)
    (case (setq com (read *terminal-io* nil :q))
      (:q)
      ((:h :?) (format t " *** commands:~% :h, :?~15t this help
 :p, :a~15t Print the current item Again
 :s~15t re-inspect this item (Self)
 :d~15t Describe the current item~%")
       (when (insp-up insp)
         (format t " :u~15t return UP to the parent~%"))
       (when (insp-left-p insp)
         (format t " :l~15t inspect the left neighbor~%"))
       (when (insp-right-p insp)
        (format t " :r~15t inspect the right neighbor~%"))
       (when (insp-nth-slot insp)
         (format t " number~15t inspect this slot~%"))
       (format t " :e lisp-form~15t eval this form, with these substitutions:
 ~20t (:slot number) is replaced with the appropriate slot value
 ~20t :self is replaced with this object
 :m num lisp~15t Modify this slot
 :q~15t return to the main Read/Eval/Print loop~% ---> ")
       (force-output))
      (:d (describe (insp-self insp)))
      ((:p :a) (print-inspection insp *terminal-io* frontend))
      (:e (handler-case (print (inspect-read-clean-eval insp *terminal-io*))
            (error (err) (format t " *** error: ~s" err))))
      (:m (handler-case
              (print (funcall (insp-set-slot insp)
                              (inspect-read-clean-eval insp *terminal-io*)
                              (inspect-read-clean-eval insp *terminal-io*)))
            (error (err) (format t " *** error: ~s" err))))
      (t (cond ((setq insp (get-insp id com))
                (print-inspection insp *terminal-io* frontend)
                (setq id (insp-id insp)))
               (t (format t "command `~s' is not valid here~%" com)
                  (setq insp (get-insp id :s))))))))

;;;
;;; HTTP backend
;;;

(defmethod print-inspection ((insp inspection) (raw stream)
                             (backend (eql :http)))
  (declare (ignore backend))
  (flet ((href (com) (format nil "/~d/~s" (insp-id insp) com)))
    (with-html-output (out raw :title (insp-title insp) :footer nil)
      (with-tag (:h1) (princ (insp-title insp) out))
      (with-tag (:ul)
        (dolist (item (insp-blurb insp))
          (with-tag (:li) (princ item out))))
      (with-tag (:font :size "+4")
        (with-tag (:pre) (write (insp-self insp) :stream out)))
      (when (insp-nth-slot insp)
        (with-tag (:ol)
          (loop :for ii :from 0 :to (insp-num-slots-print insp)
                :do (multiple-value-bind (val name)
                        (funcall (insp-nth-slot insp) ii)
                      (with-tag (:li)
                        (with-tag (:a :href (href ii))
                          (princ (or name "inspect") out))
                        (with-tag (:pre) (write val :stream out)))))))
      (with-tag (:hr))
      (with-tag (:h2) (princ "describe:" out))
      (with-tag (:pre) (describe (insp-self insp) out))
      (with-tag (:hr))  ; footer
      (with-tag (:table :width "100%")
        (with-tag (:tr)
          (with-tag (:td :align "left")
            (with-tag (:a :href (href :q)) (princ "quit" out)))
          (when (insp-left-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :l)) (princ "left" out))))
          (when (insp-right-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :r)) (princ "right" out))))
          (when (insp-up insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :u)) (princ "parent" out))))
          (with-tag (:td :align "right")
            (with-tag (:a :href (href :s)) (princ "self" out))))))))

(defun http-command (server &key (debug *inspect-debug*))
  "Accept a connection from the server, return the GET command and the socket."
  (let ((socket (socket-accept server)))
    (loop (let ((line (read-line socket nil nil)))
            (when (> debug 1) (format t "-> ~a~%" line))
            (when (string-beg-with "GET /" line)
              (let* ((pos (position #\/ line :test #'char= :start 5))
                     (id (parse-integer line :start 5 :end pos))
                     (com (read-from-string line nil nil :start (1+ pos))))
                (when (> debug 0)
                  (format t "~s: ~d ~s~%" 'http-command id com))
                (when (> debug 1)
                  (loop (format t "-> ~a~%" (read-line socket nil nil))
                        (unless (listen socket) (return))))
                (return (values socket id com))))))))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :http)))
  (declare (ignore backend))
  (do ((server (let ((server (open-socket-server 17356)))
                 (browse-url (format nil "http://~a:~d/0/:s"
                                     (socket-server-host server)
                                     (socket-server-port server)))
                 server))
       sock id com)
      ((eq com :q) (socket-server-close server))
    (setf (values sock id com) (http-command server))
    (if (eq com :q)
        (with-html-output (out sock :title "inspect" :footer nil)
          (with-tag (:h1) (princ "thanks for using inspect" out))
          (with-tag (:p) (princ "you may close this window now" out)))
        (if (setq insp (get-insp id com)) (print-inspection insp sock frontend)
            (with-html-output (out sock :title "inspect" :footer nil)
              (with-tag (:h1)
                (format out "error: wrong command: ~:d/~s" id com))
              (with-tag (:p)
                (princ "either this is an old inspect session, or a " out))
                (with-tag (:a :href "https://sourceforge.net/bugs/?func=addbug&group_id=1802") (print "bug" out)))))
    (close sock)
    (when (> *inspect-debug* 0)
      (format t "~s [~s]: cmd:~d/~s id:~d~%" 'inspect-frontend frontend
                id com (insp-id insp)))))

;;;
;;; the juice
;;;

;;;###autoload
(defun inspect (object &key (frontend *inspect-frontend*))
  (let ((*print-array* nil) (*print-pretty* t)
        (*print-circle* t) (*print-escape* t)
        #-clisp (*print-lines* *inspect-print-lines*)
        (*print-level* *inspect-print-level*)
        (*print-length* *inspect-print-length*)
        (*package* (make-package (gensym "INSPECT-TNP-PACKAGE-"))) ; for `read'
        (*inspect-frontend* frontend))
    (unwind-protect
         (inspect-frontend (inspect-backend object) frontend)
      (inspect-finalize frontend)
      (delete-package *package*))
    (values)))

(provide :inspect)
;;; inspect.lisp ends here
