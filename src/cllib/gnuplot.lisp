;;; Gnuplot (http://www.gnuplot.org/) interface
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id$
;;; $Source$

;;; the main entry point is WITH-PLOT-STREAM
;;; (see also other exported functions)

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `date2time', `+day-sec+', `days-between', `date>'
  (require :cllib-date (translate-logical-pathname "cllib:date"))
  ;; `dl-nth-date', `dated-list-name', `dl-nth-slot', `dl-shift',
  ;; `copy-dated-list', `dl-endp', `dl-len', `dl-ll', `dl-date'
  (require :cllib-datedl (translate-logical-pathname "cllib:datedl"))
  ;; `regress', `make-line', `line-sl', `line-co'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `regress-poly'
  (require :cllib-stat (translate-logical-pathname "cllib:stat"))
  ;; `pipe-output', `close-pipe', `run-prog'
  (require :port-shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(*gnuplot-path* *gnuplot-printer* *gnuplot-default-directive*
          #+(or win32 mswindows) *gnuplot-path-console*
          plot-output +plot-term-screen+ +plot-term-printer+ +plot-term-file+
          +plot-timestamp+ directive-term make-plot-stream
          with-plot-stream plot-dated-lists plot-dated-lists-depth
          plot-lists plot-lists-arg plot-error-bars plot-functions))

;;;
;;; variables
;;;

(defcustom *plot-default-backend* symbol :gnuplot
  "The default plot backend designator.")
(defcustom *gnuplot-path* simple-string
  #+(or win32 mswindows)
  "c:/gnu/gp371w32/wgnupl32.exe"
  ;; "c:/bin/gnuplot/wgnuplot.exe"
  #+unix (if (string-equal (machine-type) "linux")
             "/usr/bin/gnuplot" "/usr/local/bin/gnuplot")
  "*The path to the windows gnuplot executable.")
(defconst +gnuplot-epoch+ integer (encode-universal-time 0 0 0 1 1 2000 0)
  "*The gnuplot epoch - 2000-1-1.")
#+(or win32 mswindows)
(defcustom *gnuplot-path-console* simple-string "c:/gnu/gp371w32/pgnuplot.exe"
  "*The path to the console gnuplot executable.")
(eval-when (compile load eval)  ; CMUCL
(defcustom *gnuplot-printer* simple-string
  (format nil
          #+(or win32 mswindows) "\\\\server1\\~a"
          #+unix "|lpr -h~@[ -P~a~]"
          (getenv "SDSPRT"))
  "*The printer to print the plots."))
#+unix
(defcustom *gnuplot-stream* (or null stream) nil
  "The current gnuplot output stream.")
(eval-when (compile load eval)  ; CMUCL
(defcustom *gnuplot-file* pathname (merge-pathnames "plot.tmp" *datadir*)
  "*The tmp file for gnuplot."))
(defcustom *gnuplot-msg-stream* (or stream null t) *standard-output*
  "*The message stream of gnuplot functions.")
(defcustom *gnuplot-default-directive* t :plot
  "*The default action for `with-plot-stream'.")

(declaim (ftype (function (date) (values integer)) plot-sec-to-epoch))
(defsubst plot-sec-to-epoch (dt)
  "Return the number of seconds from date DT to `+gnuplot-epoch+'."
  (declare (type date dt))
  (- (date2time dt) +gnuplot-epoch+))

(defun plot-msg (&rest args)
  "Write a status message to `*gnuplot-msg-stream*'."
  (fresh-line *gnuplot-msg-stream*)
  (write-string "[" *gnuplot-msg-stream*)
  (current-time *gnuplot-msg-stream*)
  (write-string "] " *gnuplot-msg-stream*)
  (apply #'format *gnuplot-msg-stream* args)
  (force-output *gnuplot-msg-stream*))

(defmacro with-plot-stream ((str &rest options) &body body)
  "Execute body, with STR bound to the gnuplot stream.
Usage: (with-plot-stream (stream :plot PLOT &rest OPTIONS) body).
OPTIONS are gnuplot(1) options, the following are accepted:
 XLABEL YLABEL TIMEFMT XDATA DATA-STYLE TITLE XBEG XEND GRID TERM
 BORDER LEGEND (key in gnuplot)
PLOT means:
  :plot     => plot;
  :print   => print;
  :wait    => plot and wait for gnuplot to terminate;
  :file    => write `*gnuplot-file*' and print a message;
  pathname designator => write this file and print a message;
  stream   => write gnuplot commands to this stream;
  NIL      => do nothing, print nothing, return NIL."
  (with-gensyms ("WPS-" body-function)
    `(flet ((,body-function (,str) ,@body))
      ;; this cannot be replaced with a simple inline funcall since
      ;; OPTIONS are not necessarily known at compile time
      ,(if (oddp (length options))
           `(apply #'internal-with-plot-stream #',body-function ,@options)
           `(internal-with-plot-stream #',body-function ,@options)))))

(defgeneric plot-output (plot out backend)
  (:documentation "Ourput a plot-related object to the plot stream
according to the given backend")
  (:method ((plot t) (out stream) (backend t))
    (declare (ignorable plot))
    (unless (and (output-stream-p out) (open-stream-p out))
      (error 'code :proc 'plot-output :args (list out)
             :mesg "2nd argument must be an open output stream, got: ~s"))
    (error 'code :proc 'plot-output :args (list backend)
           :mesg "unknown backend: ~s")))

;; this specifies the plot output terminal
(eval-when (compile load eval)  ; CMUCL
(defstruct (plot-term (:conc-name pltm-))
  (terminal nil :type (or null string))
  (terminal-options nil :type (or null string))
  (target nil :type (or null string pathname))))

(defmethod plot-output ((pt plot-term) (out stream) (backend (eql :gnuplot)))
  (declare (ignorable backend))
  (format out "set terminal ~a~@[ ~a~]~%"
          (pltm-terminal pt) (pltm-terminal-options pt))
  (if (pltm-target pt)
      (format out "set output '~a'~%"
              (make-pathname :type (if (string= "postscript"
                                                (pltm-terminal pt))
                                       "ps" (pltm-terminal pt))
                             :defaults (pltm-target pt)))
      (format out "set output~%")))

(defconst +plot-term-screen+ plot-term
  (make-plot-term :terminal #+unix "x11" #+(or win32 mswindows) "windows")
  "The `plot-term' object sending the plot to the screen.")
(defconst +plot-term-printer+ plot-term
  (make-plot-term :terminal "postscript"
                  :terminal-options "landscape 'Helvetica' 9"
                  :target *gnuplot-printer*)
  "The `plot-term' object sending the plot to the printer.")
(defconst +plot-term-file+ plot-term
  (make-plot-term :terminal "postscript"
                  :terminal-options "landscape 'Helvetica' 9"
                  :target *gnuplot-file*)
  "The `plot-term' object sending the plot to the printer.")

(defgeneric directive-term (directive)
  (:documentation "Return the PLOT-TERM object appropriate for this directive")
  (:method ((directive t))
    (error 'code :proc 'directive-term :args (list directive)
           :mesg "unknown directive: ~s"))
  (:method ((directive (eql :plot))) +plot-term-screen+)
  (:method ((directive (eql :wait))) +plot-term-screen+)
  (:method ((directive (eql :print))) +plot-term-printer+)
  (:method ((directive (eql :file))) +plot-term-file+)
  (:method ((directive string))
    (make-plot-term :terminal "postscript"
                    :terminal-options "landscape 'Helvetica' 9"
                    :target directive))
  (:method ((directive pathname))
    (make-plot-term :terminal "postscript"
                    :terminal-options "landscape 'Helvetica' 9"
                    :target directive)))

(eval-when (compile load eval)  ; CMUCL
(defstruct (plot-timestamp (:conc-name plts-))
  (fmt "%Y-%m-%d %a %H:%M:%S %Z" :type string)
  (pos '(0 . 0) :type cons)
  (font "Helvetica" :type string)))

(defmethod plot-output ((pt plot-timestamp) (out stream)
                        (backend (eql :gnuplot)))
  (declare (ignorable backend))
  (format out "set timestamp \"~a\" ~d,~d '~a'~%" (plts-fmt pt)
          (car (plts-pos pt)) (cdr (plts-pos pt)) (plts-font pt)))

(defconst +plot-timestamp+ plot-timestamp (make-plot-timestamp)
  "The standard timestamp.")

(eval-when (compile load eval)  ; CMUCL
(defstruct (plot-axis (:conc-name plax-))
  (name "" :type string)        ; gnuplot name: [xyz]{2}
  (label "" :type string)       ; label: "value", "time"...
  (tics t :type boolean)
  (fmt "%g" :type string)
  (time-p nil :type boolean)
  (range nil :type (or null cons)))

(defstruct (plot-spec (:conc-name plsp-))
  (term +plot-term-screen+ :type plot-term)
  (timestamp +plot-timestamp+ :type (or null plot-timestamp))
  (x-axis (make-plot-axis :name "x") :type plot-axis)
  (y-axis (make-plot-axis :name "y") :type plot-axis)
  (data-style :lines :type symbol)
  (border t :type boolean)
  (timefmt nil :type (or null string))
  (title "" :type string)
  (legend nil :type list)
  (grid nil :type boolean)
  (data nil :type (or list function))))

(defmethod plot-output ((pt null) (out stream) (backend (eql :gnuplot)))
  (declare (ignorable pt out backend)))

(defun %plotout (xx)
  (typecase xx
    (number (format nil "~g" xx))
    (symbol (string-downcase (symbol-name xx)))
    (list (format nil "~{ ~(~a~)~}" xx))
    (t (format nil "'~a'" xx))))

(defmethod plot-output ((pa plot-axis) (out stream) (backend (eql :gnuplot)))
  (declare (ignorable backend))
  (format out "set format ~a \"~a\"~%" (plax-name pa) (plax-fmt pa))
  (format out "set ~alabel \"~a\"~%" (plax-name pa) (plax-label pa))
  (format out "set ~:[no~;~]~atics~%" (plax-tics pa) (plax-name pa))
  (if (plax-time-p pa)
      (format out "set timefmt \"~a\"~%set ~adata time~%"
              (plax-fmt pa) (plax-name pa))
      (format out "set ~adata~%" (plax-name pa)))
  (let ((range (plax-range pa)))
    (when range
      (format out "set ~arange [~a:~a]~%" (plax-name pa)
              (%plotout (car range)) (%plotout (cdr range))))))

(defmethod plot-output ((ps plot-spec) (out stream) (backend (eql :gnuplot)))
  (plot-output (plsp-term ps) out backend)
  (plot-output (plsp-timestamp ps) out backend)
  (plot-output (plsp-x-axis ps) out backend)
  (plot-output (plsp-y-axis ps) out backend)
  (flet ((set-opt (nm par)
           (case par
             ((t) (format out "set ~a~%" nm))
             ((nil) (format out "set no~a~%" nm))
             (t (format out "set ~a ~a~%" nm (%plotout par))))))
    (set-opt "border" (plsp-border ps))
    (set-opt "data style" (plsp-data-style ps))
    (set-opt "title" (plsp-title ps))
    (set-opt "key" (plsp-legend ps))
    (set-opt "grid" (plsp-grid ps))
    (if (functionp (plsp-data ps))
        (funcall (plsp-data ps) out)
        (dolist (set (plsp-data ps))
          (dolist (datum set (format out "e~%"))
            (format out "~{~a~^ ~}~%" datum))))))

(defun make-plot (&key data (plot *gnuplot-default-directive*)
                  (xlabel "x") (ylabel "y")
                  (data-style :lines) (border t)
                  timefmt xb xe (title "plot") legend
                  (xtics t) (ytics t) grid
                  (xfmt (or timefmt "%g")) (yfmt "%g"))
  (make-plot-spec
   :data data :term (directive-term plot) :data-style data-style
   :x-axis (make-plot-axis :name "x" :label xlabel :tics xtics :fmt xfmt
                           :range (when (and xb xe) (cons xb xe))
                           :time-p (not (null timefmt)))
   :y-axis (make-plot-axis :name "y" :label ylabel :tics ytics :fmt yfmt)
   :grid grid :legend legend :title title :border border))

(defgeneric make-plot-stream (directive)
  (:documentation "Create the stream appropriate for the directive.")
  (:method ((directive t))
    (error 'code :proc 'make-plot-stream :args (list directive)
           :mesg "unknown plot directive: ~s"))
  (:method ((directive stream)) directive)
  (:method ((directive (eql :file))) (make-plot-stream *gnuplot-file*))
  (:method ((directive (eql :wait))) (make-plot-stream :plot))
  (:method ((directive string)) (open directive :direction :output))
  (:method ((directive pathname)) (open directive :direction :output))
  #+(or win32 mswindows)
  (:method ((directive (eql :print))) (pipe-output *gnuplot-path-console*))
  #+(or win32 mswindows)
  (:method ((directive (eql :plot))) (make-plot-stream *gnuplot-file*))
  #+unix
  (:method ((directive (eql :print))) (make-plot-stream :plot))
  #+unix
  (:method ((directive (eql :plot)))
    (setq *gnuplot-stream*
          (or (if (and *gnuplot-stream* (open-stream-p *gnuplot-stream*))
                  *gnuplot-stream*)
              (pipe-output *gnuplot-path*)))))

(defun internal-with-plot-stream (body-function &rest opts
                                  &key (plot *gnuplot-default-directive*)
                                  (backend *plot-default-backend*)
                                  &allow-other-keys)
  "The gist of `with-plot-stream' is here.
Should not be called directly but only through `with-plot-stream'."
  (when (eq plot t)
    (plot-msg "~s: plot directive ~s is deprecated; use ~s~%"
              'internal-with-plot-stream t :plot)
    (setq plot :plot))
  (let* ((plot-stream (make-plot-stream plot))
         (plot-spec (apply #'make-plot :data body-function :plot plot opts))
         (plot-file (pltm-target (plsp-term plot-spec))))
    (declare (stream plot-stream))
    (when (or (stringp plot) (pathnamep plot)) (setq plot :file))
    (unwind-protect
         (plot-output plot-spec plot-stream backend)
      ;; clean up
      (fresh-line plot-stream)
      (force-output plot-stream)
      (when (streamp plot)
        (plot-msg "wrote plot commands to ~s~%" plot))
      (ecase plot
        #+(or win32 mswindows)
        ((t :plot)
         (close plot-stream)
         (plot-msg "Starting gnuplot...")
         (close-pipe (pipe-output *gnuplot-path* "/noend" plot-file))
         (format *gnuplot-msg-stream* "done.~%"))
        #+unix
        ((t :plot) (plot-msg "Done plotting.~%"))
        #+(or win32 mswindows)
        (:wait
         (plot-msg "Waiting for gnuplot to terminate...")
         (format *gnuplot-msg-stream* "gnuplot returned ~a.~%"
                 (run-prog *gnuplot-path* :args (list "/noend" plot-file))))
        #+unix
        (:wait
         (fresh-line *terminal-io*)
         (princ "Press <enter> to continue..." *terminal-io*)
         (force-output *terminal-io*) (read-line *terminal-io* nil nil))
        (:print (plot-msg "Sent the plot to `~a'.~%" *gnuplot-printer*)
                #+unix (format plot-stream "set output~%"))
        (:file
         (close plot-stream)
         (plot-msg "Wrote `~a'.~%Type \"load '~a'\" at the gnuplot prompt.~%"
                   plot-file plot-file))))))

(defun plot-data-style (num-ls)
  "Decide upon the appropriate data style for the number of points."
  (when (listp num-ls)
    (setq num-ls (1- (reduce #'min num-ls :key #'length))))
  (assert (realp num-ls) (num-ls)
          "~s got neither number nor list: ~s" 'plot-data-style num-ls)
  (if (> num-ls 30) :lines :linespoints))

;;;###autoload
(defun plot-dated-lists (begd endd dls &rest opts &key (title "Dated Plot")
                         (xlabel "time") rel data-style
                         (ylabel (if rel "relative value" "value"))
                         (timefmt "%Y-%m-%d") ema (slot 'val)
                         &allow-other-keys)
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages."
  (assert dls () "Nothing to plot for `~a'~%" title)
  (setq begd (if begd (date begd) (dl-nth-date (car dls)))
        endd (if endd (date endd) (dl-nth-date (car dls) -1)))
  (remf opts :ema) (remf opts :rel) (remf opts :slot)
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style (or data-style (plot-data-style
                                                 (days-between begd endd)))
                     :timefmt timefmt :xb begd :xe endd opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}"
            ;; Ugly.  But gnuplot requires a comma *between* plots,
            ;; and this is the easiest way to do that.
            (mapcan (lambda (dl)
                      (cons (dated-list-name dl)
                            (mapcar (lambda (ee)
                                      (format nil "~a - EMA [~a]"
                                              (dated-list-name dl) ee))
                                    ema)))
                    dls))
    (terpri str)                ; the command line is over!
    (let* ((emal (make-list (length ema))) bv
           (val (if rel (lambda (dl) (/ (dl-nth-slot dl slot) bv))
                    (lambda (dl) (dl-nth-slot dl slot)))))
      (dolist (dl dls)
        (setq bv (dl-nth-slot dl slot))
        (do* ((td (dl-shift (copy-dated-list dl) begd) (dl-shift td)))
             ((or (dl-endp td) (date> (dl-nth-date td) endd))
              (format str "e~%"))
          (mapl (lambda (ee cc)
                  (let ((vv (funcall val td)))
                    (push (cons (dl-nth-date td)
                                (+ (* (car cc) vv)
                                   (* (- 1 (car cc)) (or (cdaar ee) vv))))
                          (car ee))))
                emal ema)
          (format str "~a ~f~%" (dl-nth-date td) (funcall val td)))
        (dolist (em emal)
          (dolist (ee (nreverse em) (format str "e~%"))
            (format str "~a ~f~%" (car ee) (cdr ee))))
        ;; clean EMAL for the next pass
        (do ((ee emal (cdr ee))) ((null ee)) (setf (car ee) nil))))))

;;;###autoload
(defun plot-lists (lss &rest opts &key (key #'value) (title "List Plot") rel
                   (xlabel "nums") (ylabel (if rel "relative value" "value"))
                   (depth (1- (reduce #'min lss :key #'length)))
                   (data-style (plot-data-style depth)) &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss) (type fixnum depth))
  (remf opts :depth) (remf opts :rel) (remf opts :key)
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb 0 :xe (1- depth) opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car lss))
    (let* (bv (val (if rel
                       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
                       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
        (setq bv (funcall key (cadr ls)))
        (do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
            ((= ix depth) (format str "e~%"))
          (declare (fixnum ix))
          (format str "~f~20t~f~%" ix (funcall val ll)))))))

;;;###autoload
(defun plot-lists-arg (lss &rest opts &key (key #'identity)
                       (title "Arg List Plot") (xlabel "nums") rel lines
                       (ylabel (if rel "relative value" "value"))
                       data-style quads xbeg xend &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates. KEY is used to extract the cons."
  (declare (list lss))
  (when (eq lines t)
    (setq lines (mapcar (lambda (ls)
                          (regress (cdr ls) :xkey (compose car 'key)
                                   :ykey (compose cdr 'key))) lss)))
  (when (eq quads t)
    (setq quads (mapcar (lambda (ls)
                          (regress-poly (cdr ls) 2 :xkey (compose car 'key)
                                        :ykey (compose cdr 'key))) lss)))
  (setq xbeg (or xbeg (reduce #'min lss :key (compose car 'key cadr)))
        xend (or xend (reduce #'max lss :key (compose car 'key car last))))
  (remf opts :key) (remf opts :rel) (remf opts :lines) (remf opts :quads)
  (remf opts :xbeg) (remf opts :xend)
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel
                     :data-style (or data-style (plot-data-style lss))
                     :xb xbeg :xe xend :title title opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}" (mapcar #'car lss))
    (dolist (ln lines) (plot-line-str ln xbeg xend str))
    (dolist (qu quads) (plot-quad-str qu xbeg xend str))
    (terpri str)
    (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
      (dolist (ls lss)
        (setq bv (cdr (funcall key (cadr ls))))
        (do ((ll (cdr ls) (cdr ll)) kk)
            ((null ll) (format str "e~%"))
          (setq kk (funcall key (car ll)))
          (format str "~f~20t~f~%" (car kk) (funcall val (cdr kk))))))))

;;;###autoload
(defun plot-error-bars (ll &rest opts &key (title "Error Bar Plot")
                        (xlabel "nums") (ylabel "value")
                        (data-style (plot-data-style (list ll)))
                        (xkey #'first) (ykey #'second) (ydkey #'third)
                        &allow-other-keys)
  "Plot the list with errorbars.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
The first element is the title, all other are records from which we
get x, y and ydelta with xkey, ykey and ydkey."
  (declare (list ll))
  (remf opts :xkey) (remf opts :ykey) (remf opts :ydkey)
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb (funcall xkey (second ll))
                     :xe (funcall xkey (car (last ll))) opts)
    (format str "plot 0 title \"\", '-' title \"~a\" with errorbars,~
 '-' title \"\", '-' title \"\", '-' title \"\"~%" (pop ll))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a ~a~%" (funcall xkey rr)
              (funcall ykey rr) (funcall ydkey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (funcall ykey rr) (funcall ydkey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (- (funcall ykey rr) (funcall ydkey rr))))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (+ (funcall ykey rr) (funcall ydkey rr))))))

;;;###autoload
(defun plot-functions (fnl xmin xmax numpts &rest opts &key data-style
                       (title "Function Plot") &allow-other-keys)
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
FNL is a list of (name . function).
E.g.:
  (plot-functions (list (cons 'sine #'sin) (cons 'cosine #'cos)) 0 pi 100
                  :legend '(:bot :left :box) :grid t :plot :wait)"
  (declare (list fnl) (real xmin xmax) (type index-t numpts))
  (with-plot-stream (str :xb xmin :xe xmax :title title
                     :data-style (or data-style (plot-data-style numpts)) opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car fnl))
    (dolist (fn fnl)
      (dotimes (ii (1+ numpts) (format str "e~%"))
        (declare (type index-t ii))
        (let ((xx (dfloat (/ (+ (* ii xmax) (* (- numpts ii) xmin)) numpts))))
          (format str "~f~20t~f~%" xx (funcall (cdr fn) xx)))))))

;;;###autoload
(defun plot-dated-lists-depth (depth dls slot &rest opts)
  "Plot the dated lists, DEPTH *days* from the beginning.
OPTS is passed to `plot-lists-arg'."
  (apply #'plot-lists-arg
         (mapcar
          (lambda (dl)
            (cons (prin1-to-string dl)
                  (dated-list-to-day-list dl :slot slot :depth depth)))
          dls)
         opts))

(defun dated-list-to-day-list (dl &key (slot 'val) (depth (dl-len dl)))
  "Make a list of conses (days-from-beg . value) of length
DEPTH out of the dated list."
  (declare (type dated-list dl) (symbol slot) (fixnum depth))
  (do ((bd (dl-nth-date dl)) (ll (dl-ll dl) (cdr ll)) (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (declare (fixnum ii) (type date bd) (list rr ll))
    (push (cons (days-between bd (funcall (dl-date dl) (car ll)))
                (slot-value (car ll) slot)) rr)))

(defun line-day2sec (ln begd)
  "Make a new line, converting from days to seconds."
  (declare (type line ln) (type integer begd))
  (make-line :sl (/ (line-sl ln) +day-sec+) :co
             (- (line-co ln) (* (line-sl ln) (/ begd +day-sec+)))))

(defun plot-line-str (ln beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the line from BEG to END.
This is not a complete plotting function (not a UI)!"
  (declare (type line ln) (real beg end) (stream str))
  (format str ", ((x>~a)?((x<~a)?(~a*x+~a):1/0):1/0) title \"~a\" with lines~
~@[ ~d~]" beg end (line-sl ln) (line-co ln) title lt))

(defun plot-quad-str (qu beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the parabola
from BEG to END.  This is not a complete plotting function (not a UI)!"
  (declare (type (simple-array double-float (3)) qu) (real beg end))
  (format str ", ((x>~a)?((x<~a)?(~a*x*x+~a*x+~a):1/0):1/0) title \"~a\" ~
with lines~@[ ~d~]" beg end (aref qu 0) (aref qu 1) (aref qu 2) title lt))

(provide :cllib-gnuplot)
;;; gnuplot.lisp ends here
