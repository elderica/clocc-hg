;;; Network Access
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id$
;;; $Source$
;;; $Log$
;;; Revision 1.1  1999/11/24 17:07:09  sds
;;; Cross-implementation Portability System
;;;
;;;

(in-package :cl-user)
(require "ext")


;;;
;;; name resulution
;;;

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

(defstruct hostent
  "see gethostbyname(3) for details"
  (name "" :type simple-string)
  (aliases nil :type list)
  (addr-list nil :type list)
  (addr-type 2 :type fixnum))

(defun resolve-host-ipaddr (host)
  "Call gethostbyname(3) or gethostbyaddr(3)."
  #+allegro
  (let* ((ipaddr
          (etypecase host
            (string
             (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch)))
                        host)
                 (socket:dotted-to-ipaddr host) (socket:lookup-hostname host)))
            (integer host)))
         (name (socket:ipaddr-to-hostname ipaddr)))
    (make-hostent :name name :addr-list
                  (list (socket:ipaddr-to-dotted ipaddr))))
  #+(and clisp syscalls)
  (let ((he (posix:resolve-host-ipaddr host)))
    (make-hostent :name (posix::hostent-name he)
                  :aliases (posix::hostent-aliases he)
                  :addr-list (posix::hostent-addr-list he)
                  :addr-type (posix::hostent-addr-type he)))
  #+cmu
  (let ((he (ext:lookup-host-entry host)))
    (make-hostent :name (ext:host-entry-name he)
                  :aliases (ext:host-entry-aliases he)
                  :addr-list (mapcar #'ipaddr-to-dotted
                                     (ext:host-entry-addr-list he))
                  :addr-type (ext:host-entry-addr-type he)))
  #+lispworks
  (let ((he (fli:dereference (comm::gethostbyname host))))
    (make-hostent :name (fli:convert-from-foreign-string
                         (fli:foreign-slot-value he 'comm::h_name))
                  :aliases
                  (loop :with pp = (fli:foreign-slot-value he 'comm::h_aliases)
                        :for cp = (fli:dereference pp)
                        :until (fli:null-pointer-p cp)
                        :collect (fli:convert-from-foreign-string cp)
                        :do (fli:incf-pointer pp))
                  :addr-list
                  (loop :with pp =
                        (fli:foreign-slot-value he 'comm::h_addr_list)
                        :for cp = (fli:dereference pp :type '(:unsigned :long))
                        :until (zerop cp) ; broken !!!
                        :collect (ipaddr-to-dotted cp)
                        :do (fli:incf-pointer pp))
                  :addr-type (fli:foreign-slot-value he 'comm::h_addrtype)))
  ;; #+gcl
  #-(or allegro cmu lispworks (and clisp syscalls))
  (error 'not-implemented :proc (list 'resolve-host-ipaddr host)))

;;;
;;; Sockets
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
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
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

;;;
;;; Conditions
;;;

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

;;; file net.lisp ends here
