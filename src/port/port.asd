;;; Cross-implementation Portability System
;;;
;;; Copyright (C) 2010 by Sam Steingold
;;; This is Free Software, covered by the GNU LGPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html

(asdf:defsystem :port
    :author ("Sam Steingold <sds@gnu.org>")
    :licence "LGPL"
    :description "Cross-implementation Portability System"
    :depends-on ()
    :components
    ((:file "ext")
     (:file "gray" :depends-on ("ext"))
     (:file "mop" :depends-on ("ext"))
     (:file "net" :depends-on ("ext" "sys"))
     (:file "path" :depends-on ("ext"))
     (:file "proc" :depends-on ("ext"))
     (:file "shell" :depends-on ("ext"))
     (:file "sys" :depends-on ("ext" "path"))))
