;;; -*- Mode: Lisp -*-

;;; utilities.lisp --
;;; A random collection of interfaces to some general functionalities which
;;; tur out to be useful across the board.
;;;
;;; Copyright (c) 2000, 2001 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

;;; compiled-file-extension --
;;; This should be totally portable.

(declaim (inline compiled-file-extension))

(defun compiled-file-extension ()
  "Returns a string that is the default extension for CL compiled files.
The value is implementation dependent."
  (pathname-type (compile-file-pathname *default-pathname-defaults*)))


(defun binary-directory-name ()
  "Returns a string for a unique binary directory name for the
Common Lisp implementation"
  (software-binary-directory-name *common-lisp-implementation*))

;;; file-system-directory-separator --

(declaim (inline file-system-directory-separator))

(defun file-system-directory-separator ()
  "Returns a string that is the default directory separator.
The value is operating system and file system dependent."
  (os-file-system-directory-separator *os*))


;;; current-working-directory --

(declaim (inline current-working-directory)
	 (inline cwd))

(defun current-working-directory ()
  "Returns the pathname of the 'current working directory'."
  (current-directory-pathname *cl*))

(defun cwd ()
  "Returns the pathname of the 'current working directory'."
  (current-working-directory))


;;; end of file -- utilities.lisp --
