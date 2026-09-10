;;; -*- mode: lisp; package: jnil.ast -*-
;;; Copyright (c) 2026 Dan Stanger. All Rights Reserved.
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the preamble
;;; found in license-llgpl.txt.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose. See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file 
;;; license-lgpl.txt that was distributed with this file.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;;; Suite 330, Boston, MA  02111-1307  USA
;;;
;;; Description: Hack replacement functions for Jnil
;;;

;(in-package #:javatools.jlinker)
(in-package #:jnil.ast)
(defvar *verbose-output* 0)
(defun pc () *verbose-output*)
(defun pv () (and *verbose-output* (equal *verbose-output* 1)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *jlinker-data* '(:klass-name nil :java-class-string nil :java-class-name nil :superclass nil :fn nil :j nil :jcall-fields nil)))
(defmacro def-java-class (arg1 arg2 arg3 arg4 arg5)
  (let* ((java-class-string (second arg1)))
    (setf (getf *jlinker-data* :java-class-string) java-class-string)
    (setf (getf *jlinker-data* :klass-name) (first arg1))
    (setf (getf *jlinker-data* :java-class-name) (first (last (uiop:split-string java-class-string :separator "."))))
    (setf (getf *jlinker-data* :superclass) arg2)
    (setf (getf *jlinker-data* :jcall-fields) arg5))
  (values))

; The :initform is set incorrecly, what is currently the initform should be
; converted to a getter instead, but for now overwrite the initform value.
(defmacro def-java-method (arg1 arg2)
  (let* ((function-name (first arg1))
	 (java-function-name (second arg1))
	 (c (list function-name :initform java-function-name))
	 (fn (getf *jlinker-data* :fn))
	 (nfn (cons function-name fn))
	 (j (getf *jlinker-data* :j))
	 (nj (cons c j)))
    (setf (getf *jlinker-data* :fn) nfn)
    (setf (getf *jlinker-data* :j) nj))
  (values))

(defmacro jlinker-output1 ()
  (symbol-macrolet ((klass-name (getf *jlinker-data* :klass-name))
                    (java-class-string (getf *jlinker-data* :java-class-string))
                    (superclass (getf *jlinker-data* :superclass))
		    (fn (getf *jlinker-data* :fn))
		    (j (getf *jlinker-data* :j))
		    (jcall-fields (getf *jlinker-data* :jcall-fields)))
    `(progn
       (defclass ,klass-name ,superclass
	 ((eclipse-name :initform ,java-class-string)
	  (properties :initform nil :reader get-property :writer (setf set-property))
	  ,@jcall-fields ,@j))
       (defmethod print-object ((obj ,klass-name) stream)
	 (let ((*print-readably* nil) (*print-circle* t))
	   (print-unreadable-object (obj t)
	     (when (pc) (format t "class-of ~a~%" (class-of obj)))
	     (when (pv) (format t "class precedence list ~a~%"
		     (mapcar #'class-name (c2mop:class-precedence-list (class-of obj)))))
	     (mapcar #'(lambda(s)
			 (let ((sdn (c2mop:slot-definition-name s)))
                           (when (pv) (format t "~&slot name ~A slot value ~A ~%" sdn
                             (if (slot-boundp obj sdn) (slot-value obj sdn) "unbound")))))
			     (closer-mop:class-direct-slots  (class-of obj)))))))))

(defmacro jlinker-output2 ()
  (let ((klass-name (getf *jlinker-data* :klass-name))
        (fn (getf *jlinker-data* :fn)))
    `(progn
       ,@(loop for f in fn
               collect `(defmethod ,f ((obj ,klass-name))
                          (slot-value obj ',f))))))

(defmacro jlinker-cleanup ()
    (mapc #'(lambda (key) (setf (getf *jlinker-data* key) nil))
          (loop :for (key nil) :on  *jlinker-data* :by #'cddr :collect key))
  (values))

(defun jcall (arg1 arg2 &rest arg3)
;  (break)
  (incf *jcall-count*)
  (apply (coerce (cdr (assoc arg1 *jcalls* :test #'equal)) 'function) arg2 arg3))

;(trace jcall)
(defun jlinker-query () t)
(defun jlinker-init (&optional o) t)
(defun process-java-ast (arg1 arg2)
  (let ((java-ast (read-java-ast arg1 arg2)))
    (apply *parser* nil java-ast)))

(defun jstatic (eclipse-func eclipse-package arg1 &optional arg2)
  (cond ((string= eclipse-func "parseProjectUnit") (process-java-ast arg1 arg2))
	(t t) ; for now
  ))
(defun jlinker-end () t)

; jnil.ast::*jcall-count*

(defvar *jcall-count* 0)
(defparameter *jcalls* ; most of these are called in one place.
  (load-time-value
    '(("getName" . (lambda (arg) (cerror "not implemented")))
      ("toString" . (lambda (arg) (cerror "not implemented")))
      ("resolveBinding" . (lambda (arg) (cerror "not implemented")))
      ("arguments" . (lambda (arg) (slot-value arg 'jnil.ast::arguments)))
      ("getOperator" . (lambda (arg) (cerror "not implemented")))
      ("fragments" . (lambda (arg) (cerror "not implemented")))
      ("statements" . (lambda (arg) (slot-value arg 'jnil.ast::statements)))
      ("getKind" . (lambda (arg) (cerror "not implemented")))
      ("getIdentifier" . (lambda (arg) (cerror "not implemented")))
      ("bodyDeclarations" . (lambda (arg) (cerror "not implemented")))
      ("updaters" . (lambda (arg) (cerror "not implemented")))
      ("types" . (lambda (arg) (slot-value arg 'jnil.ast::types)))
      ("thrownExceptions" . (lambda (arg) (slot-value arg 'jnil.ast::thrownexceptions)))
      ("superInterfaces" . (lambda (arg) (slot-value arg 'jnil.ast::superinterfaces)))
      ("setProperty" . (lambda (arg2 arg3 arg4)
			 (format t "~&setProperty ~a ~a ~a~%" (type-of arg2) arg3 arg4)
			 (setf (set-property arg2) (acons arg3 arg4 (get-property arg2)))))
      ("parameters" . (lambda (arg) (slot-value arg 'jnil.ast::parameters)))
      ("next" . (lambda (arg) (cerror "not implemented")))
      ("main" . (lambda (arg) (cerror "not implemented")))
      ("listIterator" . (lambda (arg) (cerror "not implemented")))
      ("isFromSource" . (lambda (arg) nil)) ;See if this works for now
      ("initializers" . (lambda (arg) (cerror "not implemented")))
      ("imports" . (lambda (arg) (slot-value arg 'jnil.ast::imports)))
      ("hasNext" . (lambda (arg) (cerror "not implemented")))
      ("getTypes" . (lambda (arg) (slot-value arg 'jnil.ast::types)))
      ("getReturnType" . (lambda (arg) (cerror "not implemented")))
      ("getProperty" . (lambda (arg2 arg3)
			 (format t "~&getProperty ~a ~a~%" (type-of arg2) arg3)
			 (let ((p (assoc arg3 (get-property arg2))))
			   (format t "~&getProperty returns ~a cdr ~a~%" p (cdr p))
			   (cdr p))))
      ("getProject" . (lambda (arg) (cerror "not implemented")))
      ("getParameterTypes" . (lambda (arg) (cerror "not implemented")))
      ("getModifiers" . (lambda (arg) (cerror "not implemented")))
      ("getMethods" . (lambda (arg) (slot-value arg 'jnil.ast::methods)))
      ("getJavaProject" . (lambda (arg) (cerror "not implemented")))
      ("getJavaElement" . (lambda (arg) (cerror "not implemented")))
      ("getInterfaces" . (lambda (arg) (cerror "not implemented")))
      ("getFields" . (lambda (arg) (slot-value arg 'jnil.ast::fields)))
      ("getElementType" . (lambda (arg) (cerror "not implemented")))
      ("getDeclaredMethods" . (lambda (arg) (cerror "not implemented")))
      ("extendedOperands" . (lambda (arg) (cerror "not implemented")))
      ("expressions" . (lambda (arg) (cerror "not implemented")))
      ("dimensions" . (lambda (arg) (cerror "not implemented")))
      ("catchClauses" . (lambda (arg) (cerror "not implemented"))))))
