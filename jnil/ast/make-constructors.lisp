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
;;; Description: Create Eclipse classes
;;;

(in-package #:jnil.ast)
(defmacro make-constructor (klass &rest args)
  (let* ((klass-constructor-name (intern (format nil "MAKE-~a" (symbol-name klass))))
	 (default-args (loop for a in args collect (list a nil)))
         (new-args (append default-args
			   '((node-parent nil node-parent-supplied-p)
			     (children nil children-supplied-p)
			     (child nil child-supplied-p)))))
  `(progn
     (defun ,klass-constructor-name
            (&key ,@(loop for a in new-args collect a))
      (let ((sn (make-instance ',klass)))
	(when node-parent-supplied-p
	  (setf (slot-value sn 'node-parent) node-parent))
	(when child-supplied-p
	  (setf (slot-value child 'node-parent) sn))
	(when children-supplied-p
	  (loop for c in children do (setf (slot-value c 'node-parent) sn)))
       ,@(loop for a in args collect `(setf (slot-value sn ',a) ,a))
       sn)))))

(make-constructor abstract-type-declaration declaration-name )
(make-constructor anonymous-class-declaration )
(make-constructor array-access array-access-array array-access-index )
(make-constructor array-creation array-creation-initializer array-creation-type )
(make-constructor array-initializer)
(make-constructor array-type)
(make-constructor assignment assignment-left-hand-side assignment-right-hand-side)
(make-constructor body-declaration declaration-modifiers )
(make-constructor boolean-literal literal-value )
(make-constructor break-statement statement-label )
(make-constructor cast-expression cast-expression-expression cast-expression-type )
(make-constructor catch-clause clause-body clause-exception)
(make-constructor character-literal literal-value )
(make-constructor class-instance-creation instance-creation-anonymous-decl instance-creation-expression instance-creation-name instance-creation-binding )
(make-constructor compilation-unit unit-package imports types node-type )
(make-constructor conditional-expression expression-test expression-then expression-else )
(make-constructor constructor-invocation)
(make-constructor continue-statement statement-label )
(make-constructor do-statement statement-body statement-expression )
(make-constructor empty-statement)
(make-constructor expression expression-type-binding )
(make-constructor expression-statement expression )
(make-constructor field-access field-access-expression field-access-name )
(make-constructor field-declaration declaration-type )
(make-constructor for-statement statement-body statement-expression )
(make-constructor ifstatement ifstatement-test ifstatement-then ifstatement-else )
(make-constructor infix-expression expression-left-operand expression-right-operand extended-operands-p operator)
(make-constructor initializer initializer-body )
(make-constructor instanceof-expression expression-left-operand expression-right-operand)
(make-constructor jblock)
(make-constructor labeled-statement statement-label statement-body )
(make-constructor method-binding binding-declaring-class)
(make-constructor method-declaration declaration-name parameters declaration-return-type name thrownexceptions)
(make-constructor method-invocation invocation-name invocation-expression invocation-method-binding)
(make-constructor null-literal)
(make-constructor number-literal literal-token expression-type-binding)
(make-constructor package-binding binding-identifier binding-kind binding-modifiers binding-name)
(make-constructor package-declaration declaration-name)
(make-constructor parenthesized-expression parenthesized-expression-expression )
(make-constructor postfix-expression expression-operand)
(make-constructor prefix-expression expression-operand )
(make-constructor primitive-type primitive-type-code type-binding)
(make-constructor qualified-name qualified-name-qualifier qualified-name-name )
(make-constructor return-statement statement-expression)
(make-constructor simple-name name-string name-binding)
(make-constructor simple-type node-type type-binding)
(make-constructor single-variable-declaration declaration-name declaration-initializer declaration-type)
(make-constructor statement )
(make-constructor string-literal literal-value expression-type-binding)
(make-constructor super-constructor-invocation )
(make-constructor super-field-access)
(make-constructor super-method-invocation invocation-qualifier invocation-name )
(make-constructor switch-case case-expression )
(make-constructor switch-statement switch-expression )
(make-constructor this-expression)
(make-constructor throw-statement throw-expression )
(make-constructor try-statement)
(make-constructor type-binding binding-name binding-superclass)
(make-constructor type-declaration node-type declaration-name fields methods superinterfaces declaration-superclass types)
(make-constructor type-literal)
(make-constructor variable-binding binding-declaring-class binding-identifier binding-field-p binding-type-binding binding-modifiers binding-name)
(make-constructor variable-declaration-fragment declaration-name declaration-initializer declaration-binding)
(make-constructor while-statement statement-expression statement-body )
