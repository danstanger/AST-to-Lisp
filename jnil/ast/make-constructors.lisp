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
         (new-args (append default-args '((child nil child-supplied-p)))))
  `(progn
     (defun ,klass-constructor-name
            (&key ,@(loop for a in new-args collect a))
      (let ((sn (make-instance ',klass)))
       (when child-supplied-p (setf (slot-value child 'node-parent) sn))
       ,@(loop for a in args collect `(setf (slot-value sn ',a) ,a))
       sn)))))

(make-constructor abstract-type-declaration node-parent declaration-name )
(make-constructor anonymous-class-declaration node-parent )
(make-constructor array-access node-parent array-access-array array-access-index )
(make-constructor array-creation node-parent array-creation-initializer array-creation-type )
(make-constructor array-initializer node-parent)
(make-constructor array-type node-parent)
(make-constructor assignment node-parent assignment-left-hand-side assignment-right-hand-side node-parent)
(make-constructor body-declaration node-parent declaration-modifiers )
(make-constructor boolean-literal node-parent literal-value )
(make-constructor break-statement node-parent statement-label )
(make-constructor cast-expression node-parent cast-expression-expression cast-expression-type )
(make-constructor catch-clause node-parent clause-body clause-exception)
(make-constructor character-literal node-parent literal-value )
(make-constructor class-instance-creation node-parent instance-creation-anonymous-decl instance-creation-expression instance-creation-name instance-creation-binding )
(make-constructor conditional-expression node-parent expression-test expression-then expression-else )
(make-constructor constructor-invocation node-parent)
(make-constructor continue-statement node-parent statement-label )
(make-constructor do-statement node-parent statement-body statement-expression )
(make-constructor empty-statement node-parent)
(make-constructor expression node-parent expression-type-binding )
(make-constructor expression-statement node-parent expression )
(make-constructor field-access node-parent field-access-expression field-access-name )
(make-constructor field-declaration node-parent declaration-type )
(make-constructor for-statement node-parent statement-body statement-expression )
(make-constructor ifstatement node-parent ifstatement-test ifstatement-then ifstatement-else )
(make-constructor infix-expression node-parent expression-right-operand extended-operands-p )
(make-constructor initializer node-parent initializer-body )
(make-constructor instanceof-expression node-parent expression-left-operand expression-right-operand)
(make-constructor labeled-statement node-parent statement-label statement-body )
(make-constructor method-binding binding-declaring-class)
(make-constructor method-declaration node-parent declaration-name parameters declaration-return-type name thrownexceptions)
(make-constructor method-invocation node-parent invocation-name invocation-expression invocation-method-binding)
(make-constructor null-literal node-parent)
(make-constructor number-literal node-parent literal-token )
(make-constructor package-binding binding-identifier binding-kind binding-modifiers binding-name)
(make-constructor parenthesized-expression node-parent parenthesized-expression-expression )
(make-constructor postfix-expression node-parent expression-operand)
(make-constructor prefix-expression node-parent expression-operand )
(make-constructor qualified-name node-parent qualified-name-qualifier qualified-name-name )
(make-constructor return-statement node-parent statement-expression)
(make-constructor simple-name node-parent name-string name-binding)
(make-constructor simple-type node-parent node-type type-binding)
(make-constructor single-variable-declaration node-parent declaration-name declaration-initializer declaration-type)
(make-constructor statement node-parent )
(make-constructor string-literal node-parent literal-value )
(make-constructor super-constructor-invocation node-parent )
(make-constructor super-field-access node-parent)
(make-constructor super-method-invocation node-parent invocation-qualifier invocation-name )
(make-constructor switch-case node-parent case-expression )
(make-constructor switch-statement node-parent switch-expression )
(make-constructor this-expression node-parent)
(make-constructor throw-statement node-parent throw-expression )
(make-constructor try-statement node-parent)
(make-constructor type-binding binding-name)
(make-constructor type-declaration node-parent declaration-name fields methods superinterfaces declaration-superclass types)
(make-constructor type-literal node-parent)
(make-constructor variable-binding binding-declaring-class binding-identifier binding-field-p binding-type-binding binding-modifiers binding-name)
(make-constructor variable-declaration-fragment node-parent declaration-name declaration-initializer declaration-binding)
(make-constructor while-statement node-parent statement-expression statement-body )
