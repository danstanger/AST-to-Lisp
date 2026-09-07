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
;;; Description: Parse sexpressions produced by Antlr 3 Java Parser
;;;

(in-package #:jnil.ast)
;(setf *print-length* 4)
(defmacro safe-push (c slot td)
  `(if (slot-boundp ,c ',slot)
       (push ,td (slot-value ,c ',slot))
       (setf (slot-value ,c ',slot) (list ,td))))

(defclass triple ()
  ((mo :initarg :mo-i :reader triple-mo)
   (ti :initarg :ti-i :reader triple-ti)
   (ty :initarg :ty-i :reader triple-ty)))

(defun make-triple (&key mo ti ty) (make-instance 'triple :mo-i mo :ti-i ti :ty-i ty))

(defgeneric process (java-class eqarg &rest r)
  (:documentation "Parses java ast output")
  (:method (c eqarg &rest r)
    (format t "~&defgeneric process ~a ~a ~a~%" c eqarg r)
    ;    (break)
    (format t "~&defgeneric process symbol ~a name ~a package ~a ~%"
	    eqarg
	    (symbol-name eqarg)
	    (symbol-package eqarg))))

;; Handle package and possible multiple classes. Not sure if jnil can handle
;; more than one class in a compilation unit.
(defmethod process (c (eqjs (eql 'compilation-unit)) &rest r)
  (format t "~&process~a ~a~%" eqjs r)
  (let ((cu (make-instance 'compilation-unit)))
    ;    (print cu)
    ;    (print (slot-value cu 'jnil.ast::eclipse-name))
    (setf (slot-value cu 'jnil.ast::unit-package) nil)
    (setf (slot-value cu 'jnil.ast::imports) nil)
    (setf (slot-value cu 'jnil.ast::types) nil)
    (setf (slot-value cu 'jnil.ast::node-parent) nil)
    (setf (slot-value cu 'jnil.ast::node-type) 'jnil.ast::+eclipse-compilation-unit+)
    (dolist (s r)
      (format t "~&process java source dolist s ~a~%" s)
      (apply #'process cu s)
      ;(describe cu)
      )
    cu))

(defmethod process (c (eqcl (eql 'jnil.ast::class)) &rest r)
  (format t "~&process~a ~a~%" eqcl r)
  (destructuring-bind (m n r1) r
    (when (listp m) (apply #'process c m))
    (format t "~&name ~a type-of name ~a~%" n (type-of n))
    (let* ((sn (make-simple-name :node-parent nil :name-string (symbol-name n) :name-binding nil))
	   (td (make-type-declaration :node-parent c :declaration-name sn :child sn)))
      (apply #'process td r1)
      (safe-push c jnil.ast::types td))))

(defmethod process (c (eqctls (eql 'jnil.ast::class_top_level_scope)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqctls (type-of c) r)
  (dolist (e r)
    (format t "~&process element ~a~%" e)
    (apply #'process c e)))

(defmethod process (td (eqvmd (eql 'jnil.ast::void_method_decl)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqvmd (type-of td) r)
  (destructuring-bind (m n fpl bs) r
    (format t "~&process~a m ~a n ~a fpl ~a~%bs ~a ~%" eqvmd m n fpl bs)
    (let ((md (make-instance 'jnil.ast::method-declaration))
	  (sn (make-instance 'jnil.ast::simple-name)))
      (setf (slot-value md 'jnil.ast::node-parent) td)
      (setf (slot-value sn 'jnil.ast::node-parent) td) ;shouldn't this be md
      (setf (slot-value sn 'jnil.ast::name-string) (string n))
      (setf (slot-value sn 'jnil.ast::name-binding) nil)
      (setf (slot-value md 'jnil.ast::declaration-name) nil) ;fixme: create an initial value
      (setf (slot-value md 'jnil.ast::parameters) nil) ;fixme: create an initial value
      (setf (slot-value md 'jnil.ast::thrownexceptions) nil) ;fixme: create an initial value
      (when (listp m) (apply #'process md m))
      (setf (slot-value md 'jnil.ast::declaration-return-type)
	    (process-return-type "void"))
      (setf (slot-value md 'jnil.ast::name) sn)
      (push md (slot-value td 'jnil.ast::methods))
      (format t "~&fpl ~a~%" fpl)
      (apply #'process md fpl)
      (apply #'process md bs))))

(defmethod process (td (eqfmd (eql 'jnil.ast::function_method_decl)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqfmd (type-of td) r)
  (destructuring-bind (ml ty n fpl bs) r
    (format t "~&process~a ml ~a n ~a fpl ~a~%bs ~a ~%" eqfmd ml n fpl bs)
    (let* ((sn (make-simple-name :name-string (string n) :name-binding nil  ))
	   (md (make-method-declaration :node-parent td :child sn :declaration-name sn :name sn :thrownexceptions nil)))
      (setf (slot-value md 'jnil.ast::parameters) nil) ;fixme: create an initial value
      (when (listp ml) (apply #'process md ml))
      (setf (slot-value md 'jnil.ast::declaration-return-type)
	    (process-return-type "void"))
      (push md (slot-value td 'jnil.ast::methods))
      (format t "~&fpl ~a~%" fpl)
      (when (listp fpl) (apply #'process md fpl))
      (apply #'process md bs))))

(defmethod process (c (eqpa (eql 'package-declaration)) &rest r)
  (format t "~&process~a ~a~%" eqpa r)
  (labels ((pkg (i &optional (acc nil))
	     (cond ((null i) acc)
		   ((atom i) (push i acc) acc)
		   (t (pkg (second i) (push (third i) acc))))))
    (let* ((pd (make-instance 'jnil.ast::package-declaration))
	   (sn (make-simple-name :node-parent pd :name-string (pkg (first r)) :name-binding nil)))
      (setf (slot-value pd 'jnil.ast::node-parent) c)
      (setf (slot-value pd 'jnil.ast::declaration-name) sn)
      (setf (slot-value c 'jnil.ast::unit-package) pd))))

(defmethod process (c (eqim (eql 'jnil.ast::import-declaration)) &rest r)
  (format t "~&process~a ~a~%" eqim r)
  (labels ((allexports (x) (eql '.* x))
	   (imp (i &optional (acc nil))
	     (cond ((null i) acc)
		   ((atom i) (push i acc) acc)
		   (t (imp (second i) (push (third i) acc))))))
    (let ((id (make-instance 'jnil.ast::import-declaration))
	  (nm (make-instance 'jnil.ast::name)))
      (setf (slot-value id 'jnil.ast::node-parent) c)
      (setf (slot-value nm 'jnil.ast::node-parent) id)
      (setf (slot-value nm 'jnil.ast::name-string) (format nil "~{~a~^.~}" (imp (first r))))
      (setf (slot-value id 'jnil.ast::declaration-name) nm)
      (setf (slot-value id 'jnil.ast::on-demand-p) (allexports (second r)))
      (safe-push c jnil.ast::imports id))))

(defmethod process (c (eqml (eql 'jnil.ast::modifier_list)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqml (type-of c) r)
  (destructuring-bind (n &rest ml) r ; Ignore the nul
    (when ml
      (dolist (m ml)
	(format t "~a~%" m)))
    t))

(defmethod process (td (eqvd (eql 'jnil.ast::var_declaration)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqvd (type-of td) r)
      (setf *verbose-output* 1)
  (destructuring-bind (mo ty vdl) r
    (let* ((ti (apply #'process nil ty)))
      (format t "~&process~a modifier ~a~%" eqvd mo)
      (format t "~&process~a tipe ~a~%" eqvd ti)
      (format t "~&process~a type-binding ~a~%" eqvd (type-binding ti))
      (format t "variable vdl ~a~%" vdl)
      (setf *verbose-output* 0)
      (apply #'process (make-triple :mo mo :ti ti :ty td) vdl))))

(defmethod process (tr (eqvdl (eql 'jnil.ast::var_declarator_list)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqvdl (type-of tr) r)
  (format t "~&process~a ~a~%" eqvdl r)
    (dolist (v r)
      (apply #'process tr v)))

(defmethod process (tr (eqvdr (eql 'jnil.ast::var_declarator)) &rest r)
  (format t "~&process~a class-arg ~a |~a|~%" eqvdr (type-of tr) r)
  (format t "~&process~a ~a~%" eqvdr r)
  ;(break)
  (destructuring-bind (v &rest e) r
    (let* ((ti (triple-ti tr))
	   (td (triple-ty tr))
	   (vn (symbol-name v))
	   (tb (type-binding ti))
	   (sn (make-simple-name :name-string vn :name-binding nil))
	   (bi (make-variable-binding :binding-declaring-class (get-declaring-class td) :binding-identifier nil :binding-field-p nil :binding-type-binding tb :binding-modifiers 0 :binding-name vn))
	   (fr (make-variable-declaration-fragment :node-parent td :declaration-name sn :declaration-initializer nil :declaration-binding bi :child sn)))
      (safe-push td jnil.ast::fields fr) ; This is wrong, need to pass it up to add the type
      ;(break)
      fr)))

(defmethod process (md (eqfpl (eql 'jnil.ast::formal_param_list)) &rest r)
  (format t "~&process~a ~a~%" eqfpl r)
  (dolist (fpsd r)
    (format t "~&process~a ~a~%" eqfpl fpsd)
    (apply #'process md fpsd)))

(defmethod process (md (eqfpsd (eql 'jnil.ast::formal_param_std_decl)) &rest r)
  (format t "~&process~a ~a~%" eqfpsd r)
  (destructuring-bind (lml ty v &rest unex) r
    (when (and (boundp 'unex) unex) (warn "Unexpected fourth value in parameter processing ~a" unex))
    (format t "~&variable name ~a~%" v)
    (format t "~&variable type ~a~%" ty)
    (let* ((sn (if (listp v) (apply #'process nil v)
		   (make-simple-name :name-string (symbol-name v))))
	   (td (apply #'process nil ty))
	   (svd (make-single-variable-declaration :node-parent md
						  :declaration-name sn
		                                  :declaration-initializer nil
			                          :declaration-type td
		                                  :child td)))
    (apply #'process svd r)
    (safe-push md jnil.ast::parameters svd))))

; I expect that only the final modifier will appear here, which is ignored.
; Annotations are killed in the parser.
(defmethod process (svd (eqlml (eql 'jnil.ast::local_modifier_list)) &rest r)
  (format t "~&process~a ~a~%" eqlml r)
   (destructuring-bind (n &rest lml) r
    (when lml
      (dolist (m lml)
	(format t "~a~%" m)))
    t))

; It may be that this processing is called with a few different class types.
; So return values instead of assigning them.
; begining of TYPE processing
(defmethod process (obj (eqt (eql 'jnil.ast::tipe)) &rest r)
  (format t "~&process~a ~a~%" eqt r)
  (destructuring-bind (ty &optional ar &rest unex) r
    (when (and (boundp 'unex) unex) (warn "Unexpected third value in TYPE processing ~a" unex))
    ; fix return values.
    (let ((ty (apply #'process nil ty))
	  (at (when ar (apply #'process nil ar))))
      (cond ((boundp 'at) (setf (slot-value at 'jnil.ast::element-type) ty) ar)
	    (t ty)))))

(defmethod process (obj (eqqti (eql 'jnil.ast::qualified_type_ident)) &rest r)
  (format t "~&process~a ~a~%" eqqti r)
  (let* ((sns (format nil "~@:(~{~a~^-~}~)" r))
	 (sn (make-simple-name :node-parent nil :name-string sns :name-binding nil))
	 (tb (make-type-binding :binding-name sns))
	 (st (make-simple-type :node-parent obj :node-type sn :child sn :type-binding tb)))
    ;(intern (format nil "~@:(~{~a~^-~}~)" r) *package*))
    st))

(defmethod process (nil-obj (eqqti (eql 'jnil.ast::array_declarator_list)) &rest r)
  (format t "~&process~a ~a~%" eqqti r)
  (apply #'process nil-obj r))

; fixme need to pass in valid node for parent setup
(defmethod process (nil-obj (eqad (eql 'jnil.ast::array_declarator)) &rest r)
  (format t "~&process~a ~a~%" eqad r)
  (let ((ar (make-instance 'jnil.ast::array-type)))
    ar))

(defmethod process (md (eqal (eql 'jnil.ast::argument_list)) &rest r)
  (format t "~&process~a ~a~%" eqal r)
  (dolist (a r) (apply #'process md a)))

(defmethod process (md (eqbs (eql 'jblock)) &rest r)
  (format t "~&process~a ~a~%" eqbs r)
  (let ((jb (make-instance 'jblock)))
    (setf (slot-value jb 'jnil.ast::node-parent) md)
    (if (atom (first r))
	(progn (warn "block_scope atom ~a" (first r)) (apply #'process md r))
	(dolist (s r)
	  (setf (slot-value md 'jnil.ast::declaration-body) jb)
	  (apply #'process jb s)))))

(defmethod process ((jb jnil.ast::jblock) (eqes (eql 'jnil.ast::expr)) &rest r)
  (format t "~&process~a block specialization ~a~%" eqes r)
  (destructuring-bind (e) r
    (format t "~&destructuring-bind value ~a~%" e)
    (let ((es (make-instance 'jnil.ast::expression-statement)))
      (setf (slot-value es 'jnil.ast::node-parent) jb)
      (apply #'process es e)
      (safe-push jb jnil.ast::statements es))))

; Need to create return statement first to specialize on it.  If parser
; returned an expression vs an expression statement, then this could be
; reversed.
(defmethod process (jb (eqrs (eql 'return-statement)) &rest r)
  (format t "~&process~a ~a~%" eqrs r)
  (destructuring-bind (e) r
    (format t "~&destructuring-bind value ~a~%" e)
    (let* ((rs (make-return-statement :node-parent jb))
	   (ex (apply #'process rs e)))
      (setf (slot-value rs 'jnil.ast::statement-expression) ex)
      (safe-push jb jnil.ast::statements rs))))

(defmethod process ((rs jnil.ast::return-statement) (eqe (eql 'jnil.ast::expr)) &rest r)
  (format t "~&process~a return specialization ~a~%" eqe r)
  (destructuring-bind (e) r
    (format t "~&destructuring-bind value ~a~%" e)
    (if (atom e) (make-simple-name :node-parent rs :name-string (format nil "~a" e) :name-binding nil)
	(error "~&process~a return specialization ~a not implemented~%" eqe r))))

(defmethod process (mi (eqe (eql 'jnil.ast::expr)) &rest r)
  (format t "~&process~a no specialization ~a~%" eqe r)
  (destructuring-bind (e) r
    (format t "~&destructuring-bind value ~a~%" e)
    (let ((es (make-instance 'jnil.ast::expression-statement)))
      (setf (slot-value es 'jnil.ast::node-parent) mi)
      ;; This should be handled by the parser not here.
      (typecase e
	(number (format t "It is an integer: ~d" e)) ;fixme
	(string (let ((sl (make-string-literal :node-parent es :literal-value e)))
		  (setf (slot-value es 'jnil.ast::expression) sl)
		  (safe-push mi jnil.ast::arguments es))) ;fixme
	; If name binding is needed, then binding needs to be accessable
	; since it was created when the field was processed.
	(symbol (let ((sn (make-simple-name :node-parent es :name-string (symbol-name e) :name-binding nil)))
		  (setf (slot-value es 'jnil.ast::expression) sn)
		  (safe-push mi jnil.ast::arguments es))) ;fixme
	(t      (apply #'process es e))))))

(defmethod process ((es jnil.ast::expression-statement) (eqmc (eql 'jnil.ast::method_call)) &rest r)
  (format t "~&process~a expression statement specialization ~a~%" eqmc r)
  (destructuring-bind (m al) r
    (format t "~&method ~a argument list ~a~%" m al)
    (let* ((qm (fqn m))
	   (bqm (format nil "~{~a~^-~}" (butlast qm 1)))
	   (sns (format nil "~@:(~{~a~^-~}~)" qm))
	   (sn (make-simple-name :name-string sns :name-binding nil))
	   (mb (make-method-binding))
	   (mi (make-method-invocation :node-parent es :invocation-method-binding mb :invocation-expression sn :invocation-name sn :child sn))
	   (tbsn (make-simple-name :node-parent nil :name-string bqm :name-binding nil))
	   (tb (make-type-binding :binding-name tbsn ))
	   )
      (setf (slot-value es 'jnil.ast::expression) mi)
      (setf (slot-value mb 'jnil.ast::binding-declaring-class) tb)
      (format t "~&qualified method ~a~%" qm)
      (format t "~&argument list from destructuring bind ~a~%" al)
      (apply #'process mi al))
    (format t "~&after call~%")
    ))

; (labels ((modifiers (m &optional (acc nil)))
; (trace process)
; (process-java-ast nil nil)
;(format t "~&~&first java-ast is ~A~%" *java-ast*)
;(print (apply #'process nil *java-ast* ))
(defun fqn (i &optional (acc nil))
  (cond ((null i) acc)
	((atom i) (push i acc) acc)
	(t (fqn (second i) (push (third i) acc)))))
;(trace fqn)
(defun process-return-type (arg)
  (let ((f (coerce (cdr (assoc arg *primitive-types* :test #'equal)) 'function)))
    (if f (let ((p (funcall f)))
	    (setf (slot-value p 'jnil.ast::primitivetypecode) arg) p)
	(progn (warn "not implemented ~a~%" arg) (break) nil))))

(defun get-declaring-class (c)
  (if (null c) nil
      (let ((p (slot-value c 'jnil.ast::node-parent)))
        (if (equal (class-of c) (find-class 'jnil.ast::type-declaration)) c
	    (get-declaring-class p)))))
(trace get-declaring-class)

(defparameter *primitive-types*
  (load-time-value
    '(("byte" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("short" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("char" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("int" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("long" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("float" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("double" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("boolean" . (lambda () (make-instance 'jnil.ast::primitive-type)))
      ("void" . (lambda () (make-instance 'jnil.ast::primitive-type))))))
