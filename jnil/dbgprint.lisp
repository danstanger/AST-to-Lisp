; This is throwaway code, so its ok to use some special cases
(defun dbgprint (x &optional (d 0) (sdn ""))
  (when (> d 8) (return-from dbgprint nil))
  (cond ((or (typep x 'jnil.ast::node) (typep x 'jnil.ast::binding))
	(let* ((co (class-of x))
	       (cn (closer-mop::class-name co))
	       (cs (closer-mop::class-slots co)))
	  (format t "~&~v,1@t~a ~a~%" d sdn cn)
	  (dolist (s cs)
	    (let* ((sdn (slot-definition-name s))
		   (sn (symbol-name sdn))
		   (v (if (slot-boundp x sdn) (slot-value x sdn) "unbound")))
	      (cond ((string-equal sn "PROPERTIES") ; Special case, dont recurse
		     (format t "~&~v,1@t~a ~a~%" (1+ d) sn v))
		    ((string-equal sn "NODE-PARENT") ; Special case, dont recurse
		     (format t "~&~v,1@t~a ~a~%" (1+ d) sn (closer-mop::class-name (class-of v))))
		    ((string-equal sn "BINDING-DECLARING-CLASS") ; Special case, dont recurse
		     (format t "~&~v,1@t~a ~a~%" (1+ d) sn (closer-mop::class-name (class-of v))))
		    (t (dbgprint v (1+ d) sdn)))))))
	((listp x) (dolist (o x) (dbgprint o d sdn)))
	(t (format t "~&~v,1@t~a ~a~%" d sdn x))))


