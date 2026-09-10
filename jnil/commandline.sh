clisp -q -repl -i asdf -x "(asdf:operate 'asdf:load-source-op \"jnil\")" -x "(javatools.jlinker::read-java-ast nil nil)" -x "(setf *x* (apply #'jnil.ast::process nil javatools.jlinker::*java-ast*))" -x "(setf *c* (jnil::jnil-generate-code *x* :common-lisp t nil))"

clisp -q -repl -i asdf -x "(asdf:operate 'asdf:load-source-op \"jnil\")" -x "(javatools.jlinker::read-java-ast nil nil)" -x "(setf *x* (apply #'jnil.ast::process nil javatools.jlinker::*java-ast*))" -x "(load \"dbgprint\")" -x "(dbgprint *x*)"
