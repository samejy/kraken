(defun make-builtins ()
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "+" ht) #'+)
    (setf (gethash "-" ht) #'-)
    (setf (gethash "*" ht) #'*)
    (setf (gethash "/" ht) #'/)
    (setf (gethash "=" ht) #'equalp)
    (setf (gethash "<" ht) #'<)
    (setf (gethash ">" ht) #'>)
    (setf (gethash ">=" ht) #'>=)
    (setf (gethash "<=" ht) #'<=)
    (setf (gethash "sqrt" ht) #'sqrt)
    (setf (gethash "if" ht) #'kn-if)
    ht))

(defparameter *builtins* (make-builtins))


(defgeneric eval-argument (arg local global))

(defmethod eval-argument (arg local global)
  (declare (optimize (debug 3)))
  (break)
  (error "Cannot eval unknown argument"))

(defmethod eval-argument ((argument symbol-arg) local-env global-env)
  (cdr (find-symbol-in-envs (symbol-arg-value argument) local-env global-env)))

(defmethod eval-argument ((argument symbol-arg) local-env global-env)
  (let* ((symb-val (cdr (find-symbol-in-envs (symbol-arg-value argument) local-env global-env))))
    (typecase symb-val
      (definition 
       (progn
	 (if (null (definition-parameters symb-val))
		(eval-expression (definition-expression symb-val) local-env global-env)
		symb-val)))
      (t symb-val))))

(defmethod eval-argument ((argument constant-arg) local-env global-env)
  (constant-arg-value argument))

(defmethod eval-argument ((argument expression) local-env global-env)
  (eval-expression argument local-env global-env))

(defun apply-builtin (fn-name args-values-list local global)
  (let ((builtin (gethash fn-name *builtins*))
	;; these will probably not be necessary:
	(*builtin-local-env* local)
	(*builtin-global-env* global))
    (apply builtin args-values-list)))

(defun apply-fn (fn-def args-values-list local-env global-env symbol-context)
  (declare (optimize (debug 3)))
  (let* ((new-local-env (make-environment
			 :definitions (make-parameter-definitions (definition-parameters fn-def) args-values-list)
			 ;; only set parent env if function is in local context, else it gets a fresh context with only the parameter definitions
			 :parent-env (when (eq symbol-context 'local)
					 local-env)))) 
    (eval-expression (definition-expression fn-def) new-local-env global-env)))

(defun eval-symbol-with-args (symbol args-values local-env global-env)
  (if (gethash symbol *builtins*)
      (apply-builtin symbol args-values local-env global-env)
      (let* ((symb (find-symbol-in-envs symbol local-env global-env))
	     (symb-loc (car symb))
	     (fn-def (cdr symb)))
	(if (and (not (null (definition-parameters fn-def)))
		 (null args-values))
	    fn-def ;; return the definition un-evaluated
	    ;; else try and apply the symbol as a function
	    ;; TODO - partial application
	    (apply-fn fn-def args-values local-env global-env symb-loc)))))

(defun eval-form (form local-env global-env)
  (declare (optimize (debug 3)))
  (let* ((args (form-args form))
	 (args-values (map 'list (lambda (a) (eval-argument a local-env global-env)) args))
	 (value (form-value form))
	 (form-type (form-type form)))
    (case form-type
      (bool value)
      (number value)
      (symbol (eval-symbol-with-args value args-values local-env global-env)))))

(defun eval-expression (expression par-local-env global-env)
  (declare (optimize (debug 3)))
  (let* ((form (expression-final-form expression))
	 (exp-definitions (expression-definitions expression))
	 (local-env (make-environment
		     :definitions exp-definitions
		     :parent-env par-local-env)))
    (eval-form form local-env global-env)))

(defun find-symbol-in-envs (symb local env)
  "Lookup a symbol in the environments. If found returns a cons with a car either 'local or 'global specifying which environment it was found in, and a cdr with the symbol value. Else it return nil."
  (let* ((symb-value-local (find-symbol-in-environment symb local))
	 (symb-value-global (find-symbol-in-environment symb env))
	 (symb-value (cond ((and symb-value-local) (cons 'local symb-value-local))
			   ((and symb-value-global) (cons 'global symb-value-global))
			   (t nil))))
    (if symb-value
	symb-value
	(error (concatenate 'string "Unable to find symbol " symb)))))

(defun find-symbol-in-environment (symb env)
  (let* ((env-defs (environment-definitions env))
	 (symb-value (and env-defs (get-definition symb env-defs)))
	 (parent-env (environment-parent-env env)))
    (if symb-value
	symb-value
	(when parent-env
	    (find-symbol-in-environment symb parent-env)))))


(defun run-main (top-level-definitions)
  (declare (optimize (debug 3)))
  (let ((main (get-definition ".main" top-level-definitions))
	(global-env (make-environment
	      :definitions top-level-definitions 
	      :parent-env nil))
	(local-env (make-environment
		    ;; todo - parameters....
		    :definitions nil
		    :parent-env nil)))
    (if (null main)
	(error "Cannot run. No .main found.")
	(eval-expression (definition-expression main) local-env global-env))))

(defun interpret (source)
  (declare (optimize (debug 3)))
  (let* ((src-tree (parse-source source)))
    (run-main src-tree)))


;; (defun interpret-file (file)
;;   (with-open-file ))
