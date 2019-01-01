(defparameter *definitions-use-hash-table* nil
  "If not null, then definitions are stored in hash tables, else
   an association list. Alist is better for debugging as it prints.")

;; a definition is a symbol, zero or more parameters and an expression
;; which is evaluated in the context of the parameters and any higher level environment/context
(defstruct definition
  symbol
  parameters
  expression)

;; expression is a list zero or more of definitions
;; and a final form which is evaluated in the context of the definitions
;; to get the result
(defstruct expression
  definitions
  final-form)

;; form is a symbol followed by zero or more arguments
;; or a constant
(defstruct form
  type ; 'symbol 'constant
  value ; the symbol
  args)

;; an argument which is a constant
(defstruct constant-arg
  value)

;; an argument which is a symbol
(defstruct symbol-arg
  value)

;; an environment
(defstruct environment
  definitions
  parent-env)

(defun get-definition (symbol defs)
  (if *definitions-use-hash-table*
      (gethash symbol defs)
      (cdr (assoc symbol defs :test #'string=))))

(defun create-definitions (defs &optional (ht nil))
  "Create a hash table or alist of the definitions"
  (if (not *definitions-use-hash-table*)
      (map 'list (lambda (d) (cons (definition-symbol d) d)) defs)
      (let ((ht2 (or ht (make-hash-table :test #'equal))))
	(if (null defs)
	    ht2
	    (progn
	      (setf (gethash (definition-symbol (first defs)) ht2) (first defs))
	      (create-definitions (rest defs) ht2))))))

(defun make-parameter-definitions (parameters argument-values)
  "Makes a hash table or alist of the parameters and their associated values as arguments."
  (if *definitions-use-hash-table*
      (let ((pdefs (make-hash-table :test #'equal)))
	(loop
	   for p in parameters
	   for a in argument-values
	   do
	     (setf (gethash (cdr p) pdefs) a))
	pdefs)
      (map 'list (lambda (p a) (cons (cdr p) a)) parameters argument-values)))
