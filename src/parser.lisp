(ql:quickload 'smug)
(ql:quickload 'cl-ppcre)

(defun whitespacep (c)
  (member c '(#\Space #\Tab #\Linefeed) :test #'char=))

(defun parse-whitespace (&optional (at-least 0))
  (smug:.first (smug:.map 'string (smug:.is #'whitespacep) :at-least at-least)))

(defun terminator-p (c)
  (or (char= c #\;)
      (char= c #\})
      (whitespacep c)))

(defun equals-sign-p (c)
  (char= c #\=))

(defun open-brace-p (c)
  (char= c #\{))

(defun close-brace-p (c)
  (char= c #\}))

(defun semicolon-p (c)
  (char= c #\;))

(defun parse-terminator ()
  (smug:.is #'terminator-p))

(defun parse-letter ()
  (smug:.or (smug:.is #'cl:lower-case-p) (smug:.is #'cl:upper-case-p)))

(defun alpha-numeric ()
  (smug:.is #'cl:alphanumericp))

(defun numeric-char ()
  (smug:.is #'cl:digit-char-p))

(defun other-name-chars ()
  (smug:.or
   (alpha-numeric)
   (first-name-char)))

(defun first-name-char ()
  (smug:.or
   (parse-letter)
   (smug:.is (lambda (c) (member c '(#\+ #\- #\_ #\/ #\* #\. #\< #\> #\=))))))

(defun parse-number ()
  (smug:.let*
      ((num (smug:.map 'string (numeric-char)))
       (_ (parse-terminator)))
    (smug:.identity (cons 'number (read-from-string num)))))

(defun parse-boolean ()
  (smug:.let* ((bool (smug:.or (smug:.string= "true") (smug:.string= "false"))))
    (smug:.identity (cons 'bool bool))))

(defun parse-name ()
  (smug:.let* ((_ (parse-whitespace))
	       (fst-char (first-name-char))
	       (rest-chars (smug:.map 'string (other-name-chars) :at-least 0))
	       (_ (parse-whitespace 1)))
    (smug:.first (smug:.identity (concatenate 'string (string fst-char) rest-chars)))))

(defun parse-parameter ()
  (smug:.let* ((name (parse-name)))
    (smug:.identity (cons 'parameter name))))

(defun parse-symbol ()
  (smug:.let* ((name (parse-name)))
    (smug:.identity (cons 'symbol name))))

(defun parse-constant ()
  (smug:.or
   (parse-number)
   ;; (parse-string)
   (parse-boolean)
   ;; (parse-character)
   ))

(defun parse-atom ()
  (smug:.or
   (parse-constant)
   (parse-name)))

(defun parse-atom-as-form ()
  (smug:.let* ((atom (parse-atom)))
    (smug:.identity (make-form
		     :type (car atom)
		     :value (cdr atom)
		     :args nil))))

(defun parse-argument-symbol ()
  (smug:.let* ((symb (parse-name)))
    (smug:.identity (make-symbol-arg
		     :value symb))))

(defun parse-argument-constant ()
  (smug:.let* ((const (parse-constant)))
    (smug:.identity (make-constant-arg
		     :value (cdr const)))))

(defun parse-argument ()
  (smug:.let* ((arg (smug:.or 
		     (parse-argument-constant)
		     (parse-argument-symbol)
		     (parse-braced-expression))))
    (smug:.identity arg)))

(defun parse-arguments ()
  (smug:.let* ((args
		(smug:.map 'list (parse-argument) :at-least 0)))
    (smug:.identity args)))

(defun parse-name-and-arguments ()
  (smug:.let* ((name (parse-symbol))
	       (args (parse-arguments)))
    (smug:.identity (make-form
		     :type 'symbol
		     :value (cdr name)
		     :args args))))

(defun parse-form ()
  (smug:.let* ((_ (parse-whitespace))
	       (form
		(smug:.or
		 (parse-name-and-arguments)
		 (parse-atom-as-form)))
	       (_ (parse-whitespace)))
    (smug:.identity form)))

(defun parse-semicolon-terminated-form ()
  (smug:.let* ((_ (parse-whitespace))
	       (form (parse-form))
	       (_ (smug:.is #'semicolon-p))
	       (_ (parse-whitespace)))
    (smug:.identity (make-expression
		     :definitions nil
		     :final-form form))))

(defun parse-parameters ()
  (smug:.let* ((params (smug:.map 'list (parse-parameter) :at-least 0)))
    (smug:.identity params)))

(defun parse-expression ()
  (smug:.let* ((_ (parse-whitespace))
	       (defns (smug:.map 'list (parse-definition) :at-least 0))
	       (form (parse-form))
	       (_ (parse-whitespace)))
    (smug:.identity (make-expression
		     :definitions (create-definitions defns)
		     :final-form form))))

(defun parse-braced-expression ()
  (smug:.let* ((_ (parse-whitespace))
	       (_ (smug:.is #'open-brace-p))
	       (expr (parse-expression))
	       (_ (smug:.is #'close-brace-p))
	       (_ (parse-whitespace)))
    (smug:.identity expr)))

(defun parse-top-expression ()
  (smug:.let* ((_ (parse-whitespace))
	       (top-expr  (smug:.or
				  (parse-semicolon-terminated-form)
				  (parse-braced-expression)))
	       (_ (parse-whitespace)))
    (smug:.identity top-expr)))

(defun parse-definition ()
  "Parses a definition of the form: symbol params+ = expression
   params can be zero or more parameters.
   expression can be terminated with ; or wrapped in {}"
  (smug:.let* ((_ (parse-whitespace))
	       (symname (parse-symbol))
	       (params (parse-parameters))
	       (_ (smug:.is #'equals-sign-p))
	       (expr (parse-top-expression))
	       (_ (parse-whitespace)))
    (smug:.identity (make-definition
		     :symbol (cdr symname)
		     :parameters params
		     :expression expr))))

(defun parse-all ()
  (smug:.first (smug:.map 'list (parse-definition))))

(defun add-whitespace (strn)
  "Bit of a hack to help the parser"
  (let*
    ((one (cl-ppcre:regex-replace-all "{" strn " { "))
     (two (cl-ppcre:regex-replace-all "}" one " } "))
     (three (cl-ppcre:regex-replace-all ";" two " ; ")))
    three))

(defun get-definitions-list (source)
  (first (first (smug:run (parse-all) (add-whitespace source)))))

(defun parse-source (source)
  (create-definitions (get-definitions-list source)))
