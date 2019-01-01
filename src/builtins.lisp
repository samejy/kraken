
(defparameter *builtin-local-env* nil)
(defparameter *builtin-global-env* nil)

;; TODO - currently this is a builtin so if and else expressions are evaluated before
;; getting here, which is obviously not the desired behaviour
;; either needs to be treated differently, or defined as a builtin with lazily evaluated parameters (which is not yet implemented...)
(defun kn-if (test if-exp else-exp)
  (declare (optimize (debug 3)))
  (if test if-exp else-exp))
