
(defparameter *source* "
    function1 a b = + a b;
    function2 a b = * a b;
    function3 fn1 fn2 = {
      c = fn1 2 5;
      d = fn2 c 8;
      sqrt { function2 c d }
    }

    fn4 a b c d = {
      f1 e f = { * e f a }
      f2 g h = { + b { / g h } }
      f3 i j = {
        k = * i j;
        ** k 4
      }
      f1 { f2 c d } {f3 a c}
    }

    .main args = {
    function3 function1 function2}

    ")


(defparameter *source-1* "
    .main args = { * 2 3 }"
  "Should be 6")

(defparameter *source-2* "
    function1 a b = + a b;
    .main args = { function1 2 3 } "
  "Should be 5")

(defparameter *source-3* "
    function1 a b = + a b;
    function2 a b = * a b;

    .main args = {
	function1 2 {function2 3 4} 
}
    "
  "Should be 14")

(defparameter *source-4* "
    add a b = + a b;
    .main = {
      a = add 4 5;
      * a 2 }  "
  "Should be 18")

(defparameter *source-5* "
    add a b = + a b;
    complex a b = * a {+ a b} {- b a};
    expr a b = { / a { * b b b }}
    .main = {
      a = add 4 5;
      b = complex 7 8;
      c = expr 9 10;
      * a b c }  ")

;; function as a parameter
(defparameter *source-6* "
  add a b = + a b;
  apply fn a b = fn a b;
  .main = { apply add 1 2 }"
  "Should be 3")

;; return a function
(defparameter *source-7* "
  add = { fn a b = + a b; fn }
  apply fn a b = fn a b;
  .main = { apply add 1 2 }"
  "Should be 3")

(defparameter *source-8* "
  add a = { fn b = + a b; fn }
  add1 = add 1;
  .main = { add1 2 }"
  "Not working. Unsuprisingly.")

(defparameter *source-9*
  " 
  a = 2;
  b = 5;
  add3 a = { b = 3; + a b } 
  .main = { b = 7; + b {add3 1} }"
  "Should be 11")

(defparameter *source-10*
  " 
  add3 a = { + a b } 
  .main = { b = 7; add3 1 }"
  "Should error (b should not be available in add3).")

(defparameter *source-11*
  " 
  add3 a = { add b = + a b; add 3 } 
  .main = { b = 7; add3 1 }"
  "Should be 4. add in add3 should close over parameter a")

(defparameter *source-12*
  " 
  min a b = { if {< a b} a b }
  .main args = min 9 4;
")

(defparameter *source-13*
  "
  fib n = match (n)
    case (0) = 0; 
    case (1) = 1;
    case (2) = 1;
    case (_) = { + {fib {- n 1} } {fib {- n 2 }}; 
  .main = fib 5;"
  "TODO - not done pattern matching yet...")
