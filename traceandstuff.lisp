(defun factorial (n)
  (cond ((zerop n) 1)
	(t (* n (factorial (1- n))))))

(trace factorial)

(factorial 5)
;  0: (FACTORIAL 5)
;    1: (FACTORIAL 4)
;      2: (FACTORIAL 3)
;        3: (FACTORIAL 2)
;          4: (FACTORIAL 1)
;            5: (FACTORIAL 0)
;            5: FACTORIAL returned 1
;          4: FACTORIAL returned 1
;        3: FACTORIAL returned 2
;      2: FACTORIAL returned 6
;    1: FACTORIAL returned 24
;  0: FACTORIAL returned 120


(defun factorial-tr (n)
  (factorial-tr-helper n 1))

(defun factorial-tr-helper (n product)
  (cond ((zerop n) product)
        (t 
         ; factorial-tr-helper is the last function called 
         (factorial-tr-helper (- n 1) (* product n)))))

; Add dollar's as a read-macro. And always convert to pennies to make sure the calculations of dollars work. 
(set-macro-character #\$
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (round (* 100 (read stream)))))




(defclass empty-object () ())

; sexy
(make-instance (find-class 'empty-object))


; inheritance

(defclass 2d-object () ())

(defclass 2d-centered-object (2d-object)
  ((x :accessor x)
   (y :accessor y)
   (orientation :accessor orientation)))

(defclass oval (2d-centered-object)
  ((axis-1 :accessor axis-1)
  (axis-2 :accessor axis-2)))

(defclass regular-polygon (2d-centered-object)
  ((n-sides :accessor number-of-sides)
  (size :accessor size)))


; Find shit.
(apropos "MAP" :cl)


(describe 'regular-polygon)

; Fett och kunna inspektera... Run in SLIME.
(inspect (make-instance 'regular-polygon))


; Awesome source for sequence related functions: http://psg.com/~dlamkins/sl/chapter13.html
(position #\a "This is all about you, isn't it?")

(find #\a "This is all about you, isn't it?")

(search "ab" "This is all about you, isn't it?")

(mismatch "banana" "bananananono")

(merge 'vector (list 1 3 5 9 8) (vector 2 6 4 7 0) #'>)


; Macros

; Like quote, backquote suppresses evaluation.
; But a comma within a backquoted form "unsuppresses" evaluation for just the following subform.

`(The sum of 17 and 83 is ,(+ 17 83))
; => (THE SUM OF 17 AND 83 IS 100)

'(The sum of 17 and 83 is (+ 17 83))
; => (THE SUM OF 17 AND 83 IS (+ 17 83))

(defmacro swap (a b) ; NOTE: This is a restricted version of ROTATEF 
  `(let ((temp ,a))
     (setf ,a ,b)
     (setf ,b temp)))

(pprint (macroexpand-1 '(swap x y)))


; Have you ever wanted a function in your programming language?
(defmacro repeat (times &body body)
  (let ((x (gensym)))
    `(dotimes (,x ,times)
       ,@body)))

(macroexpand-1 '(repeat 5 (print 'hi)))

(defmacro cube (n)
  (let ((x (gensym)))
    `(let ((,x ,n))
       (* ,x ,x ,x))))
; We have to bind the variable locally because if the caller sends in an sexp instead of a number it
; will be repeated.

; i.e: (let (x 2) (cube (incf x)))

; Errors and restarts

(defpackage :testing
  (:use :common-lisp))

(in-package :testing)

(define-condition expect-type-error (error)
  ((object :initarg :object :reader object)
   (type :initarg :type :reader type))
  (:report (lambda (condition stream)
	     (format stream "~S is not of the expected type ~S."
		     (object condition)
		     (type condition)))))


(defun expect-type (object type default-value)
    (if (typep object type)
      object
      (progn
        (cerror "Substitute the default value ~5*~S."
                'expect-type-error
                :object object
                :type type
                :ignore default-value
                :allow-other-keys t)
        default-value)))

(defun my-divide (numerator denominator)
    (assert (not (zerop denominator)) (numerator denominator)
            "You can't divide ~D by ~D." numerator denominator)
    (/ numerator denominator))


; Damn format
;  Directive   Interpretation
;  ---------   --------------
;     ~%       new line
;     ~&       fresh line
;     ~|       page break
;     ~T       tab stop
;     ~<       justification
;     ~>       terminate ~<
;     ~C       character
;     ~(       case conversion
;     ~)       terminate ~(
;     ~D       decimal integer
;     ~B       binary integer
;     ~O       octal integer
;     ~X       hexadecimal integer
;     ~bR      base-b integer
;     ~R       spell an integer
;     ~P       plural
;     ~F       floating point
;     ~E       scientific notation
;     ~G       ~F or ~E, depending upon magnitude
;     ~$       monetary
;     ~A       legibly, without escapes
;     ~S       READably, with escapes

