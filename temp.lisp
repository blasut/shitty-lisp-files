
;; A sweet ass lisp file for trying out stuffs.

(defun foo (a b &optional (c 3 c-supplied-p)) 
  (list a b c c-supplied-p))

;; Dynamically typed and strongly typed

(defun foo (x y z) (+ x y z))
;; bindings to hold the arguments. En binding är en runtime manifestation of a variable.

(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |


(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

(defparameter lol 
  (let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count))))

(funcall (first lol))
(funcall (first (rest lol)))
(funcall (first (rest (rest lol))))

;;;;; Variables

;; Assignment 

(defun foo (x) (setf x 10))

(let ((y 20))
  (foo y)
  (print y))

; setf return the newly assigned value
(defvar x)
(defvar y)
(setf x (setf y (random 10)))


;; Generalized assignment

; Simple variable:    (setf x 10) 
; Array:              (setf (aref a 0) 10)
; Hash table:         (setf (gethash 'key hash) 10)
; Slot named 'field': (setf (field o) 10)



; setf can be used to modify any value

(defvar x 1)
(defvar y 1)

(setf x (+ x 1))
(setf x (- x 1))
;; Shorthands 
(incf x)    ; (setf x (+ x 1))
(decf x)    ; (setf x (- x 1))
(incf x 10) ; (setf x (+ x 10))

;; INCF & DECF kallas modify macros

;; rotate

(defvar a 1)
(defvar b 2)
(rotatef a b)
; a = 2
; b = 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Macros

(not nil)             ; T
(not (= 1 1))         ; NIL
(and (= 1 2) (= 3 3)) ; NIL
(or (= 1 2) (= 3 3))  ; T

;; Dolist för att loopa igenom en lista med saker'
; (dolist (var list-form)
;    body-form*)
(dolist (x '(1 2 3)) 
  (print x))

; För att avrbyta dolist använd 'return'
(dolist (x '(1 2 3)) 
  (print x) 
  (if (evenp x) 
      (return)))

;; dotimes används för counting-loops
; (dotimes (var count-form)
;    body-form*)
;
; count-form == integer
(dotimes (i 4) (print i))
;går att använda return här med

;; Body i både dolist och dotimes kan innehålla vilka expressions som helst, så går att nesta loops

;; DO är fett flexibel








;; Sen finns det ju LOOP

; Samla 1 till 10 i en lista
(loop :for i :from 1 :to 10 :collecting i)

; summera
(loop :for x :from 1 :to 10 :summing (expt x 2))

; Räkna antalet vokaler i en sträng
(loop :for x :across "the quick brown fox jumps over the lazy dog"
   :counting (find x "aeiou")) ;; => 11

;; Räkna ut det elvte Fibonachi numret
(loop :for i :below 10
   :and a = 0 :then b
   :and b = 1 :then (+ b a)
   :finally (return a)) ;; => 55



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Macros; defining your own.

; Macros expanderas under "macro expansion time" och körs som vanlig kod i "runtime"

; För att skriva macros:
; 1. Skriv exempelkod för att köra macrot och för det macrot ska expandera till
; 2. Skriv kod som genererar det handskrivna resultatet
; 3. Se till att macrot inte "läcker"

;; DOPRIMES
;; Itererar över prime-nummer

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; DO-PRIMES så ska det köras:
;(do-primes (p 0 19)
;  (format t "~d " p))


;; DO-PRIMES resultat:
;(do ((p (next-prime 0) (next-prime (1+ p))))
;    ((> p 19))
;  (format t  "~d " p))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

(do-primes (p 0 19)
  (format t "~d " p))

(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; UNIT TEST FRAMEWORK

; Simplest version

(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))


; To know which test failed och passed

(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
  

; Refactor time

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

; First macro

(defmacro check (form)
  `(report-result ,form ',form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

; Remove more duplication

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

; Fixing the return value

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

; Better result reporting

(defvar *test-name* nil)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MF LISTS

(cons 1 2) ; => (1 . 2)
;     | |
;   car cdr

(car (cons 1 2)); => 1
(cdr (cons 1 2)); => 2

(defparameter *cons* (cons 1 2))

(setf (car *cons*) 10) ; => *cons* = (10 . 2)
(setf (cdr *cons*) 20) ; => *cons* = (10 . 20)


(cons 1 nil); => (1)
(cons 1 (cons 2 nil)); => (1 2)
(cons 1 (cons 2 (cons 3 nil))); => (1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mostly functional approach to lists

(append (list 1 2) (list 3 4))

(defparameter *list* '(1 2 3 4 5))

(defparameter *copy* (copy-seg *list*))

(eq *copy* *list*) ; => nil
(eql *copy* *list*) ; => nil
(equalp *copy* *list*) ; => T
















