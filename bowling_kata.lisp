;; C-c C-j	slime-eval-last-expression-in-repl
;; C-c C-k	- Compile and load the current buffer's file.

(in-package :cl-user)

(ql:quickload :lisp-unit)

(defpackage :bowling
  (:use :common-lisp
	:lisp-unit))

(in-package :bowling)

(defclass game ()
  ((rolls :initarg :rolls :reader rolls))
  (:default-initargs :rolls nil))

(defmethod roll ((game game) pins)
  (make-instance 'game :rolls (append (rolls game) (list pins))))

(defun two-ball-sum (rolls)
  (+ (first rolls) (second rolls)))

(defun spare-p (rolls)
  (= (two-ball-sum rolls) 10))

(defun spare-score (rolls)
  (+ 10 (third rolls)))

(defun strike-p (rolls)
  (= (first rolls) 10))

(defun strike-score (rolls)
  (+ 10 (two-ball-sum (rest rolls))))

(defun score-rolls (rolls score frame)
  (flet ((add (points &optional (next-fn #'cddr))
	   (score-rolls (funcall next-fn rolls) (+ score points) (1+ frame))))
    (cond
      ((< 10 frame)                   score)
      ((strike-p rolls)               (add (strike-score rolls) #'cdr))
      ((spare-p rolls)                (add (spare-score rolls)))
      (t                              (add (two-ball-sum rolls))))))
    
(defmethod score ((game game))
  (score-rolls (rolls game) 0 1))


;;;;;;;;;;;;;;;;;;;;;;; 
;;;; HELPERS

(defun roll-many (game &key initial pins rolls)
  (cond
    (initial (roll-many (roll game (first initial)) 
			:initial (rest initial)
			:pins pins :rolls rolls))
    ((zerop rolls) game)
    (t (roll-many (roll game pins) :pins pins :rolls (1- rolls)))))

(defun bowling-score (&rest roll-many-options)
  (let ((game (make-instance 'game)))
    (apply #'roll-many game roll-many-options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TESTS

(setq *print-failures* t)
(define-test we-can-roll ()
  (assert-true (make-instance 'game))
  (assert-true (roll (make-instance 'game) 1)))

(define-test zero-game-test ()
  (assert-equal 0 (score (bowling-score :pins 0 :rolls 20))))

(define-test ones-game-test ()
  (assert-equal 20 (score (bowling-score :pins 1 :rolls 20))))

(define-test spare-test ()
  (assert-equal 14 (score (bowling-score :initial '(5 5 1 2) :pins 0 :rolls 16))))

(define-test strike-test ()
  (assert-equal 19 (score (bowling-score :initial '(10 1 2 3 0) :pins 0 :rolls 14))))

(define-test perfect-game-test-test ()
  (assert-equal 300 (score (bowling-score :pins 10 :rolls 12))))

; Easier to use this
(print-errors (run-tests))




