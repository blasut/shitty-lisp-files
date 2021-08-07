(let ((f #'+) (a '(1 2 3 4 5 6))) `(,f ,@a))

(let ((f '+) (a '(1 2 3 4 5 6))) `(f a))

(let ((f '+) (a '(1 2 3 4 5 6))) `(,f ,@a))

(eval (let ((f '+) (a '(1 2 3 4 5 6))) `(,f ,@a)))

(+ 1 1)

;; Backtick really means "qoute everything inside except prefixed with comma"
;; so macros use backtick because it's handy. But macros == functions that run at compile time. 

(defmacro comment (&body body))

(comment
  (this is my awesome code)
  (example of data '(1 2 3 4 5))
  (apply #'+ '(1111)))


