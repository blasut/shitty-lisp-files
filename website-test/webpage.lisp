(defpackage :webpage
  (:use :cl :cl-ppcre :hunchentoot :cl-who))
(in-package :webpage)

(ql:quickload "hunchentoot")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-who")

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))
(setf hunchentoot:*hunchentoot-default-external-format* (flex:make-external-format
							 :utf-8 :eol-style :lf))
(defparameter hunchentoot:reply-external-format (flex:make-external-format
						 :utf-8 :eol-style :lf)) 
(setf hunchentoot:*default-content-type* "text/html; charset=utf-8")

(setf *web-server* (make-instance 'hunchentoot:easy-acceptor :port *server-port*))

(push (hunchentoot:create-folder-dispatcher-and-handler 
  "/css/" 
  "/Users/schmolzer/Desktop/shitty lisp files/website-test/css/")
      *dispatch-table*)

(defmacro defpage-easy-d (name title uri parameter-list docs &body body)
  "Generates the html page and includes a page template"
  `(define-easy-handler (,name :uri ,uri :default-request-type :both)
       ,parameter-list ,docs (page-template ,title ,@body)))

(defmacro page-template (title &body body)
  "Generates the basic html page tempalte with css"
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
      
	      :content "text/html;charset=ufc-8")
       (:title (str (format nil " ~a" ,title)))
       (:link :rel "stylesheet" :type "text/css" :href "/css/base.css" :media "scren"))
      (:body
       (:div :id "container"
	     (:div :id "header"
		   (str (banner)))
	     (:div
	      (:div :id "content"
		    (str ,@body))
	      (:div :id "navigation"
		    (str (navigation)))
	      (:div :id "extra"
		    (str (extra-stuff)))
	      (:div :id "footer"
		    (str (footer-area)))))))))

(defun banner ()
  "Just a banner"
  (with-html-output-to-string (*standard-output* nil :indent t)
    (htm
     (:h1 "Quicklip Info Dump"))))

(defun navigation ()
  "Just a nav section"
  (with-html-output-to-string (*standard-output* nil :indent t)
    (htm
     (:h3 "Loaded packages"))))

(defun extra-stuff ()
  "Just some extra stuff"
  (with-html-output-to-string (*standard-output* nil :indent t)
    (htm
     (:h3 "Extras"))))

(defun footer-area ()
  "Just some footer stuff"
  (with-html-output-to-string (*standard-output* nil :indent t) 
    (htm
     (:table (:tr
	      (:td (:a :href "http://www.quicklisp.org"
		       (str "Quicklisp")))
	      (:td (:a :href "http://common-lisp.net/"
		       (str "Common-Lisp.net")))
	      (:td (:a :href "http://planet.lisp.org/"
		       (str "Planet Lisp")))
	      (:td (:a :href "http://planet.cliki.net/"
		       (str "Planet Cliki")))    
	      (:td (:a :href "http://planet.sbcl.org/"
		       (str "Planet SBCL")))       
	      (:td (:a :href "http://cl-user.net/"
		       (str "CL Directory")))    
	      (:td (:a :href "http://www.lisp.org/alu/home"
		       (str "ALU")))       
	      (:td (:a :href "http://www.lispworks.com/documentation/HyperSpec/Front/"
		       (str "Lispworks HyperSpec"))))))))

(defpage-easy-d home-page "QLID" "/" ()
    "Handles base page."
    (with-html-output-to-string (*standard-output*)
      (:htm 
       (:h1 "Yes, we have bananas."))))

(defun list-current-systems ()
  "Just return a list of all the current systems."
  (let ((system-list ()))
    (flet ((push-pkg-to-system-list
	       (pkg)
	     (push (asdf:component-name pkg) system-list)))
      (asdf:map-systems #'push-pkg-to-system-list))
    (sort system-list #'string<)))

(defun navigation ()
  "Just a nav section"
  (with-html-output-to-string (*standard-output* nil :indent t)
    (htm
     (:h3 "Loaded packages")
     (loop for x in (list-current-systems)
	  do (htm (:a :href (conc "display-package?name=" x)
		       (str x))
		   (:br))))))

(defun get-external-functions (name)
  "Returns a lsit of the external function names for the names package.
   Package is a string. It drops the reference to the package name."
  (let* ((package-name (if (stringp name) (read-from-string name) name))
	 (package (if (symbolp package-name) (find-package package-name))))
    (when (packagep package)
      (let ((lst ()))
	(do-external-symbols (s package)
	  (when (sb-introspect:function-type s)
	    (push (write-to-string s) lst)))
	(sort lst #'string<)))))


(defpage-easy-d display-package "display-package" "/display-package" ((name :parameter-type 'string))
  "Handles package display requests."
  (with-html-output-to-string (*standard-output*)
    (:htm
     (:h1 (str name))
     (loop for x in (get-external-functions name)
	  do (htm (:a :href (conc "display-function?name=" x)
		      (str x))
		  (:br))))))


(defpage-easy-d display-function "display-function" "/display-function" ((name :parameter-type 'string))
    "Handles function display requests"
  (when (symbolp (read-from-string name))
    (let* ((package-name (first (cl-ppcre:split ":" name))))
      (with-html-output-to-string (*standard-output*)
	(:htm
	 (:h2 "Package: " (:a :href (conc "display-package?name=" package-name)
			      (str package-name)))
	 (:h2 (str name))
	 (:h3 "Documentation string")
	 (str (documentation (read-from-string name) 'function))
	 (:br))))))
