(defpackage :cl-tidy.script
  (:use #:cl #:asdf))

(in-package :cl-tidy.script)

(defsystem :cl-tidy
  :description "FFI bindings for HTML Tidy"
  :version "0.0.2"
  :author "Red Daly <reddaly at gmail.com>"
  :license "MIT License"
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "main" :depends-on ("package"))
			 )))

  :depends-on ("cffi"))

(setf (asdf:component-property (asdf:find-system :cl-tidy) :website)
      "http://github.com/gonzojive/cl-tidy")
      


(defsystem :cl-tidy.doc
  :description ""
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail.com>"
  :license "No one is licensed to use this, not even myself."
  :components ((:module "doc"
			:components
			((:file "cl-tidy-docdown")
			 )))

  :depends-on ("cl-tidy" "docdown"))
