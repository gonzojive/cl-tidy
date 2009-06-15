(defpackage :cl-tidy.script
  (:use #:cl #:asdf))

(in-package :cl-tidy.script)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :cl-tidy
  :description ""
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail.com>"
  :license "No one is licensed to use this, not even myself."
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "main" :depends-on ("package"))
			 )))

  :depends-on ("cffi"))
