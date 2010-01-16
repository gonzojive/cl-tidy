(defpackage :cl-tidy.doc
    (:use :docdown :cl-tidy :cl))

(in-package :cl-tidy.doc)

(progn
  (defdoc index :page
    (:title "CL-Tidy")
    (:content
     "#### [HTML Tidy](http://sourceforge.net/projects/tidy/) for Common Lisp

## Synopsis

CL-Tidy is a small set of bindings for the HTML Tidy library.  It is
used for cleaning up mostly-correct HTML pages and for identifying the
problems in an understandable way.  From the [author of Tidy](http://www.w3.org/People/Raggett/tidy/):

> Tidy is able to fix up a wide range of problems and to bring to
> your attention things that you need to work on yourself. Each item
> found is listed with the line number and column so that you can see
> where the problem lies in your markup. Tidy won't generate a cleaned
> up version when there are problems that it can't be sure of how to
> handle. These are logged as \"errors\" rather than \"warnings\".

The interface is described in detail below but the basic usage is this:

    (cl-tidy:clean-up-html (drakma:http-request \"http://www.bkstr.com/\"))
")

    (:sections
     (defdoc download :section
       (:title "Download and Installation")
       (:content "All the code is maintained in a git repository.  To
obtain the library, use the following command:

    git clone git://github.com/gonzojive/cl-tidy.git

You can also browse the code at [http://github.com/gonzojive/cl-tidy](http://github.com/gonzojive/cl-tidy).
"))
     (defdoc functions :section
       (:title "Functions")
       (:content "")
       (:children
	(defdoc cl-tidy:clean-up-html :function)))))

  (output-docs))

(defun output-docs ()
  (with-open-file (stream (asdf:system-relative-pathname (asdf:find-system :cl-tidy.doc)
							 "doc/index.html")
			  :direction :output :if-exists :supersede)
    (write-string (generate-html-page 'index) stream)))
   
  