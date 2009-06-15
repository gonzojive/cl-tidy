(in-package :cl-tidy)

(define-foreign-library libtidy
  (:unix (:or "/git/suave/cl-tidy/tidylib/lib/libtidy.so")))

(use-foreign-library libtidy)

(defctype tidy-doc :int)

(defcfun ("tidyParseString" tidy-parse-string) :int
  (tdoc tidy-doc)
  (html-content :string))

(defcfun ("tidyCleanAndRepair" tidy-clean-and-repair) :int
  (tdoc tidy-doc))

(defcfun ("tidySaveBuffer" tidy-save-buffer) :int
  (tdoc tidy-doc)
  (output :pointer))

(defcfun ("tidyCreate" tidy-create) tidy-doc
  )

(defcfun ("tidyRelease" tidy-release) :void
  (tdoc tidy-doc))

(defcstruct tidy-buffer
  (allocator :pointer)
  (bp :pointer)
  (size :unsigned-int)
  (allocated :unsigned-int)
  (next :unsigned-int))

;; Interface

(defmacro with-tidy-doc ((var) &body body)
  `(let ((,var (tidy-create)))
     (unwind-protect (progn ,@body)
       (tidy-release ,var))))

(defun init-tidy-buffer (buf)
  (setf (foreign-slot-value buf 'tidy-buffer 'allocator) (make-pointer 0)
	(foreign-slot-value buf 'tidy-buffer 'bp) (make-pointer 0)
	(foreign-slot-value buf 'tidy-buffer 'size) 0
	(foreign-slot-value buf 'tidy-buffer 'allocated) 0
	(foreign-slot-value buf 'tidy-buffer 'next) 0)
  buf)


(defmacro with-tidy-buffer ((var) &body body)
  `(with-foreign-object (,var 'tidy-buffer)
     (init-tidy-buffer ,var)
     ,@body))
       

(defun clean-up-html (string)
  (with-tidy-doc (doc)
    (with-tidy-buffer (buf)
      (tidy-parse-string doc string)
      (tidy-clean-and-repair doc)
      (tidy-save-buffer doc buf)
      (convert-from-foreign (foreign-slot-value buf 'tidy-buffer 'bp) :string))))


(defun slurp-file-3000 (pathname)
  "A SLURP-FILE function inspired Mr. Insane 3000's SLURP-STREAM4."
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))
