(in-package :cl-tidy)

(define-foreign-library libtidy
  (:unix (:or "libtidy.so"
	      "/git/cl-tidy/lib/libtidy.so"
	      "/usr/local/lib/libtidy.so"
	      "/git/suave/cl-tidy/tidylib/lib/libtidy.so")))

(use-foreign-library libtidy)

(defctype tidy-doc :pointer)

(defcstruct tidy-buffer
  (allocator :pointer)
  (bp :pointer)
  (size :unsigned-int)
  (allocated :unsigned-int)
  (next :unsigned-int))

(defcenum boolean
  :no
  :yes)

(defcenum tidy-option
  :unknown-option ;   /**< Unknown option! */
  :indent-spaces ;    /**< Indentation n spaces */
  :TidyWrapLen ;         /**< Wrap margin */
  :TidyTabSize ;        /**< Expand tabs to n spaces */

  :TidyCharEncoding ;    /**< In/out character encoding */
  :TidyInCharEncoding ; /**< Input character encoding (if different) */
  :TidyOutCharEncoding ; /**< Output character encoding (if different) */    
  :TidyNewline ;         /**< Output line ending (default to platform) */

  :TidyDoctypeMode ;     /**< See doctype property */
  :TidyDoctype ;         /**< User specified doctype */

  :TidyDuplicateAttrs ;  /**< Keep first or last duplicate attribute */
  :TidyAltText ;         /**< Default text for alt attribute */
  
  :TidySlideStyle ;      /**< Style sheet for slides: not used for anything yet */

  :TidyErrFile ;         /**< File name to write errors to */
  :TidyOutFile ;         /**< File name to write markup to */
  :TidyWriteBack ;       /**< If true then output tidied markup */
  :TidyShowMarkup ;      /**< If false, normal output is suppressed */
  :TidyShowWarnings ;    /**< However errors are always shown */
  :TidyQuiet ;           /**< No 'Parsing X', guessed DTD or summary */
  :TidyIndentContent ;   /**< Indent content of appropriate tags */
					;/**< "auto" does text/block level content indentation */
  :TidyHideEndTags ;     /**< Suppress optional end tags */
  :TidyXmlTags ;         /**< Treat input as XML */
  :xml-out ;          /**< Create output as XML */
  :xhtml-out ;        /**< Output extensible HTML */
  :html-out ;        /**< Output plain HTML, even for XHTML input.
					;Yes means set explicitly. */
  :TidyXmlDecl ;         /**< Add <?xml?> for XML docs */
  :TidyUpperCaseTags ;   /**< Output tags in upper not lower case */
  :TidyUpperCaseAttrs ;  /**< Output attributes in upper not lower case */
  :TidyMakeBare ;        /**< Make bare HTML: remove Microsoft cruft */
  :TidyMakeClean ;       /**< Replace presentational clutter by style rules */
  :TidyLogicalEmphasis ; /**< Replace i by em and b by strong */
  :TidyDropPropAttrs ;   /**< Discard proprietary attributes */
  :TidyDropFontTags ;    /**< Discard presentation tags */
  :TidyDropEmptyParas ;  /**< Discard empty p elements */
  :TidyFixComments ;     /**< Fix comments with adjacent hyphens */
  :TidyBreakBeforeBR ;   /**< Output newline before <br> or not? */


  :TidyBurstSlides ;     /**< Create slides on each h2 element */

  :tidy-num-entities ;     /**< Use numeric entities */
  :TidyQuoteMarks ;      /**< Output " marks as &quot; */
  :tidy-quote-nbsp ;       /**< Output non-breaking space as entity */
  :tidy-quote-ampersand ;  /**< Output naked ampersand as &amp; */
  :tidy-wrap-att-vals ;     /**< Wrap within attribute values */
  :TidyWrapScriptlets ;  /**< Wrap within JavaScript string literals */
  :TidyWrapSection ;     /**< Wrap within <![ ... ]> section tags */
  :TidyWrapAsp ;         /**< Wrap within ASP pseudo elements */
  :TidyWrapJste ;        /**< Wrap within JSTE pseudo elements */
  :TidyWrapPhp ;         /**< Wrap within PHP pseudo elements */
  :TidyFixBackslash ;    /**< Fix URLs by replacing \ with / */
  :TidyIndentAttributes ;/**< Newline+indent before each attribute */
  :TidyXmlPIs ;          /**< If set to yes PIs must end with ?> */
  :TidyXmlSpace ;        /**< If set to yes adds xml:space attr as needed */
  :TidyEncloseBodyText ; /**< If yes text at body is wrapped in P's */
  :TidyEncloseBlockText ;/**< If yes text in blocks is wrapped in P's */
  :TidyKeepFileTimes ;   /**< If yes last modied time is preserved */
  :TidyWord2000 ;        /**< Draconian cleaning for Word2000 */
  :TidyMark ;            /**< Add meta element indicating tidied doc */
  :TidyEmacs ;           /**< If true format error output for GNU Emacs */
  :TidyEmacsFile ;       /**< Name of current Emacs file */
  :TidyLiteralAttribs ;  /**< If true attributes may use newlines */
  :show-body-only ;        /**< Output BODY content only */
  :TidyFixUri ;          /**< Applies URI encoding if necessary */
  :TidyLowerLiterals ;   /**< Folds known attribute values to lower case */
  :TidyHideComments ;    /**< Hides all (real) comments in output */
  :TidyIndentCdata ;     /**< Indent <!CDATA[ ... ]]> section */
  :TidyForceOutput ;     /**< Output document even if errors were found */
  :TidyShowErrors ;      /**< Number of errors to put out */
  :TidyAsciiChars ;      /**< Convert quotes and dashes to nearest ASCII char */
  :TidyJoinClasses ;     /**< Join multiple class attributes */
  :TidyJoinStyles ;      /**< Join multiple style attributes */
  :TidyEscapeCdata ;     /**< Replace <![CDATA[]]> sections with escaped text */

  )


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

(defcfun ("tidyOptSetBool" tidy-opt-set-bool) boolean
  (tdoc tidy-doc)
  (opt tidy-option)
  (val boolean))

(defcfun ("tidyOptSetValue" tidy-opt-set-value) boolean
         (tdoc tidy-doc)
         (opt tidy-option)
         (val :string))

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

(defmacro without-interrupts (&body body)
  "Run BODY with interrupts disabled."
  #+allegro `(excl:without-interrupts ,@body)
  #+sbcl `(sb-sys:without-interrupts ,@body)
  #-(or allegro sbcl) `(progn ,@body))

(defun clean-up-html (string)
  "Given an HTML string, `string`, runs the input through Tidy. If
Tidy can handle the input, returns a version of the document that is valid
XHTML.

Thus, most dirty web pages from the internet can be grabbed run
through `CLEAN-UP-HTML` and subsequently passed into a strict XML
parser."
  (declare (optimize debug))
  (without-interrupts
    (with-tidy-doc (doc)
      (with-tidy-buffer (buf)
      
        ;;(tidy-opt-set-bool doc :xml-out :yes)
        ;;(tidy-opt-set-bool doc :tidy-quote-ampersand :yes)
        ;;(tidy-opt-set-bool doc :tidy-quote-nbsp :yes)
        
        (tidy-opt-set-bool doc :tidyshowwarnings :no)
        (tidy-opt-set-bool doc :xhtml-out :yes)

        (tidy-opt-set-bool doc :tidy-num-entities :yes)
        
        
        (tidy-parse-string doc string)
        (tidy-clean-and-repair doc)
        (tidy-save-buffer doc buf)
        (convert-from-foreign (foreign-slot-value buf 'tidy-buffer 'bp) :string)))))

(defun clean-up-html-part (string)
  "Similar to `CLEAN-UP-HTML` but returns html 
   without wrapping it into <html> and <body> tags, 
   see show-body-only tidy option"
  (declare (optimize debug))
  (without-interrupts
    (with-tidy-doc (doc)
      (with-tidy-buffer (buf)
        (tidy-opt-set-value doc :show-body-only "yes")

        (tidy-opt-set-bool doc :tidyshowwarnings :no)
        (tidy-opt-set-bool doc :xhtml-out :yes)

        (tidy-opt-set-bool doc :tidy-num-entities :yes)


        (tidy-parse-string doc string)
        (tidy-clean-and-repair doc)
        (tidy-save-buffer doc buf)
        (convert-from-foreign (foreign-slot-value buf 'tidy-buffer 'bp) :string)))))


(defun slurp-file-3000 (pathname)
  "A SLURP-FILE function inspired Mr. Insane 3000's SLURP-STREAM4."
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))



;;
;; mods/additions to cl-tidy: 
;; facilitate check for tidy 'errors' and/or 'warnings'
;;



;; tidy docs indicate "Must call tidyCleanAndRepair() first."
;; - returns 
;;     1 if any warnings
;;     0 if tidy is happy
(defcfun ("tidyRunDiagnostics" tidy-run-diagnostics) :int
  (tdoc tidy-doc))

;; number of tidy warnings encountered
(defcfun ("tidyWarningCount" tidy-warning-count) :uint
  (tdoc tidy-doc))

(defcfun ("tidyErrorCount" tidy-error-count) :uint
  (tdoc tidy-doc))

;; write info to error sink
;; TODO: implement code for error sink/buffer
(defcfun ("tidyErrorSummary" tidy-error-summary) :void
  (tdoc tidy-doc))


;; note: desirable to return warning and error counts in context of
;; CLEAN-UP-HTML (e.g., as second and third values)?
(defun check-html (string)
  (with-tidy-doc (doc)
    (with-tidy-buffer (buf)
      (tidy-parse-string doc string)
      (tidy-clean-and-repair doc)
      (tidy-run-diagnostics doc)
      (values
       (tidy-warning-count doc)
       (tidy-error-count doc)
       ))))


