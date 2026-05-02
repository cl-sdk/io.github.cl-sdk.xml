;;;; W3C XML 1.0 Conformance Test Suite runner for cl-xml
;;;;
;;;; This file implements a runner for the W3C XML Conformance Test Suites:
;;;;   https://www.w3.org/XML/Test/xmlts20130923.zip
;;;;
;;;; Usage:
;;;;   (asdf:load-system :cl-xml.conformance)
;;;;   (cl-xml.conformance:run-conformance-tests)
;;;;
;;;; Or set the XMLTS_DIR environment variable to the xmlconf/ directory
;;;; before running.

(defpackage #:cl-xml.conformance
  (:use #:cl)
  (:export #:run-conformance-tests
           #:find-xmlts-dir
           #:*xmlts-dir*
           ;; Result struct and accessors
           #:conformance-results
           #:conformance-results-total
           #:conformance-results-passed
           #:conformance-results-failed
           #:conformance-results-skipped
           #:conformance-results-failures))

(in-package #:cl-xml.conformance)

;;; ── Configuration ────────────────────────────────────────────────────────

(defvar *xmlts-dir* nil
  "Pathname of the xmlconf/ directory from the W3C XML Test Suite.
When NIL (the default), RUN-CONFORMANCE-TESTS searches via the XMLTS_DIR
environment variable and several well-known paths.")

;;; ── Test-case representation ─────────────────────────────────────────────

(defstruct (conformance-test (:conc-name ct-))
  "One entry from the xmlconf.xml catalog."
  id            ; string  — unique test identifier
  type          ; string  — "valid", "invalid", "not-wellformed", or "error"
  uri           ; string  — file name relative to base
  entities      ; string  — "none", "general", "parameter", or "both"
  recommendation ; string — e.g. "XML1.0", "NS1.0"
  edition       ; string  — e.g. "1 2 3 4 5"
  sections      ; string  — relevant spec sections
  description   ; string  — human-readable description
  base)         ; pathname — directory containing the test file

(defun ct-path (test)
  "Return the absolute pathname of TEST's XML file."
  (merge-pathnames (ct-uri test) (ct-base test)))

;;; ── Catalog parsing ──────────────────────────────────────────────────────

(defun get-node-attr (node name)
  "Return the string value of attribute NAME from NODE, or NIL."
  (cdr (assoc name (cl-xml:xml-node-attributes node) :test #'string=)))

(defun join-base (outer-base local-base-str)
  "Combine OUTER-BASE (pathname) with LOCAL-BASE-STR to produce a new directory pathname."
  (if (or (null local-base-str) (string= local-base-str ""))
      outer-base
      (merge-pathnames local-base-str outer-base)))

(defun node-description (node)
  "Extract the trimmed text description from NODE's children, or empty string."
  (let ((children (cl-xml:xml-node-children node)))
    (if (and children (stringp (first children)))
        (string-trim '(#\Space #\Tab #\Newline #\Return) (first children))
        "")))

(defun collect-tests-from-node (node base acc)
  "Walk the xmlconf.xml parse tree rooted at NODE, accumulating CONFORMANCE-TEST
structs into ACC (a list).  BASE is the current directory pathname for URI resolution.
Returns the updated accumulator."
  (let* ((tag        (cl-xml:xml-node-tag node))
         (local-base (get-node-attr node "xml:base"))
         (cur-base   (join-base base local-base)))
    (cond
      ((or (string= tag "TESTSUITE") (string= tag "TESTCASES"))
       (dolist (child (cl-xml:xml-node-children node))
         (when (cl-xml:xml-node-p child)
           (setf acc (collect-tests-from-node child cur-base acc)))))
      ((string= tag "TEST")
       (push (make-conformance-test
              :id             (get-node-attr node "ID")
              :type           (get-node-attr node "TYPE")
              :uri            (get-node-attr node "URI")
              :entities       (or (get-node-attr node "ENTITIES") "none")
              :recommendation (get-node-attr node "RECOMMENDATION")
              :edition        (get-node-attr node "EDITION")
              :sections       (get-node-attr node "SECTIONS")
              :description    (node-description node)
              :base           cur-base)
             acc))))
  acc)

(defun load-catalog (xmlconf-dir)
  "Parse the xmlconf.xml catalog in XMLCONF-DIR and return a list of
CONFORMANCE-TEST structs (in catalog order)."
  (let* ((catalog-path (merge-pathnames "xmlconf.xml" xmlconf-dir))
         (catalog-str  (uiop:read-file-string catalog-path))
         (doc          (cl-xml:parse-xml catalog-str))
         (root         (cl-xml:xml-document-root doc)))
    (nreverse (collect-tests-from-node root xmlconf-dir '()))))

;;; ── Test execution ───────────────────────────────────────────────────────

(defun try-parse-xml-file (path)
  "Attempt to open and parse PATH as an XML file.
Returns (values :ok result) on success or (values :error condition) on any error."
  (handler-case
      (let ((content (uiop:read-file-string path :external-format :utf-8)))
        (values :ok (cl-xml:parse-xml content)))
    (error (c)
      (values :error c))))

(defun run-conformance-test (test)
  "Run a single conformance test.
Returns (values result reason) where RESULT is :pass, :fail, or :skip,
and REASON is a string explaining a failure or skip."
  (let ((path (ct-path test))
        (type (ct-type test)))
    ;; Skip if the test file is missing.
    (unless (probe-file path)
      (return-from run-conformance-test
        (values :skip (format nil "File not found: ~a" path))))
    (cond
      ;; TYPE="valid" — parser MUST accept the document without error.
      ((string= type "valid")
       (multiple-value-bind (status err) (try-parse-xml-file path)
         (if (eq status :ok)
             (values :pass nil)
             (values :fail
                     (format nil "Rejected valid document: ~a" err)))))
      ;; TYPE="not-wellformed" — parser MUST signal an error.
      ((string= type "not-wellformed")
       (multiple-value-bind (status err) (try-parse-xml-file path)
         (declare (ignore err))
         (if (eq status :error)
             (values :pass nil)
             (values :fail "Accepted a document that is not well-formed"))))
      ;; TYPE="invalid" or TYPE="error" — these test validity constraints or
      ;; optional behaviour; a non-validating parser may accept them.
      (t
       (values :skip (format nil "Optional test type: ~a" type))))))

;;; ── Test filtering ───────────────────────────────────────────────────────

(defun xml10-recommendation-p (rec)
  "Return T if REC is an XML 1.0 (or unspecified) recommendation string."
  (or (null rec)
      (member rec '("XML1.0" "XML1.0-errata2e" "XML1.0-errata3e" "XML1.0-errata4e")
              :test #'string=)))

(defun applicable-test-p (test)
  "Return T if TEST should be exercised by cl-xml (XML 1.0, no external entities for valid docs)."
  (and
   ;; Only XML 1.0 tests (skip XML 1.1 and pure namespace-spec tests).
   (xml10-recommendation-p (ct-recommendation test))
   ;; For valid tests, only attempt those that do not require external entity
   ;; or parameter entity processing, since cl-xml is a non-validating parser
   ;; that does not resolve external entities.
   (or (not (string= (ct-type test) "valid"))
       (string= (ct-entities test) "none"))))

;;; ── Result aggregation and reporting ─────────────────────────────────────

(defstruct conformance-results
  total passed failed skipped failures)

(defun run-all-conformance-tests (tests &key verbose)
  "Run all applicable tests from TESTS. Returns a CONFORMANCE-RESULTS struct.
When VERBOSE is true, prints one line per test."
  (let ((results (make-conformance-results
                  :total 0 :passed 0 :failed 0 :skipped 0 :failures '())))
    (dolist (test tests)
      (cond
        ((not (applicable-test-p test))
         (incf (conformance-results-skipped results))
         (when verbose
           (format t "  SKIP  ~a  [~a, not applicable]~%"
                   (ct-id test) (ct-type test))))
        (t
         (incf (conformance-results-total results))
         (multiple-value-bind (result reason) (run-conformance-test test)
           (case result
             (:pass
              (incf (conformance-results-passed results))
              (when verbose
                (format t "  PASS  ~a~%" (ct-id test))))
             (:fail
              (incf (conformance-results-failed results))
              (push (list (ct-id test) (ct-type test)
                          (ct-sections test) reason)
                    (conformance-results-failures results))
              (when verbose
                (format t "  FAIL  ~a [~a §~a]: ~a~%"
                        (ct-id test) (ct-type test) (ct-sections test) reason)))
             (:skip
              (incf (conformance-results-skipped results))
              (when verbose
                (format t "  SKIP  ~a: ~a~%" (ct-id test) reason))))))))
    (setf (conformance-results-failures results)
          (nreverse (conformance-results-failures results)))
    results))

(defun print-conformance-report (results)
  "Print a human-readable summary of RESULTS to standard output."
  (let ((total   (conformance-results-total   results))
        (passed  (conformance-results-passed  results))
        (failed  (conformance-results-failed  results))
        (skipped (conformance-results-skipped results)))
    (format t "~&~%W3C XML 1.0 Conformance Results~%")
    (format t "================================~%")
    (format t "  Applicable tests run:  ~a~%" total)
    (format t "  Passed:                ~a~%" passed)
    (format t "  Failed:                ~a~%" failed)
    (format t "  Skipped (N/A):         ~a~%" skipped)
    (when (plusp total)
      (format t "  Pass rate:             ~,1f%%~%"
              (* 100.0 (/ passed total))))
    (when (plusp failed)
      (format t "~%Failures (~a):~%" failed)
      (dolist (f (conformance-results-failures results))
        (destructuring-bind (id type sections reason) f
          (format t "  [~a] ~a (§~a): ~a~%" type id sections reason))))))

;;; ── Test-suite location discovery ────────────────────────────────────────

(defun find-xmlts-dir ()
  "Return the xmlconf/ directory pathname, or NIL if not found.
Search order:
  1. *XMLTS-DIR* (if set)
  2. XMLTS_DIR environment variable
  3. /tmp/xmlts/xmlconf/
  4. /tmp/xmlconf/
  5. xmlts/xmlconf/ inside the cl-xml.conformance source tree"
  (flet ((catalog-exists-p (dir)
           (probe-file (merge-pathnames "xmlconf.xml" dir))))
    (or
     (when *xmlts-dir*
       (let ((p (uiop:ensure-pathname *xmlts-dir* :want-directory t)))
         (when (catalog-exists-p p) p)))
     (let ((env (uiop:getenv "XMLTS_DIR")))
       (when (and env (not (string= env "")))
         (let ((p (uiop:ensure-pathname
                   (uiop:parse-unix-namestring env :ensure-directory t)
                   :want-directory t)))
           (when (catalog-exists-p p) p))))
     (dolist (candidate (list
                         (uiop:parse-unix-namestring "/tmp/xmlts/xmlconf/"
                                                     :ensure-directory t)
                         (uiop:parse-unix-namestring "/tmp/xmlconf/"
                                                     :ensure-directory t)
                         (merge-pathnames
                          "xmlts/xmlconf/"
                          (asdf:system-source-directory
                           (asdf:find-system :cl-xml.conformance nil)))))
       (when (catalog-exists-p candidate)
         (return candidate))))))

;;; ── Main entry point ─────────────────────────────────────────────────────

(defun run-conformance-tests (&key dir verbose (print-report t))
  "Run the W3C XML 1.0 Conformance Test Suite against cl-xml.

DIR      — pathname of the xmlconf/ directory; NIL means auto-detect via
           FIND-XMLTS-DIR.
VERBOSE  — when true, print one line per test as it runs.
PRINT-REPORT — when true (the default), print a summary table at the end.

Returns a CONFORMANCE-RESULTS struct, or NIL if the test suite was not found."
  (let ((xmlconf-dir (or dir (find-xmlts-dir))))
    (unless xmlconf-dir
      (when print-report
        (format t "~&W3C XML Test Suite not found.~%~
                   Set the XMLTS_DIR environment variable to the xmlconf/ directory,~%~
                   or download the suite from:~%~
                     https://www.w3.org/XML/Test/xmlts20130923.zip~%~
                   and extract so that xmlconf/xmlconf.xml exists.~%"))
      (return-from run-conformance-tests nil))
    (when print-report
      (format t "~&Loading W3C XML Conformance Test Suite catalog from:~%  ~a~%"
              xmlconf-dir))
    (let* ((tests   (load-catalog xmlconf-dir))
           (results (run-all-conformance-tests tests :verbose verbose)))
      (when print-report
        (print-conformance-report results))
      results)))
