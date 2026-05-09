(defpackage #:io.github.cl-sdk.soap
  (:use #:cl #:io.github.cl-sdk.xml)
  (:export
   ;; SOAP namespace URI constants
   #:+soap-1.1-namespace+
   #:+soap-1.2-namespace+
   ;; SOAP envelope structure
   #:soap-envelope
   #:soap-envelope-p
   #:make-soap-envelope
   #:soap-envelope-version
   #:soap-envelope-header
   #:soap-envelope-body
   ;; SOAP header structure
   #:soap-header
   #:soap-header-p
   #:make-soap-header
   #:soap-header-entries
   ;; SOAP body structure
   #:soap-body
   #:soap-body-p
   #:make-soap-body
   #:soap-body-fault
   #:soap-body-payload
   ;; SOAP fault structure
   #:soap-fault
   #:soap-fault-p
   #:make-soap-fault
   #:soap-fault-code
   #:soap-fault-string
   #:soap-fault-actor
   #:soap-fault-detail
   #:soap-fault-lang
   ;; SOAP error condition
   #:soap-error
   #:soap-error-message
   #:soap-error-path
   ;; SOAP entry points
   #:parse-soap
   #:serialize-soap
   #:soap-make-envelope))
