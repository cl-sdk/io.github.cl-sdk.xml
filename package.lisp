(defpackage #:cl-xml
  (:use #:cl #:trivial-gray-streams)
  (:export
   ;; Document
   #:xml-document
   #:xml-document-p
   #:make-xml-document
   #:xml-document-prolog
   #:xml-document-root
   ;; Element
   #:xml-node
   #:xml-node-p
   #:make-xml-node
   #:xml-node-tag
   #:xml-node-attributes
   #:xml-node-children
   ;; Comment
   #:xml-comment
   #:xml-comment-p
   #:make-xml-comment
   #:xml-comment-data
   ;; Processing instruction
   #:xml-pi
   #:xml-pi-p
   #:make-xml-pi
   #:xml-pi-target
   #:xml-pi-data
   ;; CDATA section
   #:xml-cdata
   #:xml-cdata-p
   #:make-xml-cdata
   #:xml-cdata-data
   ;; Qualified name (namespace-aware)
   #:xml-qname
   #:xml-qname-p
   #:make-xml-qname
   #:xml-qname-prefix
   #:xml-qname-local-name
   #:xml-qname-namespace-uri
   ;; SAX handler protocol
   #:sax-handler
   #:start-document
   #:end-document
   #:start-element
   #:end-element
   #:characters
   #:comment
   #:processing-instruction
   #:cdata-section
   ;; Default DOM-building handler
   #:dom-builder
   ;; Entry points
   #:parse-xml
   #:resolve-namespaces))
