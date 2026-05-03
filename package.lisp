(defpackage #:cl-xml
  (:use #:cl #:trivial-gray-streams)
  (:export
   ;; Document
   #:xml-document
   #:xml-document-p
   #:make-xml-document
   #:xml-document-prolog
   #:xml-document-doctype
   #:xml-document-root
   ;; DTD element declaration
   #:xml-dtd-element
   #:xml-dtd-element-p
   #:make-xml-dtd-element
   #:xml-dtd-element-name
   #:xml-dtd-element-content-model
   ;; DTD attribute definition
   #:xml-dtd-att-def
   #:xml-dtd-att-def-p
   #:make-xml-dtd-att-def
   #:xml-dtd-att-def-name
   #:xml-dtd-att-def-type
   #:xml-dtd-att-def-default
   ;; DTD ATTLIST declaration
   #:xml-dtd-attlist
   #:xml-dtd-attlist-p
   #:make-xml-dtd-attlist
   #:xml-dtd-attlist-element-name
   #:xml-dtd-attlist-definitions
   ;; DTD ENTITY declaration
   #:xml-dtd-entity
   #:xml-dtd-entity-p
   #:make-xml-dtd-entity
   #:xml-dtd-entity-name
   #:xml-dtd-entity-parameter-p
   #:xml-dtd-entity-definition
   ;; DTD NOTATION declaration
   #:xml-dtd-notation
   #:xml-dtd-notation-p
   #:make-xml-dtd-notation
   #:xml-dtd-notation-name
   #:xml-dtd-notation-public-id
   #:xml-dtd-notation-system-id
   ;; DOCTYPE declaration
   #:xml-doctype
   #:xml-doctype-p
   #:make-xml-doctype
   #:xml-doctype-name
   #:xml-doctype-public-id
   #:xml-doctype-system-id
   #:xml-doctype-elements
   #:xml-doctype-attlists
   #:xml-doctype-entities
   #:xml-doctype-notations
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
   #:doctype-declaration
   ;; Default DOM-building handler
   #:dom-builder
   ;; XML event types (intermediate representation from parse-xml-events)
   #:xml-event-start-element
   #:xml-event-start-element-p
   #:xml-event-start-element-tag
   #:xml-event-start-element-attributes
   #:xml-event-end-element
   #:xml-event-end-element-p
   #:xml-event-end-element-tag
   #:xml-event-characters
   #:xml-event-characters-p
   #:xml-event-characters-text
   #:xml-event-comment
   #:xml-event-comment-p
   #:xml-event-comment-data
   #:xml-event-pi
   #:xml-event-pi-p
   #:xml-event-pi-target
   #:xml-event-pi-data
   #:xml-event-cdata
   #:xml-event-cdata-p
   #:xml-event-cdata-data
   #:xml-event-doctype
   #:xml-event-doctype-p
   #:xml-event-doctype-doctype
   ;; Entry points
   #:parse-xml-events
   #:reduce-events
   #:parse-xml
   #:resolve-namespaces
   ;; XSD schema structures
   #:xsd-schema
   #:xsd-schema-p
   #:make-xsd-schema
   #:xsd-schema-target-namespace
   #:xsd-schema-elements
   #:xsd-schema-types
   ;; XSD element declaration
   #:xsd-element
   #:xsd-element-p
   #:make-xsd-element
   #:xsd-element-name
   #:xsd-element-type
   #:xsd-element-min-occurs
   #:xsd-element-max-occurs
   #:xsd-element-ref
   ;; XSD attribute declaration
   #:xsd-attribute
   #:xsd-attribute-p
   #:make-xsd-attribute
   #:xsd-attribute-name
   #:xsd-attribute-type
   #:xsd-attribute-use
   #:xsd-attribute-default
   #:xsd-attribute-fixed
   ;; XSD complex type
   #:xsd-complex-type
   #:xsd-complex-type-p
   #:make-xsd-complex-type
   #:xsd-complex-type-name
   #:xsd-complex-type-compositor
   #:xsd-complex-type-elements
   #:xsd-complex-type-attributes
   #:xsd-complex-type-mixed
   ;; XSD simple type
   #:xsd-simple-type
   #:xsd-simple-type-p
   #:make-xsd-simple-type
   #:xsd-simple-type-name
   #:xsd-simple-type-base
   #:xsd-simple-type-facets
   ;; XSD namespace URI constant
   #:+xsd-namespace-uri+
   ;; XSD validation condition
   #:xsd-validation-error
   #:xsd-validation-error-message
   #:xsd-validation-error-path
   ;; XSD entry points
   #:load-xsd
   #:validate-xml))
