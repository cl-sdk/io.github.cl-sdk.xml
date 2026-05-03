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
   #:validate-xml
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
   #:soap-make-envelope
   ;; WSDL 2.0 namespace constant
   #:+wsdl-2.0-namespace+
   ;; WSDL description
   #:wsdl-description
   #:wsdl-description-p
   #:make-wsdl-description
   #:wsdl-description-target-namespace
   #:wsdl-description-imports
   #:wsdl-description-includes
   #:wsdl-description-types
   #:wsdl-description-interfaces
   #:wsdl-description-bindings
   #:wsdl-description-services
   ;; WSDL import
   #:wsdl-import
   #:wsdl-import-p
   #:make-wsdl-import
   #:wsdl-import-namespace
   #:wsdl-import-location
   ;; WSDL include
   #:wsdl-include
   #:wsdl-include-p
   #:make-wsdl-include
   #:wsdl-include-location
   ;; WSDL interface
   #:wsdl-interface
   #:wsdl-interface-p
   #:make-wsdl-interface
   #:wsdl-interface-name
   #:wsdl-interface-extends
   #:wsdl-interface-style-default
   #:wsdl-interface-faults
   #:wsdl-interface-operations
   ;; WSDL interface fault
   #:wsdl-interface-fault
   #:wsdl-interface-fault-p
   #:make-wsdl-interface-fault
   #:wsdl-interface-fault-name
   #:wsdl-interface-fault-element
   ;; WSDL interface operation
   #:wsdl-interface-operation
   #:wsdl-interface-operation-p
   #:make-wsdl-interface-operation
   #:wsdl-interface-operation-name
   #:wsdl-interface-operation-pattern
   #:wsdl-interface-operation-style
   #:wsdl-interface-operation-inputs
   #:wsdl-interface-operation-outputs
   #:wsdl-interface-operation-in-faults
   #:wsdl-interface-operation-out-faults
   ;; WSDL message reference (input/output)
   #:wsdl-message-ref
   #:wsdl-message-ref-p
   #:make-wsdl-message-ref
   #:wsdl-message-ref-message-label
   #:wsdl-message-ref-element
   ;; WSDL fault reference (infault/outfault)
   #:wsdl-fault-ref
   #:wsdl-fault-ref-p
   #:make-wsdl-fault-ref
   #:wsdl-fault-ref-message-label
   #:wsdl-fault-ref-ref
   ;; WSDL binding
   #:wsdl-binding
   #:wsdl-binding-p
   #:make-wsdl-binding
   #:wsdl-binding-name
   #:wsdl-binding-interface
   #:wsdl-binding-type
   #:wsdl-binding-faults
   #:wsdl-binding-operations
   ;; WSDL binding fault
   #:wsdl-binding-fault
   #:wsdl-binding-fault-p
   #:make-wsdl-binding-fault
   #:wsdl-binding-fault-ref
   #:wsdl-binding-fault-code
   ;; WSDL binding operation
   #:wsdl-binding-operation
   #:wsdl-binding-operation-p
   #:make-wsdl-binding-operation
   #:wsdl-binding-operation-ref
   #:wsdl-binding-operation-inputs
   #:wsdl-binding-operation-outputs
   #:wsdl-binding-operation-in-faults
   #:wsdl-binding-operation-out-faults
   ;; WSDL service
   #:wsdl-service
   #:wsdl-service-p
   #:make-wsdl-service
   #:wsdl-service-name
   #:wsdl-service-interface
   #:wsdl-service-endpoints
   ;; WSDL endpoint
   #:wsdl-endpoint
   #:wsdl-endpoint-p
   #:make-wsdl-endpoint
   #:wsdl-endpoint-name
   #:wsdl-endpoint-binding
   #:wsdl-endpoint-address
   ;; WSDL error condition
   #:wsdl-error
   #:wsdl-error-message
   #:wsdl-error-path
   ;; WSDL entry points
   #:parse-wsdl
   #:serialize-wsdl
   #:wsdl-find-interface
   #:wsdl-find-binding
   #:wsdl-find-service))
