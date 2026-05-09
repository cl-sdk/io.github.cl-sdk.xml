(defpackage #:io.github.cl-sdk.xsd
  (:use #:cl #:io.github.cl-sdk.xml)
  (:export
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
