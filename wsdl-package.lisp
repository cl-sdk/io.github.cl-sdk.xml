(defpackage #:cl-xml.wsdl
  (:use #:cl #:cl-xml)
  (:export
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
