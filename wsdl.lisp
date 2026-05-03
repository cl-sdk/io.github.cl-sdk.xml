(in-package #:cl-xml)

;;;; WSDL (Web Services Description Language) — 2.0 implementation
;;;;
;;;; Specification: https://www.w3.org/TR/wsdl20/
;;;;
;;;; Supported:
;;;;   wsdl:description  — root element, targetNamespace, imports, includes
;;;;   wsdl:types        — XSD type container (children preserved as xml-nodes)
;;;;   wsdl:interface    — abstract interface, extends, styleDefault
;;;;     wsdl:fault      — interface-level fault (name, element)
;;;;     wsdl:operation  — interface operation (name, pattern, style)
;;;;       wsdl:input    — message reference (messageLabel, element)
;;;;       wsdl:output   — message reference (messageLabel, element)
;;;;       wsdl:infault  — fault reference (messageLabel, ref)
;;;;       wsdl:outfault — fault reference (messageLabel, ref)
;;;;   wsdl:binding      — concrete binding (name, interface, type)
;;;;     wsdl:fault      — binding fault (ref, code)
;;;;     wsdl:operation  — binding operation (ref)
;;;;       wsdl:input / wsdl:output / wsdl:infault / wsdl:outfault
;;;;   wsdl:service      — service (name, interface)
;;;;     wsdl:endpoint   — endpoint (name, binding, address)
;;;;
;;;; Public API:
;;;;   Namespace constant — +wsdl-2.0-namespace+
;;;;   Structures  — wsdl-description, wsdl-import, wsdl-include,
;;;;                 wsdl-interface, wsdl-interface-fault,
;;;;                 wsdl-interface-operation, wsdl-message-ref, wsdl-fault-ref,
;;;;                 wsdl-binding, wsdl-binding-fault, wsdl-binding-operation,
;;;;                 wsdl-service, wsdl-endpoint
;;;;   Condition   — wsdl-error
;;;;   Entry points — parse-wsdl, serialize-wsdl

;;; ─── Namespace URI constant ──────────────────────────────────────────────

(defparameter +wsdl-2.0-namespace+
  "http://www.w3.org/ns/wsdl"
  "WSDL 2.0 namespace URI.")

;;; ─── Structures ───────────────────────────────────────────────────────────

(defstruct wsdl-description
  "A parsed WSDL 2.0 description document.
TARGET-NAMESPACE is the target namespace URI string, or NIL.
IMPORTS is a list of wsdl-import structs.
INCLUDES is a list of wsdl-include structs.
TYPES is a list of xml-node structs (the children of wsdl:types), or NIL.
INTERFACES is a list of wsdl-interface structs.
BINDINGS is a list of wsdl-binding structs.
SERVICES is a list of wsdl-service structs."
  target-namespace
  (imports   '())
  (includes  '())
  (types     '())
  (interfaces '())
  (bindings  '())
  (services  '()))

(defstruct wsdl-import
  "A wsdl:import element.
NAMESPACE is the imported namespace URI string.
LOCATION is the optional document location URI string, or NIL."
  namespace
  location)

(defstruct wsdl-include
  "A wsdl:include element.
LOCATION is the document location URI string."
  location)

(defstruct wsdl-interface
  "A WSDL 2.0 wsdl:interface definition.
NAME is the interface name string.
EXTENDS is a list of interface name strings that this interface extends.
STYLE-DEFAULT is a list of IRI strings indicating the default operation style.
FAULTS is a list of wsdl-interface-fault structs.
OPERATIONS is a list of wsdl-interface-operation structs."
  name
  (extends      '())
  (style-default '())
  (faults       '())
  (operations   '()))

(defstruct wsdl-interface-fault
  "A wsdl:fault element within wsdl:interface.
NAME is the fault name string.
ELEMENT is the element declaration QName string, or NIL."
  name
  element)

(defstruct wsdl-interface-operation
  "A wsdl:operation element within wsdl:interface.
NAME is the operation name string.
PATTERN is the message exchange pattern IRI string, or NIL.
STYLE is a list of IRI strings indicating the operation style.
INPUTS is a list of wsdl-message-ref structs for wsdl:input elements.
OUTPUTS is a list of wsdl-message-ref structs for wsdl:output elements.
IN-FAULTS is a list of wsdl-fault-ref structs for wsdl:infault elements.
OUT-FAULTS is a list of wsdl-fault-ref structs for wsdl:outfault elements."
  name
  pattern
  (style      '())
  (inputs     '())
  (outputs    '())
  (in-faults  '())
  (out-faults '()))

(defstruct wsdl-message-ref
  "A wsdl:input or wsdl:output message reference within a wsdl:operation.
MESSAGE-LABEL is the message label string, or NIL.
ELEMENT is the element declaration QName string, or NIL."
  message-label
  element)

(defstruct wsdl-fault-ref
  "A wsdl:infault or wsdl:outfault fault reference within a wsdl:operation.
MESSAGE-LABEL is the message label string, or NIL.
REF is the fault QName reference string."
  message-label
  ref)

(defstruct wsdl-binding
  "A WSDL 2.0 wsdl:binding element.
NAME is the binding name string.
INTERFACE is the interface QName string this binding binds, or NIL.
TYPE is the binding type IRI string, or NIL.
FAULTS is a list of wsdl-binding-fault structs.
OPERATIONS is a list of wsdl-binding-operation structs."
  name
  interface
  type
  (faults     '())
  (operations '()))

(defstruct wsdl-binding-fault
  "A wsdl:fault element within wsdl:binding.
REF is the fault QName reference string.
CODE is the fault code string, or NIL."
  ref
  code)

(defstruct wsdl-binding-operation
  "A wsdl:operation element within wsdl:binding.
REF is the operation QName reference string.
INPUTS is a list of wsdl-message-ref structs.
OUTPUTS is a list of wsdl-message-ref structs.
IN-FAULTS is a list of wsdl-fault-ref structs.
OUT-FAULTS is a list of wsdl-fault-ref structs."
  ref
  (inputs     '())
  (outputs    '())
  (in-faults  '())
  (out-faults '()))

(defstruct wsdl-service
  "A WSDL 2.0 wsdl:service element.
NAME is the service name string.
INTERFACE is the interface QName string this service implements, or NIL.
ENDPOINTS is a list of wsdl-endpoint structs."
  name
  interface
  (endpoints '()))

(defstruct wsdl-endpoint
  "A wsdl:endpoint element within wsdl:service.
NAME is the endpoint name string.
BINDING is the binding QName string, or NIL.
ADDRESS is the endpoint URI string, or NIL."
  name
  binding
  address)

;;; ─── WSDL error condition ─────────────────────────────────────────────────

(define-condition wsdl-error (error)
  ((message :initarg :message :reader wsdl-error-message)
   (path    :initarg :path    :reader wsdl-error-path
            :initform nil))
  (:report (lambda (c s)
             (if (wsdl-error-path c)
                 (format s "WSDL error at ~a: ~a"
                         (wsdl-error-path c)
                         (wsdl-error-message c))
                 (format s "WSDL error: ~a"
                         (wsdl-error-message c)))))
  (:documentation
   "Condition signaled when a WSDL document cannot be parsed or is malformed.
MESSAGE is a string describing the problem; PATH is the element location, or NIL."))

;;; ─── Internal helpers — tag name and attribute extraction ────────────────

(defun %wsdl-local (node)
  "Return the local (unprefixed) name of xml-node NODE's tag."
  (let ((tag (xml-node-tag node)))
    (if (xml-qname-p tag)
        (xml-qname-local-name tag)
        (let ((colon (position #\: tag)))
          (if colon (subseq tag (1+ colon)) tag)))))

(defun %wsdl-ns-uri (node)
  "Return the namespace URI of xml-node NODE's tag, or NIL."
  (let ((tag (xml-node-tag node)))
    (when (xml-qname-p tag)
      (xml-qname-namespace-uri tag))))

(defun %wsdl-element-children (node)
  "Return only the xml-node children of NODE (no text/comment nodes)."
  (remove-if-not #'xml-node-p (xml-node-children node)))

(defun %wsdl-attr (node name)
  "Return the attribute value string for attribute NAME of NODE, or NIL."
  (cdr (find-if (lambda (attr)
                  (let ((k (car attr)))
                    (string= name
                             (if (xml-qname-p k)
                                 (xml-qname-local-name k)
                                 (let ((c (position #\: k)))
                                   (if c (subseq k (1+ c)) k))))))
                (xml-node-attributes node))))

(defun %wsdl-split-words (str)
  "Split STR on whitespace and return a list of non-empty strings."
  (when str
    (let ((words '()) (start nil))
      (loop for i from 0 to (length str) do
        (if (or (= i (length str))
                (member (char str i) '(#\Space #\Tab #\Newline #\Return)))
            (when start
              (push (subseq str start i) words)
              (setf start nil))
            (unless start (setf start i))))
      (nreverse words))))

;;; ─── Internal helpers — XML character escaping ───────────────────────────

(defun %wsdl-escape-text (str)
  "Escape STR for inclusion in XML character data."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;"  out))
        (#\> (write-string "&gt;"  out))
        (t   (write-char ch out))))))

(defun %wsdl-escape-attr (str)
  "Escape STR for inclusion in a double-quoted XML attribute value."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\& (write-string "&amp;"  out))
        (#\< (write-string "&lt;"   out))
        (#\" (write-string "&quot;" out))
        (t   (write-char ch out))))))

;;; ─── Internal helpers — serialization ───────────────────────────────────

(defun %wsdl-tag-string (tag)
  "Return the string form of TAG (a plain string or xml-qname)."
  (if (xml-qname-p tag)
      (let ((prefix (xml-qname-prefix tag))
            (local  (xml-qname-local-name tag)))
        (if prefix
            (concatenate 'string prefix ":" local)
            local))
      tag))

(defun %wsdl-serialize-node (node stream)
  "Serialize xml-node NODE to STREAM."
  (typecase node
    (string
     (write-string (%wsdl-escape-text node) stream))
    (xml-node
     (let* ((tag   (xml-node-tag node))
            (tag-s (%wsdl-tag-string tag))
            (attrs (xml-node-attributes node)))
       (write-char #\< stream)
       (write-string tag-s stream)
       (dolist (attr attrs)
         (format stream " ~a=\"~a\""
                 (%wsdl-tag-string (car attr))
                 (%wsdl-escape-attr (cdr attr))))
       (if (xml-node-children node)
           (progn
             (write-char #\> stream)
             (dolist (child (xml-node-children node))
               (%wsdl-serialize-node child stream))
             (format stream "</~a>" tag-s))
           (write-string " />" stream))))
    (xml-comment
     (format stream "<!--~a-->" (xml-comment-data node)))
    (xml-pi
     (let ((data (xml-pi-data node)))
       (if (string= data "")
           (format stream "<?~a?>" (xml-pi-target node))
           (format stream "<?~a ~a?>" (xml-pi-target node) data))))
    (xml-cdata
     (format stream "<![CDATA[~a]]>" (xml-cdata-data node)))))

(defun %wsdl-write-attr (stream name value)
  "Write a single XML attribute to STREAM when VALUE is non-NIL."
  (when value
    (format stream " ~a=\"~a\"" name (%wsdl-escape-attr value))))

;;; ─── Internal helpers — parsing individual elements ────────────────────

(defun %parse-wsdl-import (node)
  "Parse a wsdl:import element NODE into a wsdl-import struct."
  (let ((ns  (%wsdl-attr node "namespace"))
        (loc (%wsdl-attr node "location")))
    (unless ns
      (error 'wsdl-error
             :message "wsdl:import missing required 'namespace' attribute"
             :path "wsdl:import"))
    (make-wsdl-import :namespace ns :location loc)))

(defun %parse-wsdl-include (node)
  "Parse a wsdl:include element NODE into a wsdl-include struct."
  (let ((loc (%wsdl-attr node "location")))
    (unless loc
      (error 'wsdl-error
             :message "wsdl:include missing required 'location' attribute"
             :path "wsdl:include"))
    (make-wsdl-include :location loc)))

(defun %parse-wsdl-message-ref (node)
  "Parse a wsdl:input or wsdl:output element NODE into a wsdl-message-ref struct."
  (make-wsdl-message-ref
   :message-label (%wsdl-attr node "messageLabel")
   :element       (%wsdl-attr node "element")))

(defun %parse-wsdl-fault-ref (node)
  "Parse a wsdl:infault or wsdl:outfault element NODE into a wsdl-fault-ref struct."
  (let ((ref (%wsdl-attr node "ref")))
    (unless ref
      (error 'wsdl-error
             :message (format nil "wsdl:~a missing required 'ref' attribute"
                              (%wsdl-local node))
             :path (%wsdl-local node)))
    (make-wsdl-fault-ref
     :message-label (%wsdl-attr node "messageLabel")
     :ref ref)))

(defun %parse-wsdl-interface-operation (node)
  "Parse a wsdl:operation element NODE within wsdl:interface."
  (let ((name    (%wsdl-attr node "name"))
        (pattern (%wsdl-attr node "pattern"))
        (style   (%wsdl-split-words (%wsdl-attr node "style")))
        (inputs '()) (outputs '()) (in-faults '()) (out-faults '()))
    (unless name
      (error 'wsdl-error
             :message "wsdl:operation missing required 'name' attribute"
             :path "wsdl:interface/wsdl:operation"))
    (dolist (child (%wsdl-element-children node))
      (let ((local (%wsdl-local child)))
        (cond
          ((string= local "input")    (push (%parse-wsdl-message-ref child) inputs))
          ((string= local "output")   (push (%parse-wsdl-message-ref child) outputs))
          ((string= local "infault")  (push (%parse-wsdl-fault-ref   child) in-faults))
          ((string= local "outfault") (push (%parse-wsdl-fault-ref   child) out-faults)))))
    (make-wsdl-interface-operation
     :name      name
     :pattern   pattern
     :style     style
     :inputs    (nreverse inputs)
     :outputs   (nreverse outputs)
     :in-faults (nreverse in-faults)
     :out-faults (nreverse out-faults))))

(defun %parse-wsdl-interface-fault (node)
  "Parse a wsdl:fault element NODE within wsdl:interface."
  (let ((name (%wsdl-attr node "name")))
    (unless name
      (error 'wsdl-error
             :message "wsdl:fault missing required 'name' attribute"
             :path "wsdl:interface/wsdl:fault"))
    (make-wsdl-interface-fault
     :name    name
     :element (%wsdl-attr node "element"))))

(defun %parse-wsdl-interface (node)
  "Parse a wsdl:interface element NODE into a wsdl-interface struct."
  (let ((name          (%wsdl-attr node "name"))
        (extends-str   (%wsdl-attr node "extends"))
        (style-default (%wsdl-split-words (%wsdl-attr node "styleDefault")))
        (faults '()) (operations '()))
    (unless name
      (error 'wsdl-error
             :message "wsdl:interface missing required 'name' attribute"
             :path "wsdl:interface"))
    (dolist (child (%wsdl-element-children node))
      (let ((local (%wsdl-local child)))
        (cond
          ((string= local "fault")     (push (%parse-wsdl-interface-fault     child) faults))
          ((string= local "operation") (push (%parse-wsdl-interface-operation child) operations)))))
    (make-wsdl-interface
     :name          name
     :extends       (%wsdl-split-words extends-str)
     :style-default style-default
     :faults        (nreverse faults)
     :operations    (nreverse operations))))

(defun %parse-wsdl-binding-fault (node)
  "Parse a wsdl:fault element NODE within wsdl:binding."
  (let ((ref (%wsdl-attr node "ref")))
    (unless ref
      (error 'wsdl-error
             :message "wsdl:fault (in binding) missing required 'ref' attribute"
             :path "wsdl:binding/wsdl:fault"))
    (make-wsdl-binding-fault
     :ref  ref
     :code (%wsdl-attr node "code"))))

(defun %parse-wsdl-binding-operation (node)
  "Parse a wsdl:operation element NODE within wsdl:binding."
  (let ((ref (%wsdl-attr node "ref"))
        (inputs '()) (outputs '()) (in-faults '()) (out-faults '()))
    (unless ref
      (error 'wsdl-error
             :message "wsdl:operation (in binding) missing required 'ref' attribute"
             :path "wsdl:binding/wsdl:operation"))
    (dolist (child (%wsdl-element-children node))
      (let ((local (%wsdl-local child)))
        (cond
          ((string= local "input")    (push (%parse-wsdl-message-ref child) inputs))
          ((string= local "output")   (push (%parse-wsdl-message-ref child) outputs))
          ((string= local "infault")  (push (%parse-wsdl-fault-ref   child) in-faults))
          ((string= local "outfault") (push (%parse-wsdl-fault-ref   child) out-faults)))))
    (make-wsdl-binding-operation
     :ref        ref
     :inputs     (nreverse inputs)
     :outputs    (nreverse outputs)
     :in-faults  (nreverse in-faults)
     :out-faults (nreverse out-faults))))

(defun %parse-wsdl-binding (node)
  "Parse a wsdl:binding element NODE into a wsdl-binding struct."
  (let ((name      (%wsdl-attr node "name"))
        (interface (%wsdl-attr node "interface"))
        (type      (%wsdl-attr node "type"))
        (faults '()) (operations '()))
    (unless name
      (error 'wsdl-error
             :message "wsdl:binding missing required 'name' attribute"
             :path "wsdl:binding"))
    (dolist (child (%wsdl-element-children node))
      (let ((local (%wsdl-local child)))
        (cond
          ((string= local "fault")     (push (%parse-wsdl-binding-fault     child) faults))
          ((string= local "operation") (push (%parse-wsdl-binding-operation child) operations)))))
    (make-wsdl-binding
     :name       name
     :interface  interface
     :type       type
     :faults     (nreverse faults)
     :operations (nreverse operations))))

(defun %parse-wsdl-endpoint (node)
  "Parse a wsdl:endpoint element NODE into a wsdl-endpoint struct."
  (let ((name    (%wsdl-attr node "name"))
        (binding (%wsdl-attr node "binding"))
        (address (%wsdl-attr node "address")))
    (unless name
      (error 'wsdl-error
             :message "wsdl:endpoint missing required 'name' attribute"
             :path "wsdl:service/wsdl:endpoint"))
    (make-wsdl-endpoint :name name :binding binding :address address)))

(defun %parse-wsdl-service (node)
  "Parse a wsdl:service element NODE into a wsdl-service struct."
  (let ((name      (%wsdl-attr node "name"))
        (interface (%wsdl-attr node "interface"))
        (endpoints '()))
    (unless name
      (error 'wsdl-error
             :message "wsdl:service missing required 'name' attribute"
             :path "wsdl:service"))
    (dolist (child (%wsdl-element-children node))
      (when (string= "endpoint" (%wsdl-local child))
        (push (%parse-wsdl-endpoint child) endpoints)))
    (make-wsdl-service
     :name      name
     :interface interface
     :endpoints (nreverse endpoints))))

;;; ─── Public: parse-wsdl ──────────────────────────────────────────────────

(defun parse-wsdl (input)
  "Parse INPUT (a string or character stream) as a WSDL 2.0 description document.

Returns a wsdl-description struct.  Namespace prefixes are resolved so that
the WSDL 2.0 namespace URI is detected regardless of the prefix used.

Signals wsdl-error if:
  - The document root element is not wsdl:description.
  - The root element is not in the WSDL 2.0 namespace."
  (let* ((doc      (parse-xml input))
         (resolved (resolve-namespaces doc))
         (root     (xml-document-root resolved))
         (local    (%wsdl-local root))
         (ns-uri   (%wsdl-ns-uri root)))
    (unless (string= local "description")
      (error 'wsdl-error
             :message (format nil
                         "Expected wsdl:description root element, found '~a'"
                         local)))
    (unless (string= ns-uri +wsdl-2.0-namespace+)
      (error 'wsdl-error
             :message (format nil
                         "Unknown WSDL namespace URI '~a'; expected '~a'"
                         ns-uri +wsdl-2.0-namespace+)))
    (let ((target-ns  (%wsdl-attr root "targetNamespace"))
          (imports    '())
          (includes   '())
          (types-nodes '())
          (interfaces '())
          (bindings   '())
          (services   '()))
      (dolist (child (%wsdl-element-children root))
        (let ((child-local (%wsdl-local child)))
          (cond
            ((string= child-local "import")
             (push (%parse-wsdl-import child) imports))
            ((string= child-local "include")
             (push (%parse-wsdl-include child) includes))
            ((string= child-local "types")
             (setf types-nodes
                   (append types-nodes (%wsdl-element-children child))))
            ((string= child-local "interface")
             (push (%parse-wsdl-interface child) interfaces))
            ((string= child-local "binding")
             (push (%parse-wsdl-binding child) bindings))
            ((string= child-local "service")
             (push (%parse-wsdl-service child) services)))))
      (make-wsdl-description
       :target-namespace target-ns
       :imports          (nreverse imports)
       :includes         (nreverse includes)
       :types            types-nodes
       :interfaces       (nreverse interfaces)
       :bindings         (nreverse bindings)
       :services         (nreverse services)))))

;;; ─── Internal helpers — serialization of WSDL structures ────────────────

(defun %serialize-wsdl-message-ref (local stream)
  "Serialize a wsdl-message-ref struct to STREAM with the given LOCAL tag name."
  (lambda (ref)
    (write-string "<wsdl:" stream)
    (write-string local stream)
    (%wsdl-write-attr stream "messageLabel" (wsdl-message-ref-message-label ref))
    (%wsdl-write-attr stream "element"      (wsdl-message-ref-element ref))
    (write-string " />" stream)))

(defun %serialize-wsdl-fault-ref (local stream)
  "Serialize a wsdl-fault-ref struct to STREAM with the given LOCAL tag name."
  (lambda (ref)
    (write-string "<wsdl:" stream)
    (write-string local stream)
    (%wsdl-write-attr stream "messageLabel" (wsdl-fault-ref-message-label ref))
    (%wsdl-write-attr stream "ref"          (wsdl-fault-ref-ref ref))
    (write-string " />" stream)))

(defun %serialize-wsdl-interface-operation (op stream)
  "Serialize wsdl-interface-operation OP to STREAM."
  (write-string "<wsdl:operation" stream)
  (%wsdl-write-attr stream "name"    (wsdl-interface-operation-name op))
  (%wsdl-write-attr stream "pattern" (wsdl-interface-operation-pattern op))
  (let ((style (wsdl-interface-operation-style op)))
    (when style
      (%wsdl-write-attr stream "style"
                        (format nil "~{~a~^ ~}" style))))
  (if (or (wsdl-interface-operation-inputs op)
          (wsdl-interface-operation-outputs op)
          (wsdl-interface-operation-in-faults op)
          (wsdl-interface-operation-out-faults op))
      (progn
        (write-char #\> stream)
        (dolist (r (wsdl-interface-operation-inputs op))
          (funcall (%serialize-wsdl-message-ref "input" stream) r))
        (dolist (r (wsdl-interface-operation-outputs op))
          (funcall (%serialize-wsdl-message-ref "output" stream) r))
        (dolist (r (wsdl-interface-operation-in-faults op))
          (funcall (%serialize-wsdl-fault-ref "infault" stream) r))
        (dolist (r (wsdl-interface-operation-out-faults op))
          (funcall (%serialize-wsdl-fault-ref "outfault" stream) r))
        (write-string "</wsdl:operation>" stream))
      (write-string " />" stream)))

(defun %serialize-wsdl-interface (iface stream)
  "Serialize wsdl-interface IFACE to STREAM."
  (write-string "<wsdl:interface" stream)
  (%wsdl-write-attr stream "name" (wsdl-interface-name iface))
  (let ((extends (wsdl-interface-extends iface)))
    (when extends
      (%wsdl-write-attr stream "extends" (format nil "~{~a~^ ~}" extends))))
  (let ((sd (wsdl-interface-style-default iface)))
    (when sd
      (%wsdl-write-attr stream "styleDefault" (format nil "~{~a~^ ~}" sd))))
  (if (or (wsdl-interface-faults iface)
          (wsdl-interface-operations iface))
      (progn
        (write-char #\> stream)
        (dolist (f (wsdl-interface-faults iface))
          (write-string "<wsdl:fault" stream)
          (%wsdl-write-attr stream "name"    (wsdl-interface-fault-name f))
          (%wsdl-write-attr stream "element" (wsdl-interface-fault-element f))
          (write-string " />" stream))
        (dolist (op (wsdl-interface-operations iface))
          (%serialize-wsdl-interface-operation op stream))
        (write-string "</wsdl:interface>" stream))
      (write-string " />" stream)))

(defun %serialize-wsdl-binding-operation (op stream)
  "Serialize wsdl-binding-operation OP to STREAM."
  (write-string "<wsdl:operation" stream)
  (%wsdl-write-attr stream "ref" (wsdl-binding-operation-ref op))
  (if (or (wsdl-binding-operation-inputs op)
          (wsdl-binding-operation-outputs op)
          (wsdl-binding-operation-in-faults op)
          (wsdl-binding-operation-out-faults op))
      (progn
        (write-char #\> stream)
        (dolist (r (wsdl-binding-operation-inputs op))
          (funcall (%serialize-wsdl-message-ref "input" stream) r))
        (dolist (r (wsdl-binding-operation-outputs op))
          (funcall (%serialize-wsdl-message-ref "output" stream) r))
        (dolist (r (wsdl-binding-operation-in-faults op))
          (funcall (%serialize-wsdl-fault-ref "infault" stream) r))
        (dolist (r (wsdl-binding-operation-out-faults op))
          (funcall (%serialize-wsdl-fault-ref "outfault" stream) r))
        (write-string "</wsdl:operation>" stream))
      (write-string " />" stream)))

(defun %serialize-wsdl-binding (binding stream)
  "Serialize wsdl-binding BINDING to STREAM."
  (write-string "<wsdl:binding" stream)
  (%wsdl-write-attr stream "name"      (wsdl-binding-name binding))
  (%wsdl-write-attr stream "interface" (wsdl-binding-interface binding))
  (%wsdl-write-attr stream "type"      (wsdl-binding-type binding))
  (if (or (wsdl-binding-faults binding)
          (wsdl-binding-operations binding))
      (progn
        (write-char #\> stream)
        (dolist (f (wsdl-binding-faults binding))
          (write-string "<wsdl:fault" stream)
          (%wsdl-write-attr stream "ref"  (wsdl-binding-fault-ref f))
          (%wsdl-write-attr stream "code" (wsdl-binding-fault-code f))
          (write-string " />" stream))
        (dolist (op (wsdl-binding-operations binding))
          (%serialize-wsdl-binding-operation op stream))
        (write-string "</wsdl:binding>" stream))
      (write-string " />" stream)))

(defun %serialize-wsdl-service (service stream)
  "Serialize wsdl-service SERVICE to STREAM."
  (write-string "<wsdl:service" stream)
  (%wsdl-write-attr stream "name"      (wsdl-service-name service))
  (%wsdl-write-attr stream "interface" (wsdl-service-interface service))
  (if (wsdl-service-endpoints service)
      (progn
        (write-char #\> stream)
        (dolist (ep (wsdl-service-endpoints service))
          (write-string "<wsdl:endpoint" stream)
          (%wsdl-write-attr stream "name"    (wsdl-endpoint-name ep))
          (%wsdl-write-attr stream "binding" (wsdl-endpoint-binding ep))
          (%wsdl-write-attr stream "address" (wsdl-endpoint-address ep))
          (write-string " />" stream))
        (write-string "</wsdl:service>" stream))
      (write-string " />" stream)))

;;; ─── Public: serialize-wsdl ──────────────────────────────────────────────

(defun serialize-wsdl (description &key stream)
  "Serialize DESCRIPTION (a wsdl-description struct) to XML.

When STREAM is NIL (the default), returns the XML as a fresh string.
When STREAM is a character output stream, writes the XML to it and returns NIL.

The output is a complete XML document beginning with an XML declaration,
using the namespace prefix 'wsdl' for the WSDL 2.0 namespace."
  (flet ((write-it (s)
           (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" s)
           (write-string "<wsdl:description" s)
           (format s " xmlns:wsdl=\"~a\"" +wsdl-2.0-namespace+)
           (%wsdl-write-attr s "targetNamespace"
                             (wsdl-description-target-namespace description))
           (write-char #\> s)
           ;; imports
           (dolist (imp (wsdl-description-imports description))
             (write-string "<wsdl:import" s)
             (%wsdl-write-attr s "namespace" (wsdl-import-namespace imp))
             (%wsdl-write-attr s "location"  (wsdl-import-location imp))
             (write-string " />" s))
           ;; includes
           (dolist (inc (wsdl-description-includes description))
             (write-string "<wsdl:include" s)
             (%wsdl-write-attr s "location" (wsdl-include-location inc))
             (write-string " />" s))
           ;; types
           (let ((type-nodes (wsdl-description-types description)))
             (when type-nodes
               (write-string "<wsdl:types>" s)
               (dolist (n type-nodes)
                 (%wsdl-serialize-node n s))
               (write-string "</wsdl:types>" s)))
           ;; interfaces
           (dolist (iface (wsdl-description-interfaces description))
             (%serialize-wsdl-interface iface s))
           ;; bindings
           (dolist (b (wsdl-description-bindings description))
             (%serialize-wsdl-binding b s))
           ;; services
           (dolist (svc (wsdl-description-services description))
             (%serialize-wsdl-service svc s))
           (write-string "</wsdl:description>" s)))
    (if stream
        (progn (write-it stream) nil)
        (with-output-to-string (s)
          (write-it s)))))

;;; ─── Convenience lookup functions ───────────────────────────────────────

(defun wsdl-find-interface (description name)
  "Return the wsdl-interface struct with NAME from DESCRIPTION, or NIL."
  (find name (wsdl-description-interfaces description)
        :key #'wsdl-interface-name :test #'string=))

(defun wsdl-find-binding (description name)
  "Return the wsdl-binding struct with NAME from DESCRIPTION, or NIL."
  (find name (wsdl-description-bindings description)
        :key #'wsdl-binding-name :test #'string=))

(defun wsdl-find-service (description name)
  "Return the wsdl-service struct with NAME from DESCRIPTION, or NIL."
  (find name (wsdl-description-services description)
        :key #'wsdl-service-name :test #'string=))
