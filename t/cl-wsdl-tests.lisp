(in-package #:io.github.cl-sdk.wsdl.test)

(def-suite cl-wsdl-suite
  :description "Test suite for cl-wsdl.")

(in-suite cl-wsdl-suite)

;;; ── WSDL ─────────────────────────────────────────────────────────────────

;;; A minimal but complete WSDL 2.0 document used by many tests below.
(defparameter +simple-wsdl+
  "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/hello\">
  <wsdl:interface name=\"HelloInterface\">
    <wsdl:operation name=\"sayHello\"
                    pattern=\"http://www.w3.org/ns/wsdl/in-out\">
      <wsdl:input  element=\"tns:SayHelloRequest\" />
      <wsdl:output element=\"tns:SayHelloResponse\" />
    </wsdl:operation>
  </wsdl:interface>
  <wsdl:binding name=\"HelloBinding\"
                interface=\"tns:HelloInterface\"
                type=\"http://www.w3.org/ns/wsdl/soap\">
    <wsdl:operation ref=\"tns:sayHello\" />
  </wsdl:binding>
  <wsdl:service name=\"HelloService\"
                interface=\"tns:HelloInterface\">
    <wsdl:endpoint name=\"HelloEndpoint\"
                   binding=\"tns:HelloBinding\"
                   address=\"http://example.com/hello\" />
  </wsdl:service>
</wsdl:description>")

(test wsdl-namespace-constant
  "The WSDL 2.0 namespace URI constant has the correct value."
  (is (string= "http://www.w3.org/ns/wsdl"
               io.github.cl-sdk.wsdl:+wsdl-2.0-namespace+)))

(test parse-wsdl-returns-description
  "parse-wsdl returns a wsdl-description struct."
  (let ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+)))
    (is (io.github.cl-sdk.wsdl:wsdl-description-p desc))))

(test parse-wsdl-target-namespace
  "parse-wsdl captures the targetNamespace attribute."
  (let ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+)))
    (is (string= "http://example.com/hello"
                 (io.github.cl-sdk.wsdl:wsdl-description-target-namespace desc)))))

(test parse-wsdl-interface-count
  "parse-wsdl populates the interfaces list."
  (let ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+)))
    (is (= 1 (length (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc))))))

(test parse-wsdl-interface-name
  "parse-wsdl captures the interface name."
  (let* ((desc  (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (iface (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc))))
    (is (io.github.cl-sdk.wsdl:wsdl-interface-p iface))
    (is (string= "HelloInterface" (io.github.cl-sdk.wsdl:wsdl-interface-name iface)))))

(test parse-wsdl-interface-operation
  "parse-wsdl parses wsdl:operation inside wsdl:interface."
  (let* ((desc  (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (iface (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc)))
         (op    (first (io.github.cl-sdk.wsdl:wsdl-interface-operations iface))))
    (is (io.github.cl-sdk.wsdl:wsdl-interface-operation-p op))
    (is (string= "sayHello" (io.github.cl-sdk.wsdl:wsdl-interface-operation-name op)))
    (is (string= "http://www.w3.org/ns/wsdl/in-out"
                 (io.github.cl-sdk.wsdl:wsdl-interface-operation-pattern op)))))

(test parse-wsdl-operation-input-output
  "parse-wsdl records wsdl:input and wsdl:output as wsdl-message-ref structs."
  (let* ((desc  (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (op    (first (io.github.cl-sdk.wsdl:wsdl-interface-operations
                        (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc)))))
         (in    (first (io.github.cl-sdk.wsdl:wsdl-interface-operation-inputs op)))
         (out   (first (io.github.cl-sdk.wsdl:wsdl-interface-operation-outputs op))))
    (is (io.github.cl-sdk.wsdl:wsdl-message-ref-p in))
    (is (string= "tns:SayHelloRequest" (io.github.cl-sdk.wsdl:wsdl-message-ref-element in)))
    (is (io.github.cl-sdk.wsdl:wsdl-message-ref-p out))
    (is (string= "tns:SayHelloResponse" (io.github.cl-sdk.wsdl:wsdl-message-ref-element out)))))

(test parse-wsdl-binding
  "parse-wsdl parses wsdl:binding with name, interface, and type."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (binding (first (io.github.cl-sdk.wsdl:wsdl-description-bindings desc))))
    (is (io.github.cl-sdk.wsdl:wsdl-binding-p binding))
    (is (string= "HelloBinding"  (io.github.cl-sdk.wsdl:wsdl-binding-name binding)))
    (is (string= "tns:HelloInterface" (io.github.cl-sdk.wsdl:wsdl-binding-interface binding)))
    (is (string= "http://www.w3.org/ns/wsdl/soap"
                 (io.github.cl-sdk.wsdl:wsdl-binding-type binding)))))

(test parse-wsdl-binding-operation
  "parse-wsdl parses wsdl:operation inside wsdl:binding."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (binding (first (io.github.cl-sdk.wsdl:wsdl-description-bindings desc)))
         (bop     (first (io.github.cl-sdk.wsdl:wsdl-binding-operations binding))))
    (is (io.github.cl-sdk.wsdl:wsdl-binding-operation-p bop))
    (is (string= "tns:sayHello" (io.github.cl-sdk.wsdl:wsdl-binding-operation-ref bop)))))

(test parse-wsdl-service
  "parse-wsdl parses wsdl:service."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (service (first (io.github.cl-sdk.wsdl:wsdl-description-services desc))))
    (is (io.github.cl-sdk.wsdl:wsdl-service-p service))
    (is (string= "HelloService"      (io.github.cl-sdk.wsdl:wsdl-service-name service)))
    (is (string= "tns:HelloInterface" (io.github.cl-sdk.wsdl:wsdl-service-interface service)))))

(test parse-wsdl-endpoint
  "parse-wsdl parses wsdl:endpoint inside wsdl:service."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (service (first (io.github.cl-sdk.wsdl:wsdl-description-services desc)))
         (ep      (first (io.github.cl-sdk.wsdl:wsdl-service-endpoints service))))
    (is (io.github.cl-sdk.wsdl:wsdl-endpoint-p ep))
    (is (string= "HelloEndpoint"    (io.github.cl-sdk.wsdl:wsdl-endpoint-name ep)))
    (is (string= "tns:HelloBinding" (io.github.cl-sdk.wsdl:wsdl-endpoint-binding ep)))
    (is (string= "http://example.com/hello" (io.github.cl-sdk.wsdl:wsdl-endpoint-address ep)))))

(test parse-wsdl-interface-fault
  "parse-wsdl parses wsdl:fault inside wsdl:interface."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:interface name=\"FaultIface\">
    <wsdl:fault name=\"NotFound\" element=\"tns:NotFoundFault\" />
  </wsdl:interface>
</wsdl:description>"))
         (iface (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc)))
         (fault (first (io.github.cl-sdk.wsdl:wsdl-interface-faults iface))))
    (is (io.github.cl-sdk.wsdl:wsdl-interface-fault-p fault))
    (is (string= "NotFound" (io.github.cl-sdk.wsdl:wsdl-interface-fault-name fault)))
    (is (string= "tns:NotFoundFault" (io.github.cl-sdk.wsdl:wsdl-interface-fault-element fault)))))

(test parse-wsdl-infault-outfault
  "parse-wsdl parses wsdl:infault and wsdl:outfault inside wsdl:operation."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:interface name=\"FaultOp\">
    <wsdl:operation name=\"op1\"
                    pattern=\"http://www.w3.org/ns/wsdl/in-out\">
      <wsdl:input  element=\"tns:Req\" />
      <wsdl:output element=\"tns:Resp\" />
      <wsdl:infault  ref=\"tns:InputFault\" />
      <wsdl:outfault ref=\"tns:OutputFault\" />
    </wsdl:operation>
  </wsdl:interface>
</wsdl:description>"))
         (op (first (io.github.cl-sdk.wsdl:wsdl-interface-operations
                     (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc))))))
    (let ((inf  (first (io.github.cl-sdk.wsdl:wsdl-interface-operation-in-faults op)))
          (outf (first (io.github.cl-sdk.wsdl:wsdl-interface-operation-out-faults op))))
      (is (io.github.cl-sdk.wsdl:wsdl-fault-ref-p inf))
      (is (string= "tns:InputFault"  (io.github.cl-sdk.wsdl:wsdl-fault-ref-ref inf)))
      (is (io.github.cl-sdk.wsdl:wsdl-fault-ref-p outf))
      (is (string= "tns:OutputFault" (io.github.cl-sdk.wsdl:wsdl-fault-ref-ref outf))))))

(test parse-wsdl-interface-extends
  "parse-wsdl captures the extends attribute as a list of names."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:interface name=\"Child\" extends=\"tns:Base1 tns:Base2\" />
</wsdl:description>"))
         (iface (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc))))
    (is (equal '("tns:Base1" "tns:Base2")
               (io.github.cl-sdk.wsdl:wsdl-interface-extends iface)))))

(test parse-wsdl-import
  "parse-wsdl captures wsdl:import elements."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:import namespace=\"http://other.example.com/\"
               location=\"other.wsdl\" />
</wsdl:description>"))
         (imp (first (io.github.cl-sdk.wsdl:wsdl-description-imports desc))))
    (is (io.github.cl-sdk.wsdl:wsdl-import-p imp))
    (is (string= "http://other.example.com/" (io.github.cl-sdk.wsdl:wsdl-import-namespace imp)))
    (is (string= "other.wsdl" (io.github.cl-sdk.wsdl:wsdl-import-location imp)))))

(test parse-wsdl-include
  "parse-wsdl captures wsdl:include elements."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:include location=\"common.wsdl\" />
</wsdl:description>"))
         (inc (first (io.github.cl-sdk.wsdl:wsdl-description-includes desc))))
    (is (io.github.cl-sdk.wsdl:wsdl-include-p inc))
    (is (string= "common.wsdl" (io.github.cl-sdk.wsdl:wsdl-include-location inc)))))

(test parse-wsdl-types-preserved
  "parse-wsdl stores type children as xml-nodes."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:types>
    <xs:schema targetNamespace=\"http://example.com/\">
      <xs:element name=\"Foo\" type=\"xs:string\" />
    </xs:schema>
  </wsdl:types>
</wsdl:description>")))
    (is (= 1 (length (io.github.cl-sdk.wsdl:wsdl-description-types desc))))
    (is (io.github.cl-sdk.xml:xml-node-p (first (io.github.cl-sdk.wsdl:wsdl-description-types desc))))))

(test parse-wsdl-binding-fault
  "parse-wsdl parses wsdl:fault inside wsdl:binding."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:binding name=\"B\" interface=\"tns:I\"
                type=\"http://www.w3.org/ns/wsdl/soap\">
    <wsdl:fault ref=\"tns:NotFound\" code=\"soap:Sender\" />
  </wsdl:binding>
</wsdl:description>"))
         (bf (first (io.github.cl-sdk.wsdl:wsdl-binding-faults
                     (first (io.github.cl-sdk.wsdl:wsdl-description-bindings desc))))))
    (is (io.github.cl-sdk.wsdl:wsdl-binding-fault-p bf))
    (is (string= "tns:NotFound" (io.github.cl-sdk.wsdl:wsdl-binding-fault-ref bf)))
    (is (string= "soap:Sender"  (io.github.cl-sdk.wsdl:wsdl-binding-fault-code bf)))))

(test parse-wsdl-wrong-root-signals-error
  "parse-wsdl signals wsdl-error when the root element is not wsdl:description."
  (signals io.github.cl-sdk.wsdl:wsdl-error
    (io.github.cl-sdk.wsdl:parse-wsdl
     "<?xml version=\"1.0\"?>
<wsdl:definitions xmlns:wsdl=\"http://www.w3.org/ns/wsdl\" />")))

(test parse-wsdl-wrong-namespace-signals-error
  "parse-wsdl signals wsdl-error when the namespace URI is not WSDL 2.0."
  (signals io.github.cl-sdk.wsdl:wsdl-error
    (io.github.cl-sdk.wsdl:parse-wsdl
     "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\" />")))

(test serialize-wsdl-returns-string
  "serialize-wsdl with no :stream argument returns a string."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (out  (io.github.cl-sdk.wsdl:serialize-wsdl desc)))
    (is (stringp out))))

(test serialize-wsdl-contains-declaration
  "serialize-wsdl output starts with an XML declaration."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (out  (io.github.cl-sdk.wsdl:serialize-wsdl desc)))
    (is (search "<?xml" out))))

(test serialize-wsdl-contains-wsdl-namespace
  "serialize-wsdl output declares the WSDL 2.0 namespace."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (out  (io.github.cl-sdk.wsdl:serialize-wsdl desc)))
    (is (search "http://www.w3.org/ns/wsdl" out))))

(test serialize-wsdl-to-stream
  "serialize-wsdl writes to a supplied output stream and returns NIL."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (ret  nil))
    (with-output-to-string (s)
      (setf ret (io.github.cl-sdk.wsdl:serialize-wsdl desc :stream s)))
    (is (null ret))))

(test serialize-wsdl-roundtrip-interface
  "serialize-wsdl output can be re-parsed to recover interface data."
  (let* ((desc1 (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (xml   (io.github.cl-sdk.wsdl:serialize-wsdl desc1))
         (desc2 (io.github.cl-sdk.wsdl:parse-wsdl xml)))
    (is (= (length (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc1))
           (length (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc2))))
    (is (string= (io.github.cl-sdk.wsdl:wsdl-interface-name
                  (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc1)))
                 (io.github.cl-sdk.wsdl:wsdl-interface-name
                  (first (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc2)))))))

(test serialize-wsdl-roundtrip-binding
  "serialize-wsdl output can be re-parsed to recover binding data."
  (let* ((desc1 (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (xml   (io.github.cl-sdk.wsdl:serialize-wsdl desc1))
         (desc2 (io.github.cl-sdk.wsdl:parse-wsdl xml)))
    (is (= (length (io.github.cl-sdk.wsdl:wsdl-description-bindings desc1))
           (length (io.github.cl-sdk.wsdl:wsdl-description-bindings desc2))))
    (is (string= (io.github.cl-sdk.wsdl:wsdl-binding-name
                  (first (io.github.cl-sdk.wsdl:wsdl-description-bindings desc1)))
                 (io.github.cl-sdk.wsdl:wsdl-binding-name
                  (first (io.github.cl-sdk.wsdl:wsdl-description-bindings desc2)))))))

(test serialize-wsdl-roundtrip-service
  "serialize-wsdl output can be re-parsed to recover service and endpoint data."
  (let* ((desc1 (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (xml   (io.github.cl-sdk.wsdl:serialize-wsdl desc1))
         (desc2 (io.github.cl-sdk.wsdl:parse-wsdl xml)))
    (let ((svc1 (first (io.github.cl-sdk.wsdl:wsdl-description-services desc1)))
          (svc2 (first (io.github.cl-sdk.wsdl:wsdl-description-services desc2))))
      (is (string= (io.github.cl-sdk.wsdl:wsdl-service-name svc1)
                   (io.github.cl-sdk.wsdl:wsdl-service-name svc2)))
      (is (string= (io.github.cl-sdk.wsdl:wsdl-endpoint-address
                    (first (io.github.cl-sdk.wsdl:wsdl-service-endpoints svc1)))
                   (io.github.cl-sdk.wsdl:wsdl-endpoint-address
                    (first (io.github.cl-sdk.wsdl:wsdl-service-endpoints svc2))))))))

(test wsdl-find-interface-found
  "wsdl-find-interface returns the matching interface struct."
  (let* ((desc  (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (iface (io.github.cl-sdk.wsdl:wsdl-find-interface desc "HelloInterface")))
    (is (io.github.cl-sdk.wsdl:wsdl-interface-p iface))
    (is (string= "HelloInterface" (io.github.cl-sdk.wsdl:wsdl-interface-name iface)))))

(test wsdl-find-interface-not-found
  "wsdl-find-interface returns NIL when the name does not exist."
  (let ((desc (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+)))
    (is (null (io.github.cl-sdk.wsdl:wsdl-find-interface desc "NoSuchInterface")))))

(test wsdl-find-binding-found
  "wsdl-find-binding returns the matching binding struct."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (binding (io.github.cl-sdk.wsdl:wsdl-find-binding desc "HelloBinding")))
    (is (io.github.cl-sdk.wsdl:wsdl-binding-p binding))
    (is (string= "HelloBinding" (io.github.cl-sdk.wsdl:wsdl-binding-name binding)))))

(test wsdl-find-service-found
  "wsdl-find-service returns the matching service struct."
  (let* ((desc    (io.github.cl-sdk.wsdl:parse-wsdl +simple-wsdl+))
         (service (io.github.cl-sdk.wsdl:wsdl-find-service desc "HelloService")))
    (is (io.github.cl-sdk.wsdl:wsdl-service-p service))
    (is (string= "HelloService" (io.github.cl-sdk.wsdl:wsdl-service-name service)))))

(test wsdl-error-condition
  "wsdl-error condition reports message and path correctly."
  (let ((err (make-condition 'io.github.cl-sdk.wsdl:wsdl-error
                             :message "bad WSDL"
                             :path "wsdl:description")))
    (is (string= "bad WSDL" (io.github.cl-sdk.wsdl:wsdl-error-message err)))
    (is (string= "wsdl:description" (io.github.cl-sdk.wsdl:wsdl-error-path err)))
    (is (search "bad WSDL" (format nil "~a" err)))))

(test wsdl-error-condition-no-path
  "wsdl-error condition without a path omits the path in the message."
  (let ((err (make-condition 'io.github.cl-sdk.wsdl:wsdl-error :message "test")))
    (is (null (io.github.cl-sdk.wsdl:wsdl-error-path err)))
    (is (search "test" (format nil "~a" err)))))

(test wsdl-description-struct
  "make-wsdl-description creates a struct with default empty slots."
  (let ((d (io.github.cl-sdk.wsdl:make-wsdl-description)))
    (is (io.github.cl-sdk.wsdl:wsdl-description-p d))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-target-namespace d)))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-imports d)))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-interfaces d)))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-bindings d)))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-services d)))))

(test parse-wsdl-no-target-namespace
  "parse-wsdl accepts a description without targetNamespace."
  (let ((desc (io.github.cl-sdk.wsdl:parse-wsdl
               "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\">
</wsdl:description>")))
    (is (io.github.cl-sdk.wsdl:wsdl-description-p desc))
    (is (null (io.github.cl-sdk.wsdl:wsdl-description-target-namespace desc)))))

(test parse-wsdl-multiple-interfaces
  "parse-wsdl collects all wsdl:interface children in order."
  (let* ((desc (io.github.cl-sdk.wsdl:parse-wsdl
                "<?xml version=\"1.0\"?>
<wsdl:description xmlns:wsdl=\"http://www.w3.org/ns/wsdl\"
                  targetNamespace=\"http://example.com/\">
  <wsdl:interface name=\"Alpha\" />
  <wsdl:interface name=\"Beta\" />
  <wsdl:interface name=\"Gamma\" />
</wsdl:description>"))
         (ifaces (io.github.cl-sdk.wsdl:wsdl-description-interfaces desc)))
    (is (= 3 (length ifaces)))
    (is (string= "Alpha" (io.github.cl-sdk.wsdl:wsdl-interface-name (first  ifaces))))
    (is (string= "Beta"  (io.github.cl-sdk.wsdl:wsdl-interface-name (second ifaces))))
    (is (string= "Gamma" (io.github.cl-sdk.wsdl:wsdl-interface-name (third  ifaces))))))
