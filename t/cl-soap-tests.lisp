(in-package #:io.github.cl-sdk.soap.test)

(def-suite cl-soap-suite
  :description "Test suite for cl-soap.")

(in-suite cl-soap-suite)

;;; ── SOAP ─────────────────────────────────────────────────────────────────

;;; Namespace URI constants

(test soap-namespace-constants
  "SOAP namespace URI constants have the correct values."
  (is (string= "http://schemas.xmlsoap.org/soap/envelope/"
               io.github.cl-sdk.soap:+soap-1.1-namespace+))
  (is (string= "http://www.w3.org/2003/05/soap-envelope"
               io.github.cl-sdk.soap:+soap-1.2-namespace+)))

;;; Structure construction

(test soap-envelope-struct
  "soap-envelope struct can be constructed and its fields read back."
  (let* ((body (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env  (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body)))
    (is (io.github.cl-sdk.soap:soap-envelope-p env))
    (is (eq :1.1 (io.github.cl-sdk.soap:soap-envelope-version env)))
    (is (null (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (io.github.cl-sdk.soap:soap-body-p (io.github.cl-sdk.soap:soap-envelope-body env)))))

(test soap-header-struct
  "soap-header struct stores entries correctly."
  (let ((hdr (io.github.cl-sdk.soap:make-soap-header :entries '())))
    (is (io.github.cl-sdk.soap:soap-header-p hdr))
    (is (null (io.github.cl-sdk.soap:soap-header-entries hdr)))))

(test soap-body-struct
  "soap-body struct stores payload correctly."
  (let ((body (io.github.cl-sdk.soap:make-soap-body :payload '())))
    (is (io.github.cl-sdk.soap:soap-body-p body))
    (is (null (io.github.cl-sdk.soap:soap-body-fault body)))
    (is (null (io.github.cl-sdk.soap:soap-body-payload body)))))

(test soap-fault-struct
  "soap-fault struct stores all fields correctly."
  (let ((f (io.github.cl-sdk.soap:make-soap-fault
            :code   "soap:Client"
            :string "Bad request"
            :actor  "http://example.com/"
            :detail nil)))
    (is (io.github.cl-sdk.soap:soap-fault-p f))
    (is (string= "soap:Client"         (io.github.cl-sdk.soap:soap-fault-code f)))
    (is (string= "Bad request"         (io.github.cl-sdk.soap:soap-fault-string f)))
    (is (string= "http://example.com/" (io.github.cl-sdk.soap:soap-fault-actor f)))
    (is (null (io.github.cl-sdk.soap:soap-fault-detail f)))))

;;; parse-soap — SOAP 1.1

(defparameter +soap-1.1-minimal+
  (concatenate 'string
    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
    "<soap:Body>"
    "<m:GetPrice xmlns:m=\"http://example.com/\"><m:Item>Widget</m:Item></m:GetPrice>"
    "</soap:Body>"
    "</soap:Envelope>"))

(test parse-soap-1.1-basic
  "parse-soap returns a soap-envelope with version :1.1 for a SOAP 1.1 message."
  (let ((env (io.github.cl-sdk.soap:parse-soap +soap-1.1-minimal+)))
    (is (io.github.cl-sdk.soap:soap-envelope-p env))
    (is (eq :1.1 (io.github.cl-sdk.soap:soap-envelope-version env)))
    (is (null (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (io.github.cl-sdk.soap:soap-body-p (io.github.cl-sdk.soap:soap-envelope-body env)))
    (is (null (io.github.cl-sdk.soap:soap-body-fault (io.github.cl-sdk.soap:soap-envelope-body env))))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-body-payload
                      (io.github.cl-sdk.soap:soap-envelope-body env)))))))

(test parse-soap-1.1-body-element-name
  "The body payload element is the GetPrice element."
  (let* ((env  (io.github.cl-sdk.soap:parse-soap +soap-1.1-minimal+))
         (body (io.github.cl-sdk.soap:soap-envelope-body env))
         (elem (first (io.github.cl-sdk.soap:soap-body-payload body)))
         (tag  (io.github.cl-sdk.xml:xml-node-tag elem)))
    (is (io.github.cl-sdk.xml:xml-qname-p tag))
    (is (string= "GetPrice" (io.github.cl-sdk.xml:xml-qname-local-name tag)))))

;;; parse-soap — SOAP 1.2

(defparameter +soap-1.2-minimal+
  (concatenate 'string
    "<env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\">"
    "<env:Body>"
    "<m:Echo xmlns:m=\"http://example.com/\"><m:text>hello</m:text></m:Echo>"
    "</env:Body>"
    "</env:Envelope>"))

(test parse-soap-1.2-basic
  "parse-soap returns a soap-envelope with version :1.2 for a SOAP 1.2 message."
  (let ((env (io.github.cl-sdk.soap:parse-soap +soap-1.2-minimal+)))
    (is (io.github.cl-sdk.soap:soap-envelope-p env))
    (is (eq :1.2 (io.github.cl-sdk.soap:soap-envelope-version env)))
    (is (null (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-body-payload
                      (io.github.cl-sdk.soap:soap-envelope-body env)))))))

;;; parse-soap — Header

(defparameter +soap-1.1-with-header+
  (concatenate 'string
    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
    "<soap:Header>"
    "<m:Auth xmlns:m=\"http://example.com/\"><m:token>abc</m:token></m:Auth>"
    "</soap:Header>"
    "<soap:Body>"
    "<m:Ping xmlns:m=\"http://example.com/\" />"
    "</soap:Body>"
    "</soap:Envelope>"))

(test parse-soap-with-header
  "parse-soap populates soap-header with the header block elements."
  (let* ((env (io.github.cl-sdk.soap:parse-soap +soap-1.1-with-header+))
         (hdr (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (io.github.cl-sdk.soap:soap-header-p hdr))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-header-entries hdr))))
    (let* ((entry (first (io.github.cl-sdk.soap:soap-header-entries hdr)))
           (tag   (io.github.cl-sdk.xml:xml-node-tag entry)))
      (is (io.github.cl-sdk.xml:xml-qname-p tag))
      (is (string= "Auth" (io.github.cl-sdk.xml:xml-qname-local-name tag))))))

;;; parse-soap — SOAP 1.1 Fault

(defparameter +soap-1.1-fault+
  (concatenate 'string
    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
    "<soap:Body>"
    "<soap:Fault>"
    "<faultcode>soap:Client</faultcode>"
    "<faultstring>Invalid input</faultstring>"
    "<faultactor>http://example.com/service</faultactor>"
    "<detail><err xmlns=\"http://example.com/\">code 42</err></detail>"
    "</soap:Fault>"
    "</soap:Body>"
    "</soap:Envelope>"))

(test parse-soap-1.1-fault-detected
  "parse-soap sets soap-body-fault for a SOAP 1.1 Fault body."
  (let* ((env   (io.github.cl-sdk.soap:parse-soap +soap-1.1-fault+))
         (body  (io.github.cl-sdk.soap:soap-envelope-body env))
         (fault (io.github.cl-sdk.soap:soap-body-fault body)))
    (is (io.github.cl-sdk.soap:soap-fault-p fault))
    (is (null (io.github.cl-sdk.soap:soap-body-payload body)))))

(test parse-soap-1.1-fault-code
  "SOAP 1.1 fault code is extracted from faultcode text."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.1-fault+)))))
    (is (string= "soap:Client" (io.github.cl-sdk.soap:soap-fault-code fault)))))

(test parse-soap-1.1-fault-string
  "SOAP 1.1 fault string is extracted from faultstring text."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.1-fault+)))))
    (is (string= "Invalid input" (io.github.cl-sdk.soap:soap-fault-string fault)))))

(test parse-soap-1.1-fault-actor
  "SOAP 1.1 fault actor is extracted from faultactor text."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.1-fault+)))))
    (is (string= "http://example.com/service" (io.github.cl-sdk.soap:soap-fault-actor fault)))))

(test parse-soap-1.1-fault-detail-present
  "SOAP 1.1 fault detail is the detail xml-node."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.1-fault+)))))
    (is (io.github.cl-sdk.xml:xml-node-p (io.github.cl-sdk.soap:soap-fault-detail fault)))
    (is (string= "detail"
                 (let ((tag (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.soap:soap-fault-detail fault))))
                   (if (io.github.cl-sdk.xml:xml-qname-p tag)
                       (io.github.cl-sdk.xml:xml-qname-local-name tag)
                       tag))))))

;;; parse-soap — SOAP 1.2 Fault

(defparameter +soap-1.2-fault+
  (concatenate 'string
    "<env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\">"
    "<env:Body>"
    "<env:Fault>"
    "<env:Code><env:Value>env:Sender</env:Value></env:Code>"
    "<env:Reason><env:Text xml:lang=\"en\">Bad request</env:Text></env:Reason>"
    "<env:Role>http://example.com/node</env:Role>"
    "<env:Detail><m:err xmlns:m=\"http://example.com/\">42</m:err></env:Detail>"
    "</env:Fault>"
    "</env:Body>"
    "</env:Envelope>"))

(test parse-soap-1.2-fault-code
  "SOAP 1.2 fault code is extracted from Code/Value text."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.2-fault+)))))
    (is (io.github.cl-sdk.soap:soap-fault-p fault))
    (is (string= "env:Sender" (io.github.cl-sdk.soap:soap-fault-code fault)))))

(test parse-soap-1.2-fault-string
  "SOAP 1.2 fault string is extracted from Reason/Text text."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.2-fault+)))))
    (is (string= "Bad request" (io.github.cl-sdk.soap:soap-fault-string fault)))))

(test parse-soap-1.2-fault-actor
  "SOAP 1.2 Role is mapped to soap-fault-actor."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.2-fault+)))))
    (is (string= "http://example.com/node" (io.github.cl-sdk.soap:soap-fault-actor fault)))))

(test parse-soap-1.2-fault-detail-present
  "SOAP 1.2 fault Detail is the Detail xml-node."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.2-fault+)))))
    (is (io.github.cl-sdk.xml:xml-node-p (io.github.cl-sdk.soap:soap-fault-detail fault)))
    (is (string= "Detail"
                 (let ((tag (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.soap:soap-fault-detail fault))))
                   (if (io.github.cl-sdk.xml:xml-qname-p tag)
                       (io.github.cl-sdk.xml:xml-qname-local-name tag)
                       tag))))))

(test parse-soap-1.2-fault-lang
  "SOAP 1.2 fault lang is extracted from Reason/Text xml:lang attribute."
  (let* ((fault (io.github.cl-sdk.soap:soap-body-fault
                 (io.github.cl-sdk.soap:soap-envelope-body
                  (io.github.cl-sdk.soap:parse-soap +soap-1.2-fault+)))))
    (is (string= "en" (io.github.cl-sdk.soap:soap-fault-lang fault)))))

;;; parse-soap — error cases

(test parse-soap-not-envelope-error
  "parse-soap signals soap-error when root element is not Envelope."
  (signals io.github.cl-sdk.soap:soap-error
    (io.github.cl-sdk.soap:parse-soap
     "<root xmlns=\"http://schemas.xmlsoap.org/soap/envelope/\" />")))

(test parse-soap-unknown-namespace-error
  "parse-soap signals soap-error for an unknown SOAP namespace URI."
  (signals io.github.cl-sdk.soap:soap-error
    (io.github.cl-sdk.soap:parse-soap
     "<s:Envelope xmlns:s=\"http://example.com/soap\"><s:Body /></s:Envelope>")))

(test parse-soap-no-body-error
  "parse-soap signals soap-error when the Envelope has no Body."
  (signals io.github.cl-sdk.soap:soap-error
    (io.github.cl-sdk.soap:parse-soap
     "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" />")))

;;; serialize-soap

(defun parse-soap-from-string (str)
  "Round-trip helper: parse STR as a SOAP envelope."
  (io.github.cl-sdk.soap:parse-soap str))

(test serialize-soap-returns-string
  "serialize-soap returns a non-empty string by default."
  (let* ((body (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env  (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body))
         (xml  (io.github.cl-sdk.soap:serialize-soap env)))
    (is (stringp xml))
    (is (plusp (length xml)))))

(test serialize-soap-contains-envelope-tag
  "The serialized string contains soap:Envelope."
  (let* ((body (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env  (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body))
         (xml  (io.github.cl-sdk.soap:serialize-soap env)))
    (is (search "Envelope" xml))))

(test serialize-soap-1.1-namespace-present
  "Serialized SOAP 1.1 output contains the SOAP 1.1 namespace URI."
  (let* ((body (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env  (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body))
         (xml  (io.github.cl-sdk.soap:serialize-soap env)))
    (is (search "schemas.xmlsoap.org/soap/envelope/" xml))))

(test serialize-soap-1.2-namespace-present
  "Serialized SOAP 1.2 output contains the SOAP 1.2 namespace URI."
  (let* ((body (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env  (io.github.cl-sdk.soap:make-soap-envelope :version :1.2 :body body))
         (xml  (io.github.cl-sdk.soap:serialize-soap env)))
    (is (search "w3.org/2003/05/soap-envelope" xml))))

(test serialize-soap-to-stream
  "serialize-soap writes to a supplied stream and returns nil."
  (let* ((body   (io.github.cl-sdk.soap:make-soap-body :payload '()))
         (env    (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body))
         result)
    (with-output-to-string (s)
      (setf result (io.github.cl-sdk.soap:serialize-soap env :stream s)))
    (is (null result))))

(test serialize-soap-roundtrip-1.1
  "Serializing and re-parsing a SOAP 1.1 envelope preserves version and body."
  (let* ((env (io.github.cl-sdk.soap:parse-soap +soap-1.1-minimal+))
         (xml (io.github.cl-sdk.soap:serialize-soap env))
         (env2 (io.github.cl-sdk.soap:parse-soap xml)))
    (is (eq :1.1 (io.github.cl-sdk.soap:soap-envelope-version env2)))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-body-payload
                      (io.github.cl-sdk.soap:soap-envelope-body env2)))))))

(test serialize-soap-roundtrip-1.2
  "Serializing and re-parsing a SOAP 1.2 envelope preserves version."
  (let* ((env  (io.github.cl-sdk.soap:parse-soap +soap-1.2-minimal+))
         (xml  (io.github.cl-sdk.soap:serialize-soap env))
         (env2 (io.github.cl-sdk.soap:parse-soap xml)))
    (is (eq :1.2 (io.github.cl-sdk.soap:soap-envelope-version env2)))))

(test serialize-soap-roundtrip-header
  "Serializing and re-parsing preserves the header block count."
  (let* ((env  (io.github.cl-sdk.soap:parse-soap +soap-1.1-with-header+))
         (xml  (io.github.cl-sdk.soap:serialize-soap env))
         (env2 (io.github.cl-sdk.soap:parse-soap xml))
         (hdr  (io.github.cl-sdk.soap:soap-envelope-header env2)))
    (is (io.github.cl-sdk.soap:soap-header-p hdr))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-header-entries hdr))))))

(test serialize-soap-1.1-fault-roundtrip
  "Serializing and re-parsing a SOAP 1.1 fault preserves code and string."
  (let* ((fault-in (io.github.cl-sdk.soap:make-soap-fault
                    :code "soap:Server" :string "Internal error"))
         (body     (io.github.cl-sdk.soap:make-soap-body :fault fault-in))
         (env      (io.github.cl-sdk.soap:make-soap-envelope :version :1.1 :body body))
         (xml      (io.github.cl-sdk.soap:serialize-soap env))
         (env2     (io.github.cl-sdk.soap:parse-soap xml))
         (fault    (io.github.cl-sdk.soap:soap-body-fault (io.github.cl-sdk.soap:soap-envelope-body env2))))
    (is (io.github.cl-sdk.soap:soap-fault-p fault))
    (is (string= "soap:Server"    (io.github.cl-sdk.soap:soap-fault-code fault)))
    (is (string= "Internal error" (io.github.cl-sdk.soap:soap-fault-string fault)))))

(test serialize-soap-1.2-fault-roundtrip
  "Serializing and re-parsing a SOAP 1.2 fault preserves code and string."
  (let* ((fault-in (io.github.cl-sdk.soap:make-soap-fault
                    :code "env:Receiver" :string "Processing failed"))
         (body     (io.github.cl-sdk.soap:make-soap-body :fault fault-in))
         (env      (io.github.cl-sdk.soap:make-soap-envelope :version :1.2 :body body))
         (xml      (io.github.cl-sdk.soap:serialize-soap env))
         (env2     (io.github.cl-sdk.soap:parse-soap xml))
         (fault    (io.github.cl-sdk.soap:soap-body-fault (io.github.cl-sdk.soap:soap-envelope-body env2))))
    (is (io.github.cl-sdk.soap:soap-fault-p fault))
    (is (string= "env:Receiver"     (io.github.cl-sdk.soap:soap-fault-code fault)))
    (is (string= "Processing failed" (io.github.cl-sdk.soap:soap-fault-string fault)))))

;;; soap-make-envelope

(test soap-make-envelope-basic
  "soap-make-envelope wraps body XML in a SOAP 1.1 envelope."
  (let* ((env (io.github.cl-sdk.soap:soap-make-envelope
               "<m:GetUser xmlns:m=\"http://example.com/\"><m:id>1</m:id></m:GetUser>"))
         (body (io.github.cl-sdk.soap:soap-envelope-body env)))
    (is (io.github.cl-sdk.soap:soap-envelope-p env))
    (is (eq :1.1 (io.github.cl-sdk.soap:soap-envelope-version env)))
    (is (null (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-body-payload body))))
    (is (string= "GetUser"
                 (io.github.cl-sdk.xml:xml-qname-local-name
                  (io.github.cl-sdk.xml:xml-node-tag
                   (first (io.github.cl-sdk.soap:soap-body-payload body))))))))

(test soap-make-envelope-version-1.2
  "soap-make-envelope respects the :version :1.2 keyword argument."
  (let ((env (io.github.cl-sdk.soap:soap-make-envelope
              "<ping />" :version :1.2)))
    (is (eq :1.2 (io.github.cl-sdk.soap:soap-envelope-version env)))))

(test soap-make-envelope-with-header
  "soap-make-envelope with :header-xml adds header entries."
  (let* ((env (io.github.cl-sdk.soap:soap-make-envelope
               "<ping />"
               :header-xml "<auth><token>xyz</token></auth>"))
         (hdr (io.github.cl-sdk.soap:soap-envelope-header env)))
    (is (io.github.cl-sdk.soap:soap-header-p hdr))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-header-entries hdr))))
    (let ((tag (io.github.cl-sdk.xml:xml-node-tag (first (io.github.cl-sdk.soap:soap-header-entries hdr)))))
      (is (string= "auth"
                   (if (io.github.cl-sdk.xml:xml-qname-p tag)
                       (io.github.cl-sdk.xml:xml-qname-local-name tag)
                       tag))))))

(test soap-make-envelope-serialize-roundtrip
  "An envelope built with soap-make-envelope survives a serialize/parse round-trip."
  (let* ((env  (io.github.cl-sdk.soap:soap-make-envelope
                "<m:Op xmlns:m=\"http://example.com/\" />" :version :1.1))
         (xml  (io.github.cl-sdk.soap:serialize-soap env))
         (env2 (io.github.cl-sdk.soap:parse-soap xml)))
    (is (eq :1.1 (io.github.cl-sdk.soap:soap-envelope-version env2)))
    (is (= 1 (length (io.github.cl-sdk.soap:soap-body-payload
                      (io.github.cl-sdk.soap:soap-envelope-body env2)))))))

(test soap-error-condition
  "soap-error condition reports message correctly."
  (let ((err (make-condition 'io.github.cl-sdk.soap:soap-error :message "test error")))
    (is (string= "test error" (io.github.cl-sdk.soap:soap-error-message err)))
    (is (null (io.github.cl-sdk.soap:soap-error-path err)))
    (is (search "test error" (format nil "~a" err)))))


