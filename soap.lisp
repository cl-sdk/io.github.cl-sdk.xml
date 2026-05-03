(in-package #:cl-xml.soap)

;;;; SOAP (Simple Object Access Protocol) — 1.1 and 1.2 implementation
;;;;
;;;; Supported:
;;;;   SOAP 1.1  (namespace http://schemas.xmlsoap.org/soap/envelope/)
;;;;   SOAP 1.2  (namespace http://www.w3.org/2003/05/soap-envelope)
;;;;
;;;; Public API:
;;;;   Structures  — soap-envelope, soap-header, soap-body, soap-fault
;;;;   Condition   — soap-error
;;;;   Entry points — parse-soap, serialize-soap, soap-make-envelope

;;; ─── Namespace URI constants ─────────────────────────────────────────────

(defparameter +soap-1.1-namespace+
  "http://schemas.xmlsoap.org/soap/envelope/"
  "SOAP 1.1 envelope namespace URI.")

(defparameter +soap-1.2-namespace+
  "http://www.w3.org/2003/05/soap-envelope"
  "SOAP 1.2 envelope namespace URI.")

;;; ─── Structures ───────────────────────────────────────────────────────────

(defstruct soap-envelope
  "A parsed SOAP message envelope.
VERSION is :1.1 or :1.2.
HEADER is a soap-header struct, or NIL when no soap:Header element is present.
BODY is a soap-body struct."
  (version :1.1)
  header
  body)

(defstruct soap-header
  "A parsed SOAP Header element.
ENTRIES is a list of xml-node structs — the immediate element children of the
soap:Header element, each representing one header block."
  (entries '()))

(defstruct soap-body
  "A parsed SOAP Body element.
FAULT is a soap-fault struct when the Body contains a Fault element, or NIL.
PAYLOAD is a list of xml-node structs — the element children of the Body when
no Fault is present."
  fault
  (payload '()))

(defstruct soap-fault
  "A parsed SOAP Fault element (compatible with both SOAP 1.1 and 1.2).
CODE is the fault code string
  (SOAP 1.1 faultcode text; SOAP 1.2 Code/Value text).
STRING is the human-readable fault message
  (SOAP 1.1 faultstring text; SOAP 1.2 Reason/Text text).
ACTOR is the URI string identifying the node that faulted, or NIL
  (SOAP 1.1 faultactor text; SOAP 1.2 Role text).
DETAIL is the xml-node representing the detail/Detail element, or NIL.
LANG is the BCP 47 language tag for the SOAP 1.2 Reason/Text xml:lang attribute
  (default \"en\"); ignored when serializing SOAP 1.1."
  code
  string
  actor
  detail
  (lang "en"))

;;; ─── SOAP error condition ─────────────────────────────────────────────────

(define-condition soap-error (error)
  ((message :initarg :message :reader soap-error-message)
   (path    :initarg :path    :reader soap-error-path
            :initform nil))
  (:report (lambda (c s)
             (if (soap-error-path c)
                 (format s "SOAP error at ~a: ~a"
                         (soap-error-path c)
                         (soap-error-message c))
                 (format s "SOAP error: ~a"
                         (soap-error-message c)))))
  (:documentation
   "Condition signaled when a SOAP message cannot be parsed or is malformed.
MESSAGE is a string describing the problem; PATH is the element location, or NIL."))

;;; ─── Internal helpers — tag name extraction ──────────────────────────────

(defun %soap-local (node)
  "Return the local (unprefixed) name of xml-node NODE's tag.
After RESOLVE-NAMESPACES the tag is always an XML-QNAME; the string fallback
handles nodes that have not been through namespace resolution (e.g., raw
soap-fragment-wrapper children created directly from PARSE-XML)."
  (let ((tag (xml-node-tag node)))
    (if (xml-qname-p tag)
        (xml-qname-local-name tag)
        (let ((colon (position #\: tag)))
          (if colon (subseq tag (1+ colon)) tag)))))

(defun %soap-ns-uri (node)
  "Return the namespace URI of xml-node NODE's tag, or NIL."
  (let ((tag (xml-node-tag node)))
    (when (xml-qname-p tag)
      (xml-qname-namespace-uri tag))))

(defun %soap-element-children (node)
  "Return only the xml-node children of NODE."
  (remove-if-not #'xml-node-p (xml-node-children node)))

(defun %soap-text-content (node)
  "Return the concatenated text content of xml-node NODE."
  (with-output-to-string (out)
    (dolist (child (xml-node-children node))
      (typecase child
        (string    (write-string child out))
        (xml-cdata (write-string (xml-cdata-data child) out))))))

;;; ─── Internal helpers — XML character escaping ───────────────────────────

(defun %soap-escape-text (str)
  "Escape STR for inclusion in XML character data."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;"  out))
        (#\> (write-string "&gt;"  out))
        (t   (write-char ch out))))))

(defun %soap-escape-attr (str)
  "Escape STR for inclusion in a double-quoted XML attribute value."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\& (write-string "&amp;"  out))
        (#\< (write-string "&lt;"   out))
        (#\" (write-string "&quot;" out))
        (t   (write-char ch out))))))

;;; ─── Internal helpers — XML serializer ───────────────────────────────────

(defun %soap-tag-string (tag)
  "Return the string form of TAG (a plain string or xml-qname)."
  (if (xml-qname-p tag)
      (let ((prefix (xml-qname-prefix tag))
            (local  (xml-qname-local-name tag)))
        (if prefix
            (concatenate 'string prefix ":" local)
            local))
      tag))

(defun %soap-collect-ns-decls (node)
  "Return an alist of (prefix . uri) pairs for the namespace declarations
implied by the tag and attributes of NODE (does not recurse into children)."
  (let ((decls '()))
    (flet ((add (prefix uri)
             (when (and prefix uri
                        (not (assoc prefix decls :test #'string=)))
               (push (cons prefix uri) decls))))
      (let ((tag (xml-node-tag node)))
        (when (xml-qname-p tag)
          (add (xml-qname-prefix tag) (xml-qname-namespace-uri tag))))
      (dolist (attr (xml-node-attributes node))
        (let ((k (car attr)))
          (when (xml-qname-p k)
            (add (xml-qname-prefix k) (xml-qname-namespace-uri k))))))
    (nreverse decls)))

(defun %soap-serialize-node (node stream)
  "Serialize NODE (xml-node, string, xml-comment, xml-pi, or xml-cdata) to STREAM."
  (typecase node
    (string
     (write-string (%soap-escape-text node) stream))
    (xml-node
     (let* ((tag    (xml-node-tag node))
            (tag-s  (%soap-tag-string tag))
            (attrs  (xml-node-attributes node))
            (decls  (%soap-collect-ns-decls node)))
       (write-char #\< stream)
       (write-string tag-s stream)
       ;; Emit namespace declarations derived from the qname tag / attributes.
       (dolist (decl decls)
         (format stream " xmlns:~a=\"~a\""
                 (car decl) (%soap-escape-attr (cdr decl))))
       ;; Emit attributes.
       (dolist (attr attrs)
         (let ((k (car attr))
               (v (cdr attr)))
           (format stream " ~a=\"~a\""
                   (%soap-tag-string k) (%soap-escape-attr v))))
       ;; Children or self-close.
       (if (xml-node-children node)
           (progn
             (write-char #\> stream)
             (dolist (child (xml-node-children node))
               (%soap-serialize-node child stream))
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

;;; ─── Internal helpers — SOAP fault serialization ────────────────────────

(defun %serialize-soap-fault-1.1 (fault stream)
  "Serialize FAULT as a SOAP 1.1 Fault element to STREAM."
  (write-string "<soap:Fault>" stream)
  (when (soap-fault-code fault)
    (format stream "<faultcode>~a</faultcode>"
            (%soap-escape-text (soap-fault-code fault))))
  (when (soap-fault-string fault)
    (format stream "<faultstring>~a</faultstring>"
            (%soap-escape-text (soap-fault-string fault))))
  (when (soap-fault-actor fault)
    (format stream "<faultactor>~a</faultactor>"
            (%soap-escape-text (soap-fault-actor fault))))
  (when (soap-fault-detail fault)
    (write-string "<detail>" stream)
    (%soap-serialize-node (soap-fault-detail fault) stream)
    (write-string "</detail>" stream))
  (write-string "</soap:Fault>" stream))

(defun %serialize-soap-fault-1.2 (fault stream)
  "Serialize FAULT as a SOAP 1.2 Fault element to STREAM."
  (write-string "<soap:Fault>" stream)
  (when (soap-fault-code fault)
    (format stream "<soap:Code><soap:Value>~a</soap:Value></soap:Code>"
            (%soap-escape-text (soap-fault-code fault))))
  (when (soap-fault-string fault)
    (let ((lang (or (soap-fault-lang fault) "en")))
      (format stream "<soap:Reason><soap:Text xml:lang=\"~a\">~a</soap:Text></soap:Reason>"
              (%soap-escape-attr lang)
              (%soap-escape-text (soap-fault-string fault)))))
  (when (soap-fault-actor fault)
    (format stream "<soap:Role>~a</soap:Role>"
            (%soap-escape-text (soap-fault-actor fault))))
  (when (soap-fault-detail fault)
    (write-string "<soap:Detail>" stream)
    (%soap-serialize-node (soap-fault-detail fault) stream)
    (write-string "</soap:Detail>" stream))
  (write-string "</soap:Fault>" stream))

;;; ─── Public: serialize-soap ──────────────────────────────────────────────

(defun serialize-soap (envelope &key stream)
  "Serialize ENVELOPE (a soap-envelope struct) to XML.

When STREAM is NIL (the default), returns the XML as a fresh string.
When STREAM is a character output stream, writes the XML to it and returns NIL.

The output is a complete XML document beginning with an XML declaration,
using the namespace prefix 'soap' for the SOAP envelope namespace."
  (flet ((write-it (s)
           (let* ((version (soap-envelope-version envelope))
                  (ns      (if (eq version :1.2)
                               +soap-1.2-namespace+
                               +soap-1.1-namespace+)))
             (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" s)
             (format s "<soap:Envelope xmlns:soap=\"~a\">" ns)
             ;; Header (optional)
             (let ((hdr (soap-envelope-header envelope)))
               (when hdr
                 (write-string "<soap:Header>" s)
                 (dolist (entry (soap-header-entries hdr))
                   (%soap-serialize-node entry s))
                 (write-string "</soap:Header>" s)))
             ;; Body
             (write-string "<soap:Body>" s)
             (let* ((body  (soap-envelope-body envelope))
                    (fault (soap-body-fault body)))
               (if fault
                   (if (eq version :1.2)
                       (%serialize-soap-fault-1.2 fault s)
                       (%serialize-soap-fault-1.1 fault s))
                   (dolist (node (soap-body-payload body))
                     (%soap-serialize-node node s))))
             (write-string "</soap:Body>" s)
             (write-string "</soap:Envelope>" s))))
    (if stream
        (progn (write-it stream) nil)
        (with-output-to-string (s)
          (write-it s)))))

;;; ─── Internal helpers — SOAP fault parsing ───────────────────────────────

(defun %parse-soap-fault-1.1 (fault-node)
  "Parse a SOAP 1.1 soap:Fault element into a soap-fault struct."
  (let (code string actor detail)
    (dolist (child (%soap-element-children fault-node))
      (let ((local (%soap-local child)))
        (cond
          ((string= local "faultcode")   (setf code   (%soap-text-content child)))
          ((string= local "faultstring") (setf string (%soap-text-content child)))
          ((string= local "faultactor")  (setf actor  (%soap-text-content child)))
          ((string= local "detail")      (setf detail child)))))
    (make-soap-fault :code code :string string :actor actor :detail detail)))

(defun %parse-soap-fault-1.2 (fault-node)
  "Parse a SOAP 1.2 env:Fault element into a soap-fault struct."
  (let (code string lang actor detail)
    (dolist (child (%soap-element-children fault-node))
      (let ((local (%soap-local child)))
        (cond
          ((string= local "Code")
           (let ((val (find-if (lambda (c) (string= "Value" (%soap-local c)))
                               (%soap-element-children child))))
             (when val (setf code (%soap-text-content val)))))
          ((string= local "Reason")
           (let ((text (find-if (lambda (c) (string= "Text" (%soap-local c)))
                                (%soap-element-children child))))
             (when text
               (setf string (%soap-text-content text))
               ;; Extract xml:lang attribute (may be a plain string or xml-qname key)
               (setf lang
                     (cdr (find-if (lambda (attr)
                                     (let ((k (car attr)))
                                       (string= "lang"
                                                (if (xml-qname-p k)
                                                    (xml-qname-local-name k)
                                                    (let ((c (position #\: k)))
                                                      (if c (subseq k (1+ c)) k))))))
                                   (xml-node-attributes text)))))))
          ((string= local "Role")
           (setf actor (%soap-text-content child)))
          ((string= local "Detail")
           (setf detail child)))))
    (make-soap-fault :code code :string string :lang (or lang "en")
                     :actor actor :detail detail)))

;;; ─── Public: parse-soap ──────────────────────────────────────────────────

(defun parse-soap (input)
  "Parse INPUT (a string or character stream) as a SOAP 1.1 or SOAP 1.2 message.

Returns a soap-envelope struct.  Namespace prefixes are resolved so that the
SOAP version is detected from the envelope namespace URI regardless of what
prefix the document author chose.

Signals soap-error if:
  - The document root element is not a SOAP Envelope.
  - The namespace URI is not a recognised SOAP version.
  - The Envelope has no Body element."
  (let* ((doc      (parse-xml input))
         (resolved (resolve-namespaces doc))
         (root     (xml-document-root resolved))
         (root-local (%soap-local root))
         (root-ns    (%soap-ns-uri root)))
    ;; Must be an Envelope element.
    (unless (string= root-local "Envelope")
      (error 'soap-error
             :message (format nil
                         "Expected SOAP Envelope element, found '~a'"
                         root-local)))
    ;; Detect SOAP version from namespace URI.
    (let ((version
           (cond
             ((string= root-ns +soap-1.1-namespace+) :1.1)
             ((string= root-ns +soap-1.2-namespace+) :1.2)
             (t (error 'soap-error
                       :message (format nil
                                   "Unknown SOAP namespace URI '~a'"
                                   root-ns))))))
      ;; Locate Header and Body children.
      (let (header-node body-node)
        (dolist (child (%soap-element-children root))
          (let ((local (%soap-local child)))
            (cond
              ((string= local "Header") (setf header-node child))
              ((string= local "Body")   (setf body-node   child)))))
        (unless body-node
          (error 'soap-error :message "SOAP Envelope has no Body element"))
        ;; Build soap-header (may be NIL).
        (let* ((header
                (when header-node
                  (make-soap-header
                   :entries (%soap-element-children header-node))))
               ;; Build soap-body: Fault or payload.
               (body-children (%soap-element-children body-node))
               (fault-child
                (find-if (lambda (c) (string= "Fault" (%soap-local c)))
                         body-children))
               (body
                (if fault-child
                    (make-soap-body
                     :fault (if (eq version :1.2)
                                (%parse-soap-fault-1.2 fault-child)
                                (%parse-soap-fault-1.1 fault-child)))
                    (make-soap-body :payload body-children))))
          (make-soap-envelope :version version :header header :body body))))))

;;; ─── Public: soap-make-envelope ──────────────────────────────────────────

(defun soap-make-envelope (body-xml &key (version :1.1) header-xml)
  "Convenience constructor: build a soap-envelope from XML string fragments.

BODY-XML is an XML string whose top-level elements become the Body payload.
VERSION is :1.1 (default) or :1.2.
HEADER-XML, when provided, is an XML string whose top-level elements become
the Header entries.

The XML fragments are wrapped in a temporary root element before parsing so
that multiple sibling elements are accepted.  Namespace declarations are
resolved so that tags in the returned xml-nodes carry xml-qname structs.
Returns a soap-envelope struct."
  (flet ((parse-fragment (xml)
           (let* ((wrapped  (concatenate 'string
                                         "<soap-fragment-wrapper>" xml
                                         "</soap-fragment-wrapper>"))
                  (doc      (parse-xml wrapped))
                  (resolved (resolve-namespaces doc)))
             (remove-if-not #'xml-node-p
                            (xml-node-children (xml-document-root resolved))))))
    (let* ((body-nodes   (parse-fragment body-xml))
           (header-nodes (when header-xml (parse-fragment header-xml)))
           (header       (when header-nodes
                           (make-soap-header :entries header-nodes)))
           (body         (make-soap-body :payload body-nodes)))
      (make-soap-envelope :version version :header header :body body))))
