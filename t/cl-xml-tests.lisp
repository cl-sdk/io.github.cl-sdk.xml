(in-package #:cl-xml.test)

(def-suite cl-xml-suite
  :description "Test suite for cl-xml.")

(in-suite cl-xml-suite)

;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun parse-root (str)
  "Parse STR and return the root xml-node."
  (cl-xml:xml-document-root (cl-xml:parse-xml str)))

;;; ── Original regression tests ────────────────────────────────────────────

(test self-closing-node
  "A self-closing element is parsed into an xml-node with no children."
  (let ((node (parse-root "<tag />")))
    (is (string= "tag" (cl-xml:xml-node-tag node)))
    (is (null (cl-xml:xml-node-attributes node)))
    (is (null (cl-xml:xml-node-children node)))))

(test self-closing-node-with-attribute
  "A self-closing element with one attribute."
  (let ((node (parse-root "<img src=\"logo.png\" />")))
    (is (string= "img" (cl-xml:xml-node-tag node)))
    (is (equal '(("src" . "logo.png")) (cl-xml:xml-node-attributes node)))
    (is (null (cl-xml:xml-node-children node)))))

(test empty-element
  "An element with an explicit closing tag but no children."
  (let ((node (parse-root "<div></div>")))
    (is (string= "div" (cl-xml:xml-node-tag node)))
    (is (null (cl-xml:xml-node-attributes node)))
    (is (null (cl-xml:xml-node-children node)))))

(test element-with-attribute
  "An element with one attribute and an explicit closing tag."
  (let ((node (parse-root "<a href=\"https://example.com\"></a>")))
    (is (string= "a" (cl-xml:xml-node-tag node)))
    (is (equal '(("href" . "https://example.com"))
               (cl-xml:xml-node-attributes node)))
    (is (null (cl-xml:xml-node-children node)))))

(test element-with-children
  "An element whose children are parsed recursively."
  (let ((root (parse-root "<root><child /></root>")))
    (is (string= "root" (cl-xml:xml-node-tag root)))
    (is (= 1 (length (cl-xml:xml-node-children root))))
    (let ((child (first (cl-xml:xml-node-children root))))
      (is (string= "child" (cl-xml:xml-node-tag child)))
      (is (null (cl-xml:xml-node-children child))))))

(test problem-statement-example
  "Parse the full example from the problem statement."
  (let* ((xml "
<root>
    <node-without-children attribute-name=\"attribute-value\" />
    <node-with-children attribute-name=\"attribute-value\">
    </node-with-children>
</root>")
         (root (parse-root xml)))
    (is (string= "root" (cl-xml:xml-node-tag root)))
    (is (null (cl-xml:xml-node-attributes root)))
    (is (= 2 (length (cl-xml:xml-node-children root))))
    (let ((self-closing (first (cl-xml:xml-node-children root)))
          (with-children (second (cl-xml:xml-node-children root))))
      ;; self-closing child
      (is (string= "node-without-children" (cl-xml:xml-node-tag self-closing)))
      (is (equal '(("attribute-name" . "attribute-value"))
                 (cl-xml:xml-node-attributes self-closing)))
      (is (null (cl-xml:xml-node-children self-closing)))
      ;; child with explicit closing tag
      (is (string= "node-with-children" (cl-xml:xml-node-tag with-children)))
      (is (equal '(("attribute-name" . "attribute-value"))
                 (cl-xml:xml-node-attributes with-children)))
      (is (null (cl-xml:xml-node-children with-children))))))

(test multiple-attributes
  "An element with multiple attributes preserves order."
  (let ((node (parse-root "<el a=\"1\" b=\"2\" c=\"3\" />")))
    (is (equal '(("a" . "1") ("b" . "2") ("c" . "3"))
               (cl-xml:xml-node-attributes node)))))

(test deeply-nested
  "Deeply nested elements are parsed correctly."
  (let* ((root (parse-root "<a><b><c /></b></a>"))
         (b (first (cl-xml:xml-node-children root)))
         (c (first (cl-xml:xml-node-children b))))
    (is (string= "a" (cl-xml:xml-node-tag root)))
    (is (string= "b" (cl-xml:xml-node-tag b)))
    (is (string= "c" (cl-xml:xml-node-tag c)))
    (is (null (cl-xml:xml-node-children c)))))

;;; ── xml-document structure ───────────────────────────────────────────────

(test parse-xml-returns-document
  "parse-xml returns an xml-document with prolog and root fields."
  (let ((doc (cl-xml:parse-xml "<root />")))
    (is (cl-xml:xml-document-p doc))
    (is (null (cl-xml:xml-document-prolog doc)))
    (is (cl-xml:xml-node-p (cl-xml:xml-document-root doc)))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

;;; ── XML 1.0 conformance — Names (§2.3) ──────────────────────────────────

(test valid-name-with-underscore-and-hyphen
  "Names may contain underscores, hyphens, and dots after the first char."
  (let ((node (parse-root "<_my-tag.1 />")))
    (is (string= "_my-tag.1" (cl-xml:xml-node-tag node)))))

(test valid-name-with-colon
  "Colons are valid XML name characters."
  (let ((node (parse-root "<ns:tag />")))
    (is (string= "ns:tag" (cl-xml:xml-node-tag node)))))

(test invalid-name-start-digit
  "A name starting with a digit is a well-formedness error."
  (signals error (cl-xml:parse-xml "<1tag />")))

(test invalid-name-start-hyphen
  "A name starting with a hyphen is a well-formedness error."
  (signals error (cl-xml:parse-xml "<-tag />")))

;;; ── XML 1.0 conformance — Attributes (§3.1, §3.3.3) ─────────────────────

(test duplicate-attribute-error
  "Duplicate attribute names on the same element are a well-formedness error."
  (signals error (cl-xml:parse-xml "<el a=\"1\" a=\"2\" />")))

(test lt-in-attribute-value-error
  "A literal '<' inside an attribute value is a well-formedness error."
  (signals error (cl-xml:parse-xml "<el a=\"x<y\" />")))

;;; ── XML 1.0 conformance — Entity references (§4.6) ──────────────────────

(test entity-ref-amp-in-attribute
  "&amp; in an attribute value expands to '&'."
  (let ((node (parse-root "<el v=\"a&amp;b\" />")))
    (is (string= "a&b" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-lt-in-attribute
  "&lt; in an attribute value expands to '<'."
  (let ((node (parse-root "<el v=\"a&lt;b\" />")))
    (is (string= "a<b" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-gt-in-attribute
  "&gt; in an attribute value expands to '>'."
  (let ((node (parse-root "<el v=\"a&gt;b\" />")))
    (is (string= "a>b" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-quot-in-attribute
  "&quot; in a single-quoted attribute value expands to '\"'."
  (let ((node (parse-root "<el v='a&quot;b' />")))
    (is (string= "a\"b" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                    :test #'string=))))))

(test entity-ref-apos-in-attribute
  "&apos; in a double-quoted attribute value expands to \"'\"."
  (let ((node (parse-root "<el v=\"a&apos;b\" />")))
    (is (string= "a'b" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                   :test #'string=))))))

(test unknown-entity-ref-error
  "An unknown named entity reference signals an error."
  (signals error (cl-xml:parse-xml "<el v=\"&unknown;\" />")))

;;; ── XML 1.0 conformance — Character references (§4.1) ───────────────────

(test char-ref-decimal
  "A decimal character reference &#65; expands to 'A'."
  (let ((node (parse-root "<el v=\"&#65;\" />")))
    (is (string= "A" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                 :test #'string=))))))

(test char-ref-hex
  "A hexadecimal character reference &#x41; expands to 'A'."
  (let ((node (parse-root "<el v=\"&#x41;\" />")))
    (is (string= "A" (cdr (assoc "v" (cl-xml:xml-node-attributes node)
                                 :test #'string=))))))

;;; ── XML 1.0 conformance — Character data (§2.4) ─────────────────────────

(test text-content-preserved
  "Non-whitespace text content is preserved as a string child."
  (let* ((root (parse-root "<p>hello</p>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (stringp (first children)))
    (is (string= "hello" (first children)))))

(test text-with-entity-ref
  "Entity references in text content are expanded."
  (let* ((root (parse-root "<p>a&amp;b</p>"))
         (children (cl-xml:xml-node-children root)))
    (is (string= "a&b" (first children)))))

(test whitespace-only-text-discarded
  "Whitespace-only text runs between elements do not become text children."
  (let ((root (parse-root "<root>
  <a />
  <b />
</root>")))
    (is (= 2 (length (cl-xml:xml-node-children root))))
    (is (every #'cl-xml:xml-node-p (cl-xml:xml-node-children root)))))

;;; ── XML 1.0 conformance — CDATA sections (§2.7) ─────────────────────────

(test cdata-section-preserved-as-node
  "A CDATA section is preserved as an xml-cdata child node."
  (let* ((root (parse-root "<el><![CDATA[a<b>&c]]></el>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (cl-xml:xml-cdata-p (first children)))
    (is (string= "a<b>&c" (cl-xml:xml-cdata-data (first children))))))

(test cdata-whitespace-only-preserved
  "A CDATA section containing only whitespace is still preserved as a node."
  (let* ((root (parse-root "<el><![CDATA[   ]]></el>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (cl-xml:xml-cdata-p (first children)))
    (is (string= "   " (cl-xml:xml-cdata-data (first children))))))

;;; ── XML 1.0 conformance — Comments (§2.5) ───────────────────────────────

(test comment-preserved-as-node
  "Comments inside an element are preserved as xml-comment child nodes."
  (let* ((root (parse-root "<root><!-- hello --><child /></root>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 2 (length children)))
    (is (cl-xml:xml-comment-p (first children)))
    (is (string= " hello " (cl-xml:xml-comment-data (first children))))
    (is (cl-xml:xml-node-p (second children)))
    (is (string= "child" (cl-xml:xml-node-tag (second children))))))

(test multiple-comments-in-element
  "Multiple comments between elements are each preserved as xml-comment nodes."
  (let* ((root (parse-root "<root><!-- a --><!-- b --><x /></root>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 3 (length children)))
    (is (cl-xml:xml-comment-p (first children)))
    (is (string= " a " (cl-xml:xml-comment-data (first children))))
    (is (cl-xml:xml-comment-p (second children)))
    (is (string= " b " (cl-xml:xml-comment-data (second children))))
    (is (cl-xml:xml-node-p (third children)))))

(test illegal-double-dash-in-comment
  "The sequence '--' inside a comment (not as part of '-->') is an error."
  (signals error (cl-xml:parse-xml "<el><!-- bad -- comment --></el>")))

;;; ── XML 1.0 conformance — Processing instructions (§2.6) ────────────────

(test pi-preserved-as-node
  "Processing instructions inside an element are preserved as xml-pi nodes."
  (let* ((root (parse-root "<root><?app data?><child /></root>"))
         (children (cl-xml:xml-node-children root)))
    (is (= 2 (length children)))
    (is (cl-xml:xml-pi-p (first children)))
    (is (string= "app" (cl-xml:xml-pi-target (first children))))
    (is (string= "data" (cl-xml:xml-pi-data (first children))))
    (is (cl-xml:xml-node-p (second children)))))

(test pi-no-data
  "A processing instruction with no data has an empty data string."
  (let* ((root (parse-root "<el><?foo?></el>"))
         (pinstruction (first (cl-xml:xml-node-children root))))
    (is (cl-xml:xml-pi-p pinstruction))
    (is (string= "foo" (cl-xml:xml-pi-target pinstruction)))
    (is (string= "" (cl-xml:xml-pi-data pinstruction)))))

;;; ── XML 1.0 conformance — Prolog (§2.8) ─────────────────────────────────

(test xml-declaration-in-prolog
  "An XML declaration is preserved as an xml-pi node in the document prolog."
  (let ((doc (cl-xml:parse-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root />")))
    (is (= 1 (length (cl-xml:xml-document-prolog doc))))
    (let ((decl (first (cl-xml:xml-document-prolog doc))))
      (is (cl-xml:xml-pi-p decl))
      (is (string= "xml" (cl-xml:xml-pi-target decl))))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

(test doctype-skipped
  "A DOCTYPE declaration in the prolog is silently skipped."
  (let ((doc (cl-xml:parse-xml "<!DOCTYPE root><root />")))
    (is (null (cl-xml:xml-document-prolog doc)))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

(test doctype-with-internal-subset-skipped
  "A DOCTYPE with an internal subset is silently skipped."
  (let ((doc (cl-xml:parse-xml "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />")))
    (is (null (cl-xml:xml-document-prolog doc)))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

(test prolog-comment-preserved
  "A comment in the document prolog is preserved as an xml-comment node."
  (let ((doc (cl-xml:parse-xml "<!-- intro --><root />")))
    (is (= 1 (length (cl-xml:xml-document-prolog doc))))
    (is (cl-xml:xml-comment-p (first (cl-xml:xml-document-prolog doc))))
    (is (string= " intro "
                 (cl-xml:xml-comment-data
                  (first (cl-xml:xml-document-prolog doc)))))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

(test prolog-pi-preserved
  "A processing instruction in the document prolog is preserved as xml-pi."
  (let ((doc (cl-xml:parse-xml "<?stylesheet type=\"text/xsl\" href=\"a.xsl\"?><root />")))
    (is (= 1 (length (cl-xml:xml-document-prolog doc))))
    (is (cl-xml:xml-pi-p (first (cl-xml:xml-document-prolog doc))))
    (is (string= "stylesheet"
                 (cl-xml:xml-pi-target (first (cl-xml:xml-document-prolog doc)))))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

;;; ── SAX handler — collecting helper ─────────────────────────────────────

(defclass collecting-handler (cl-xml:sax-handler)
  ((events :initform '() :accessor handler-events))
  (:documentation "SAX handler that accumulates all events into a list."))

(defmethod cl-xml:start-element ((h collecting-handler) tag attributes)
  (push (list :start-element tag attributes) (handler-events h)))

(defmethod cl-xml:end-element ((h collecting-handler) tag)
  (push (list :end-element tag) (handler-events h)))

(defmethod cl-xml:characters ((h collecting-handler) text)
  (push (list :characters text) (handler-events h)))

(defmethod cl-xml:comment ((h collecting-handler) data)
  (push (list :comment data) (handler-events h)))

(defmethod cl-xml:processing-instruction ((h collecting-handler) target data)
  (push (list :pi target data) (handler-events h)))

(defmethod cl-xml:cdata-section ((h collecting-handler) data)
  (push (list :cdata data) (handler-events h)))

(defmethod cl-xml:end-document ((h collecting-handler))
  (nreverse (handler-events h)))

(defun sax-events (str)
  "Parse STR with a COLLECTING-HANDLER and return the event list."
  (cl-xml:parse-xml str :handler (make-instance 'collecting-handler)))

;;; ── SAX handler tests ─────────────────────────────────────────────────────

(test sax-default-returns-document
  "parse-xml without :handler returns an xml-document (backward compat)."
  (let ((doc (cl-xml:parse-xml "<root />")))
    (is (cl-xml:xml-document-p doc))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))))

(test sax-self-closing-events
  "A self-closing element fires start-element then end-element."
  (let ((events (sax-events "<tag />")))
    (is (equal '((:start-element "tag" nil)
                 (:end-element   "tag"))
               events))))

(test sax-self-closing-with-attributes
  "A self-closing element fires start-element with the attribute alist."
  (let ((events (sax-events "<img src=\"logo.png\" />")))
    (is (equal '((:start-element "img" (("src" . "logo.png")))
                 (:end-element   "img"))
               events))))

(test sax-nested-element-event-order
  "Nested elements produce events in the correct depth-first order."
  (let ((events (sax-events "<a><b /></a>")))
    (is (equal '((:start-element "a" nil)
                 (:start-element "b" nil)
                 (:end-element   "b")
                 (:end-element   "a"))
               events))))

(test sax-text-content-event
  "Text content fires a CHARACTERS event with the expanded string."
  (let ((events (sax-events "<p>hello &amp; world</p>")))
    (is (equal '((:start-element "p" nil)
                 (:characters    "hello & world")
                 (:end-element   "p"))
               events))))

(test sax-whitespace-text-reported
  "Custom handlers receive whitespace-only character runs (unlike dom-builder)."
  (let ((events (sax-events "<root>
  <a />
</root>")))
    ;; The custom handler sees all characters; filter to :characters events.
    (let ((char-events (remove-if-not (lambda (e) (eq :characters (first e)))
                                      events)))
      (is (plusp (length char-events))))))

(test sax-comment-event
  "Comments fire a COMMENT event with the raw body."
  (let ((events (sax-events "<root><!-- hello --></root>")))
    (is (equal '((:start-element "root" nil)
                 (:comment       " hello ")
                 (:end-element   "root"))
               events))))

(test sax-pi-event
  "Processing instructions fire a PROCESSING-INSTRUCTION event."
  (let ((events (sax-events "<root><?app data?></root>")))
    (is (equal '((:start-element "root" nil)
                 (:pi            "app" "data")
                 (:end-element   "root"))
               events))))

(test sax-cdata-event
  "CDATA sections fire a CDATA-SECTION event with the literal content."
  (let ((events (sax-events "<root><![CDATA[a<b>&c]]></root>")))
    (is (equal '((:start-element "root" nil)
                 (:cdata         "a<b>&c")
                 (:end-element   "root"))
               events))))

(test sax-prolog-pi-event
  "A prolog processing instruction fires a PROCESSING-INSTRUCTION event."
  (let ((events (sax-events "<?xml version=\"1.0\"?><root />")))
    (is (equal '((:pi            "xml" "version=\"1.0\"")
                 (:start-element "root" nil)
                 (:end-element   "root"))
               events))))

(test sax-prolog-comment-event
  "A prolog comment fires a COMMENT event."
  (let ((events (sax-events "<!-- intro --><root />")))
    (is (equal '((:comment       " intro ")
                 (:start-element "root" nil)
                 (:end-element   "root"))
               events))))

(test sax-end-document-return-value
  "The return value of END-DOCUMENT becomes the return value of PARSE-XML."
  (let ((result (cl-xml:parse-xml "<x />" :handler (make-instance 'collecting-handler))))
    (is (listp result))
    (is (equal '(:start-element "x" nil) (first result)))
    (is (equal '(:end-element   "x")     (second result)))))

(test sax-dom-builder-is-default
  "The dom-builder preserves whitespace-filtering: whitespace-only text is discarded."
  (let ((root (parse-root "<root>
  <a />
</root>")))
    (is (= 1 (length (cl-xml:xml-node-children root))))
    (is (cl-xml:xml-node-p (first (cl-xml:xml-node-children root))))))

(test full-prolog
  "A document with XML decl, DOCTYPE, and a prolog comment is parsed correctly."
  (let* ((xml (concatenate 'string
                "<?xml version=\"1.0\"?>"
                "<!DOCTYPE root>"
                "<!-- comment -->"
                "<root><child /></root>"))
         (doc (cl-xml:parse-xml xml)))
    ;; XML decl (as xml-pi) + prolog comment = 2 items; DOCTYPE is skipped
    (is (= 2 (length (cl-xml:xml-document-prolog doc))))
    (is (cl-xml:xml-pi-p (first (cl-xml:xml-document-prolog doc))))
    (is (cl-xml:xml-comment-p (second (cl-xml:xml-document-prolog doc))))
    (let ((root (cl-xml:xml-document-root doc)))
      (is (string= "root" (cl-xml:xml-node-tag root)))
      (is (= 1 (length (cl-xml:xml-node-children root)))))))

;;; ── Stream input ──────────────────────────────────────────────────────────

(test parse-xml-accepts-stream
  "parse-xml accepts a character stream in addition to a string."
  (let* ((stream (make-string-input-stream "<tag attr=\"val\">text</tag>"))
         (root   (cl-xml:xml-document-root (cl-xml:parse-xml stream))))
    (is (string= "tag" (cl-xml:xml-node-tag root)))
    (is (equal '(("attr" . "val")) (cl-xml:xml-node-attributes root)))
    (is (string= "text" (first (cl-xml:xml-node-children root))))))

;;; ── trivial-gray-streams input ────────────────────────────────────────────

;;; Minimal Gray stream that wraps a string-input-stream.
(defclass test-gray-stream (fundamental-character-input-stream)
  ((inner :initarg :inner :reader inner-stream)))

(defmethod stream-read-char ((s test-gray-stream))
  (read-char (inner-stream s) nil :eof))

(defmethod stream-unread-char ((s test-gray-stream) ch)
  (unread-char ch (inner-stream s)))

(defmethod stream-peek-char ((s test-gray-stream))
  (let ((ch (read-char (inner-stream s) nil :eof)))
    (unless (eq ch :eof)
      (unread-char ch (inner-stream s)))
    ch))

(test parse-xml-accepts-gray-stream
  "parse-xml accepts a trivial-gray-streams fundamental-character-input-stream."
  (let* ((inner  (make-string-input-stream "<el key=\"v\">hello</el>"))
         (stream (make-instance 'test-gray-stream :inner inner))
         (root   (cl-xml:xml-document-root (cl-xml:parse-xml stream))))
    (is (string= "el" (cl-xml:xml-node-tag root)))
    (is (equal '(("key" . "v")) (cl-xml:xml-node-attributes root)))
    (is (string= "hello" (first (cl-xml:xml-node-children root))))))

;;; ── Namespace resolution (resolve-namespaces) ────────────────────────────

(defun resolve-root-tag (str)
  "Parse STR, resolve namespaces, and return the tag (xml-qname) of the root xml-node."
  (cl-xml:xml-node-tag
   (cl-xml:xml-document-root
    (cl-xml:resolve-namespaces (cl-xml:parse-xml str)))))

(test ns-qname-struct
  "xml-qname struct has prefix, local-name, and namespace-uri fields."
  (let ((q (cl-xml:make-xml-qname :prefix "ns" :local-name "tag"
                                  :namespace-uri "http://example.com/")))
    (is (cl-xml:xml-qname-p q))
    (is (string= "ns"                  (cl-xml:xml-qname-prefix q)))
    (is (string= "tag"                 (cl-xml:xml-qname-local-name q)))
    (is (string= "http://example.com/" (cl-xml:xml-qname-namespace-uri q)))))

(test ns-resolve-prefixed-element
  "A prefixed element tag is resolved to an xml-qname with the correct URI."
  (let ((tag (resolve-root-tag "<ns:root xmlns:ns=\"http://example.com/\" />")))
    (is (cl-xml:xml-qname-p tag))
    (is (string= "ns"                  (cl-xml:xml-qname-prefix tag)))
    (is (string= "root"                (cl-xml:xml-qname-local-name tag)))
    (is (string= "http://example.com/" (cl-xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-default-namespace
  "An unprefixed element in a default namespace gets the declared namespace URI."
  (let ((tag (resolve-root-tag "<root xmlns=\"http://example.com/\" />")))
    (is (cl-xml:xml-qname-p tag))
    (is (null (cl-xml:xml-qname-prefix tag)))
    (is (string= "root"                (cl-xml:xml-qname-local-name tag)))
    (is (string= "http://example.com/" (cl-xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-no-namespace
  "An unprefixed element with no default namespace has nil namespace-uri."
  (let ((tag (resolve-root-tag "<root />")))
    (is (cl-xml:xml-qname-p tag))
    (is (null (cl-xml:xml-qname-prefix tag)))
    (is (string= "root" (cl-xml:xml-qname-local-name tag)))
    (is (null (cl-xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-xmlns-attrs-removed
  "xmlns and xmlns:prefix attributes are absent after resolve-namespaces."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns=\"http://a.com/\" xmlns:b=\"http://b.com/\" />"))
         (resolved (cl-xml:resolve-namespaces doc))
         (attrs    (cl-xml:xml-node-attributes
                    (cl-xml:xml-document-root resolved))))
    (is (null attrs))))

(test ns-resolve-prefixed-attribute
  "A prefixed attribute key is resolved to an xml-qname with the correct URI."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns:ns=\"http://example.com/\" ns:attr=\"val\" />"))
         (resolved (cl-xml:resolve-namespaces doc))
         (attrs    (cl-xml:xml-node-attributes
                    (cl-xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (cl-xml:xml-qname-p key))
      (is (string= "ns"                  (cl-xml:xml-qname-prefix key)))
      (is (string= "attr"                (cl-xml:xml-qname-local-name key)))
      (is (string= "http://example.com/" (cl-xml:xml-qname-namespace-uri key))))
    (is (string= "val" (cdar attrs)))))

(test ns-resolve-unprefixed-attribute-has-no-ns
  "An unprefixed attribute has nil namespace-uri even when a default ns is active."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns=\"http://example.com/\" attr=\"val\" />"))
         (resolved (cl-xml:resolve-namespaces doc))
         (attrs    (cl-xml:xml-node-attributes
                    (cl-xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (cl-xml:xml-qname-p key))
      (is (null (cl-xml:xml-qname-prefix key)))
      (is (string= "attr" (cl-xml:xml-qname-local-name key)))
      (is (null (cl-xml:xml-qname-namespace-uri key))))))

(test ns-resolve-inherited-binding
  "Namespace bindings declared on a parent are visible in child elements."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns:ns=\"http://example.com/\"><ns:child /></root>"))
         (resolved (cl-xml:resolve-namespaces doc))
         (child    (first (cl-xml:xml-node-children
                           (cl-xml:xml-document-root resolved))))
         (ctag     (cl-xml:xml-node-tag child)))
    (is (cl-xml:xml-qname-p ctag))
    (is (string= "ns"                  (cl-xml:xml-qname-prefix ctag)))
    (is (string= "child"               (cl-xml:xml-qname-local-name ctag)))
    (is (string= "http://example.com/" (cl-xml:xml-qname-namespace-uri ctag)))))

(test ns-resolve-xml-prefix-predeclared
  "The 'xml' prefix is pre-declared and resolves without an explicit xmlns:xml."
  (let* ((doc      (cl-xml:parse-xml "<root xml:lang=\"en\" />"))
         (resolved (cl-xml:resolve-namespaces doc))
         (attrs    (cl-xml:xml-node-attributes
                    (cl-xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (string= "xml"  (cl-xml:xml-qname-prefix key)))
      (is (string= "lang" (cl-xml:xml-qname-local-name key)))
      (is (string= "http://www.w3.org/XML/1998/namespace"
                   (cl-xml:xml-qname-namespace-uri key))))))

(test ns-resolve-undeclared-prefix-error
  "Using an undeclared namespace prefix signals an error."
  (signals error
    (cl-xml:resolve-namespaces (cl-xml:parse-xml "<ns:root />"))))

(test ns-resolve-default-ns-reset
  "An xmlns='' declaration on a child resets the default namespace to nil."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns=\"http://example.com/\"><child xmlns=\"\" /></root>"))
         (resolved (cl-xml:resolve-namespaces doc))
         (child    (first (cl-xml:xml-node-children
                           (cl-xml:xml-document-root resolved))))
         (ctag     (cl-xml:xml-node-tag child)))
    (is (null (cl-xml:xml-qname-namespace-uri ctag)))))

(test ns-resolve-returns-xml-document
  "resolve-namespaces returns an xml-document."
  (let ((resolved (cl-xml:resolve-namespaces (cl-xml:parse-xml "<root />"))))
    (is (cl-xml:xml-document-p resolved))))

(test ns-resolve-preserves-prolog
  "resolve-namespaces preserves the document prolog unchanged."
  (let* ((doc      (cl-xml:parse-xml "<!-- comment --><root />"))
         (resolved (cl-xml:resolve-namespaces doc)))
    (is (equal (cl-xml:xml-document-prolog doc)
               (cl-xml:xml-document-prolog resolved)))))

(test ns-resolve-multiple-prefixes
  "Multiple namespace prefixes on the same element are all resolved correctly."
  (let* ((doc      (cl-xml:parse-xml
                    (concatenate 'string
                      "<root xmlns:a=\"http://a.com/\""
                      "      xmlns:b=\"http://b.com/\""
                      "      a:x=\"1\" b:y=\"2\" />")))
         (resolved (cl-xml:resolve-namespaces doc))
         (attrs    (cl-xml:xml-node-attributes
                    (cl-xml:xml-document-root resolved))))
    (is (= 2 (length attrs)))
    (let ((a-attr (find "x" attrs :key (lambda (e) (cl-xml:xml-qname-local-name (car e)))
                                  :test #'string=))
          (b-attr (find "y" attrs :key (lambda (e) (cl-xml:xml-qname-local-name (car e)))
                                  :test #'string=)))
      (is (string= "http://a.com/" (cl-xml:xml-qname-namespace-uri (car a-attr))))
      (is (string= "http://b.com/" (cl-xml:xml-qname-namespace-uri (car b-attr)))))))

(test ns-resolve-preserves-text-children
  "Text and CDATA children are passed through unchanged by resolve-namespaces."
  (let* ((doc      (cl-xml:parse-xml
                    "<root xmlns=\"http://example.com/\">hello</root>"))
         (resolved (cl-xml:resolve-namespaces doc))
         (children (cl-xml:xml-node-children
                    (cl-xml:xml-document-root resolved))))
    (is (= 1 (length children)))
    (is (string= "hello" (first children)))))

;;; ── DTD element parsing ───────────────────────────────────────────────────

;;; Helpers

(defun parse-doctype-elements (str)
  "Parse STR and return the list of xml-dtd-element structs from the DOCTYPE."
  (cl-xml:xml-doctype-elements
   (cl-xml:xml-document-doctype (cl-xml:parse-xml str))))

;;; xml-dtd-element struct

(test dtd-element-struct
  "xml-dtd-element struct has name and content-model fields."
  (let ((e (cl-xml:make-xml-dtd-element :name "root" :content-model :empty)))
    (is (cl-xml:xml-dtd-element-p e))
    (is (string= "root" (cl-xml:xml-dtd-element-name e)))
    (is (eq :empty (cl-xml:xml-dtd-element-content-model e)))))

;;; xml-doctype struct

(test dtd-doctype-struct
  "xml-doctype struct has name, public-id, system-id, and elements fields."
  (let ((d (cl-xml:make-xml-doctype :name "root" :public-id nil
                                    :system-id "root.dtd" :elements '())))
    (is (cl-xml:xml-doctype-p d))
    (is (string= "root" (cl-xml:xml-doctype-name d)))
    (is (null (cl-xml:xml-doctype-public-id d)))
    (is (string= "root.dtd" (cl-xml:xml-doctype-system-id d)))
    (is (null (cl-xml:xml-doctype-elements d)))))

;;; xml-document-doctype accessor

(test no-doctype-gives-nil
  "A document without a DOCTYPE has xml-document-doctype = NIL."
  (let ((doc (cl-xml:parse-xml "<root />")))
    (is (null (cl-xml:xml-document-doctype doc)))))

(test doctype-present
  "A document with a DOCTYPE has a non-nil xml-doctype in xml-document-doctype."
  (let* ((doc (cl-xml:parse-xml "<!DOCTYPE root><root />"))
         (dtd (cl-xml:xml-document-doctype doc)))
    (is (cl-xml:xml-doctype-p dtd))
    (is (string= "root" (cl-xml:xml-doctype-name dtd)))))

(test doctype-no-external-id
  "A DOCTYPE with no external identifier has nil public-id and system-id."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml "<!DOCTYPE html><html />"))))
    (is (null (cl-xml:xml-doctype-public-id dtd)))
    (is (null (cl-xml:xml-doctype-system-id dtd)))))

(test doctype-system-id
  "A DOCTYPE with SYSTEM identifier records the system-id."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml
                "<!DOCTYPE root SYSTEM \"root.dtd\"><root />"))))
    (is (null (cl-xml:xml-doctype-public-id dtd)))
    (is (string= "root.dtd" (cl-xml:xml-doctype-system-id dtd)))))

(test doctype-public-id
  "A DOCTYPE with PUBLIC identifier records both public-id and system-id."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml
                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \"xhtml1.dtd\"><html />"))))
    (is (string= "-//W3C//DTD XHTML 1.0//EN" (cl-xml:xml-doctype-public-id dtd)))
    (is (string= "xhtml1.dtd" (cl-xml:xml-doctype-system-id dtd)))))

(test doctype-root-element-preserved
  "The root element is still correctly parsed when a DOCTYPE is present."
  (let ((doc (cl-xml:parse-xml "<!DOCTYPE root><root><child /></root>")))
    (is (string= "root" (cl-xml:xml-node-tag (cl-xml:xml-document-root doc))))
    (is (= 1 (length (cl-xml:xml-node-children
                      (cl-xml:xml-document-root doc)))))))

;;; DTD ELEMENT content models

(test dtd-element-empty
  "<!ELEMENT> with EMPTY content model is parsed as :empty."
  (let ((elems (parse-doctype-elements
                "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />")))
    (is (= 1 (length elems)))
    (let ((e (first elems)))
      (is (string= "root" (cl-xml:xml-dtd-element-name e)))
      (is (eq :empty (cl-xml:xml-dtd-element-content-model e))))))

(test dtd-element-any
  "<!ELEMENT> with ANY content model is parsed as :any."
  (let ((elems (parse-doctype-elements
                "<!DOCTYPE root [<!ELEMENT root ANY>]><root />")))
    (is (= 1 (length elems)))
    (is (eq :any (cl-xml:xml-dtd-element-content-model (first elems))))))

(test dtd-element-pcdata-only
  "<!ELEMENT> with (#PCDATA) is parsed as (:mixed)."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (#PCDATA)>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (consp cm))
    (is (eq :mixed (car cm)))
    (is (null (cdr cm)))))

(test dtd-element-mixed-content
  "<!ELEMENT> with (#PCDATA|a|b)* is parsed as (:mixed \"a\" \"b\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (#PCDATA|a|b)*>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (eq :mixed (car cm)))
    (is (equal '("a" "b") (cdr cm)))))

(test dtd-element-sequence
  "<!ELEMENT> with (a, b, c) is parsed as (:seq \"a\" \"b\" \"c\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b, c)>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:seq "a" "b" "c") cm))))

(test dtd-element-choice
  "<!ELEMENT> with (a|b) is parsed as (:choice \"a\" \"b\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a|b)>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:choice "a" "b") cm))))

(test dtd-element-sequence-plus
  "<!ELEMENT> with (a, b)+ wraps the group with :+."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b)+>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:+ (:seq "a" "b")) cm))))

(test dtd-element-choice-star
  "<!ELEMENT> with (a|b)* wraps the group with :*."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a|b)*>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:* (:choice "a" "b")) cm))))

(test dtd-element-sequence-opt
  "<!ELEMENT> with (a, b)? wraps the group with :?."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b)?>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:? (:seq "a" "b")) cm))))

(test dtd-element-leaf-quantifier
  "A leaf content particle with + quantifier is wrapped with :+."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a+)>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    ;; (a+) → single-element sequence containing (:+ "a")
    (is (equal '(:seq (:+ "a")) cm))))

(test dtd-element-nested-group
  "<!ELEMENT> with ((a,b)|c) is parsed as a nested group."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ((a,b)|c)>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:choice (:seq "a" "b") "c") cm))))

(test dtd-multiple-elements
  "Multiple <!ELEMENT> declarations are all captured in order."
  (let ((elems (parse-doctype-elements
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ELEMENT root (a, b)>"
                  "<!ELEMENT a (#PCDATA)>"
                  "<!ELEMENT b EMPTY>"
                  "]><root />"))))
    (is (= 3 (length elems)))
    (is (string= "root" (cl-xml:xml-dtd-element-name (first elems))))
    (is (string= "a"    (cl-xml:xml-dtd-element-name (second elems))))
    (is (string= "b"    (cl-xml:xml-dtd-element-name (third elems))))
    (is (equal '(:seq "a" "b")
               (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (eq :empty (cl-xml:xml-dtd-element-content-model (third elems))))))

(test dtd-attlist-coexists-with-element
  "<!ATTLIST> declaration does not interfere with <!ELEMENT> parsing."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ELEMENT root EMPTY>"
                  "<!ATTLIST root id ID #REQUIRED>"
                  "]><root />"))))
         (elems (cl-xml:xml-doctype-elements dtd)))
    (is (= 1 (length elems)))
    (is (string= "root" (cl-xml:xml-dtd-element-name (first elems))))
    (is (= 1 (length (cl-xml:xml-doctype-attlists dtd))))))

(test dtd-comment-in-subset-skipped
  "Comments inside the internal subset are silently skipped."
  (let ((elems (parse-doctype-elements
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!-- declare the root -->"
                  "<!ELEMENT root EMPTY>"
                  "]><root />"))))
    (is (= 1 (length elems)))
    (is (string= "root" (cl-xml:xml-dtd-element-name (first elems))))))

(test dtd-whitespace-in-content-model
  "Whitespace around operators in a content model is handled correctly."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ( a | b | c )>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:choice "a" "b" "c") cm))))

(test dtd-whitespace-sequence
  "Whitespace around ',' in a sequence is handled correctly."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ( a , b , c )>]><root />"))
         (cm (cl-xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:seq "a" "b" "c") cm))))

;;; SAX doctype-declaration event

(test sax-doctype-declaration-event
  "The doctype-declaration SAX event is fired with the parsed xml-doctype."
  (let* ((handler (make-instance 'collecting-handler))
         (result  (cl-xml:parse-xml
                   "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />"
                   :handler handler)))
    ;; collecting-handler doesn't define doctype-declaration, so the default
    ;; no-op is used; the event must not crash and the rest must parse.
    (is (listp result))
    (is (some (lambda (e) (equal '(:start-element "root" nil) e)) result))))

;;; ── DTD ATTLIST parsing ───────────────────────────────────────────────────

(defun parse-doctype-attlists (str)
  "Parse STR and return the list of xml-dtd-attlist structs from the DOCTYPE."
  (cl-xml:xml-doctype-attlists
   (cl-xml:xml-document-doctype (cl-xml:parse-xml str))))

;;; xml-dtd-att-def struct

(test dtd-att-def-struct
  "xml-dtd-att-def struct has name, type, and default fields."
  (let ((d (cl-xml:make-xml-dtd-att-def :name "id" :type :id :default :required)))
    (is (cl-xml:xml-dtd-att-def-p d))
    (is (string= "id" (cl-xml:xml-dtd-att-def-name d)))
    (is (eq :id (cl-xml:xml-dtd-att-def-type d)))
    (is (eq :required (cl-xml:xml-dtd-att-def-default d)))))

;;; xml-dtd-attlist struct

(test dtd-attlist-struct
  "xml-dtd-attlist struct has element-name and definitions fields."
  (let ((a (cl-xml:make-xml-dtd-attlist :element-name "root" :definitions '())))
    (is (cl-xml:xml-dtd-attlist-p a))
    (is (string= "root" (cl-xml:xml-dtd-attlist-element-name a)))
    (is (null (cl-xml:xml-dtd-attlist-definitions a)))))

;;; xml-doctype-attlists accessor

(test dtd-doctype-has-attlists
  "xml-doctype-attlists returns NIL when no ATTLIST is present."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml "<!DOCTYPE root><root />"))))
    (is (null (cl-xml:xml-doctype-attlists dtd)))))

;;; Tokenized attribute types

(test dtd-attlist-cdata-required
  "<!ATTLIST> with CDATA type and #REQUIRED default is parsed correctly."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE root [<!ATTLIST root title CDATA #REQUIRED>]><root />"))
         (al (first alists))
         (def (first (cl-xml:xml-dtd-attlist-definitions al))))
    (is (= 1 (length alists)))
    (is (string= "root" (cl-xml:xml-dtd-attlist-element-name al)))
    (is (string= "title" (cl-xml:xml-dtd-att-def-name def)))
    (is (eq :cdata (cl-xml:xml-dtd-att-def-type def)))
    (is (eq :required (cl-xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-id-implied
  "<!ATTLIST> with ID type and #IMPLIED default is parsed correctly."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE root [<!ATTLIST root id ID #IMPLIED>]><root />"))
         (def (first (cl-xml:xml-dtd-attlist-definitions (first alists)))))
    (is (eq :id (cl-xml:xml-dtd-att-def-type def)))
    (is (eq :implied (cl-xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-idref-type
  "<!ATTLIST> with IDREF type is parsed as :idref."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x IDREF #IMPLIED>]><r />"))))))
    (is (eq :idref (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-idrefs-type
  "<!ATTLIST> with IDREFS type is parsed as :idrefs."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x IDREFS #IMPLIED>]><r />"))))))
    (is (eq :idrefs (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-entity-type
  "<!ATTLIST> with ENTITY type is parsed as :entity."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x ENTITY #IMPLIED>]><r />"))))))
    (is (eq :entity (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-entities-type
  "<!ATTLIST> with ENTITIES type is parsed as :entities."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x ENTITIES #IMPLIED>]><r />"))))))
    (is (eq :entities (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-nmtoken-type
  "<!ATTLIST> with NMTOKEN type is parsed as :nmtoken."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x NMTOKEN #IMPLIED>]><r />"))))))
    (is (eq :nmtoken (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-nmtokens-type
  "<!ATTLIST> with NMTOKENS type is parsed as :nmtokens."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x NMTOKENS #IMPLIED>]><r />"))))))
    (is (eq :nmtokens (cl-xml:xml-dtd-att-def-type def)))))

;;; Enumerated and NOTATION types

(test dtd-attlist-enumeration-type
  "<!ATTLIST> with enumerated type is parsed as (:enumeration token+)."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE r [<!ATTLIST r size (small|medium|large) \"medium\">]><r />"))
         (def (first (cl-xml:xml-dtd-attlist-definitions (first alists)))))
    (is (equal '(:enumeration "small" "medium" "large")
               (cl-xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-notation-type
  "<!ATTLIST> with NOTATION type is parsed as (:notation name+)."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE r [<!ATTLIST r fmt NOTATION (gif|png) #IMPLIED>]><r />"))
         (def (first (cl-xml:xml-dtd-attlist-definitions (first alists)))))
    (is (equal '(:notation "gif" "png")
               (cl-xml:xml-dtd-att-def-type def)))))

;;; Default declarations

(test dtd-attlist-fixed-default
  "<!ATTLIST> with #FIXED AttValue is parsed as (:fixed value)."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r lang CDATA #FIXED \"en\">]><r />"))))))
    (is (equal '(:fixed "en") (cl-xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-bare-default
  "<!ATTLIST> with a bare AttValue is parsed as (:default value)."
  (let* ((def (first (cl-xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r lang CDATA \"en\">]><r />"))))))
    (is (equal '(:default "en") (cl-xml:xml-dtd-att-def-default def)))))

;;; Multiple attribute definitions in one ATTLIST

(test dtd-attlist-multiple-defs
  "Multiple AttDef entries in one <!ATTLIST> declaration are all captured."
  (let* ((al (first (parse-doctype-attlists
                     (concatenate 'string
                       "<!DOCTYPE r [<!ATTLIST r"
                       "  id ID #REQUIRED"
                       "  class CDATA #IMPLIED"
                       "  lang CDATA \"en\">]><r />"))))
         (defs (cl-xml:xml-dtd-attlist-definitions al)))
    (is (= 3 (length defs)))
    (is (string= "id"    (cl-xml:xml-dtd-att-def-name (first defs))))
    (is (string= "class" (cl-xml:xml-dtd-att-def-name (second defs))))
    (is (string= "lang"  (cl-xml:xml-dtd-att-def-name (third defs))))))

;;; Multiple ATTLIST declarations

(test dtd-multiple-attlists
  "Multiple <!ATTLIST> declarations are captured in order."
  (let* ((alists (parse-doctype-attlists
                  (concatenate 'string
                    "<!DOCTYPE r [<!ATTLIST r id ID #REQUIRED>"
                    "<!ATTLIST p class CDATA #IMPLIED>]><r />"))))
    (is (= 2 (length alists)))
    (is (string= "r" (cl-xml:xml-dtd-attlist-element-name (first alists))))
    (is (string= "p" (cl-xml:xml-dtd-attlist-element-name (second alists))))))

;;; ── DTD ENTITY parsing ────────────────────────────────────────────────────

(defun parse-doctype-entities (str)
  "Parse STR and return the list of xml-dtd-entity structs from the DOCTYPE."
  (cl-xml:xml-doctype-entities
   (cl-xml:xml-document-doctype (cl-xml:parse-xml str))))

;;; xml-dtd-entity struct

(test dtd-entity-struct
  "xml-dtd-entity struct has name, parameter-p, and definition fields."
  (let ((e (cl-xml:make-xml-dtd-entity :name "amp" :parameter-p nil
                                        :definition "&")))
    (is (cl-xml:xml-dtd-entity-p e))
    (is (string= "amp" (cl-xml:xml-dtd-entity-name e)))
    (is (null (cl-xml:xml-dtd-entity-parameter-p e)))
    (is (string= "&" (cl-xml:xml-dtd-entity-definition e)))))

;;; Internal general entity

(test dtd-entity-internal-general
  "<!ENTITY name 'value'> is parsed as an internal general entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY greeting \"Hello\">]><r />"))
         (e (first ents)))
    (is (= 1 (length ents)))
    (is (string= "greeting" (cl-xml:xml-dtd-entity-name e)))
    (is (null (cl-xml:xml-dtd-entity-parameter-p e)))
    (is (string= "Hello" (cl-xml:xml-dtd-entity-definition e)))))

;;; Internal parameter entity

(test dtd-entity-internal-parameter
  "<!ENTITY % name 'value'> is parsed as an internal parameter entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY % inline \"(#PCDATA)\">]><r />"))
         (e (first ents)))
    (is (= 1 (length ents)))
    (is (string= "inline" (cl-xml:xml-dtd-entity-name e)))
    (is (cl-xml:xml-dtd-entity-parameter-p e))
    (is (string= "(#PCDATA)" (cl-xml:xml-dtd-entity-definition e)))))

;;; External general entity — SYSTEM

(test dtd-entity-external-system
  "<!ENTITY name SYSTEM 'uri'> is parsed as an external entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY logo SYSTEM \"logo.png\">]><r />"))
         (e (first ents)))
    (is (equal '(:external nil "logo.png") (cl-xml:xml-dtd-entity-definition e)))))

;;; External general entity — PUBLIC

(test dtd-entity-external-public
  "<!ENTITY name PUBLIC 'pub' 'sys'> is parsed with both identifiers."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY iso PUBLIC \"-//ISO//EN\" \"iso.ent\">]><r />"))
         (e (first ents)))
    (is (equal '(:external "-//ISO//EN" "iso.ent")
               (cl-xml:xml-dtd-entity-definition e)))))

;;; Unparsed (NDATA) entity

(test dtd-entity-unparsed-ndata
  "<!ENTITY name SYSTEM 'uri' NDATA fmt> is parsed as an unparsed entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY logo SYSTEM \"logo.gif\" NDATA gif>]><r />"))
         (e (first ents)))
    (is (equal '(:unparsed nil "logo.gif" "gif")
               (cl-xml:xml-dtd-entity-definition e)))))

;;; External parameter entity

(test dtd-entity-external-parameter
  "<!ENTITY % name SYSTEM 'uri'> is parsed as an external parameter entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY % common SYSTEM \"common.ent\">]><r />"))
         (e (first ents)))
    (is (cl-xml:xml-dtd-entity-parameter-p e))
    (is (equal '(:external nil "common.ent")
               (cl-xml:xml-dtd-entity-definition e)))))

;;; Multiple entities

(test dtd-multiple-entities
  "Multiple <!ENTITY> declarations are captured in order."
  (let* ((ents (parse-doctype-entities
                (concatenate 'string
                  "<!DOCTYPE r ["
                  "<!ENTITY a \"first\">"
                  "<!ENTITY b \"second\">"
                  "]><r />"))))
    (is (= 2 (length ents)))
    (is (string= "a" (cl-xml:xml-dtd-entity-name (first ents))))
    (is (string= "b" (cl-xml:xml-dtd-entity-name (second ents))))))

;;; ── DTD NOTATION parsing ──────────────────────────────────────────────────

(defun parse-doctype-notations (str)
  "Parse STR and return the list of xml-dtd-notation structs from the DOCTYPE."
  (cl-xml:xml-doctype-notations
   (cl-xml:xml-document-doctype (cl-xml:parse-xml str))))

;;; xml-dtd-notation struct

(test dtd-notation-struct
  "xml-dtd-notation struct has name, public-id, and system-id fields."
  (let ((n (cl-xml:make-xml-dtd-notation :name "gif" :public-id nil
                                          :system-id "image/gif")))
    (is (cl-xml:xml-dtd-notation-p n))
    (is (string= "gif" (cl-xml:xml-dtd-notation-name n)))
    (is (null (cl-xml:xml-dtd-notation-public-id n)))
    (is (string= "image/gif" (cl-xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with SYSTEM identifier

(test dtd-notation-system
  "<!NOTATION name SYSTEM 'uri'> records system-id and nil public-id."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION gif SYSTEM \"image/gif\">]><r />"))
         (n (first notations)))
    (is (= 1 (length notations)))
    (is (string= "gif"       (cl-xml:xml-dtd-notation-name n)))
    (is (null               (cl-xml:xml-dtd-notation-public-id n)))
    (is (string= "image/gif" (cl-xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with PUBLIC + SYSTEM identifiers (ExternalID)

(test dtd-notation-public-and-system
  "<!NOTATION name PUBLIC 'pub' 'sys'> records both identifiers."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION jpeg PUBLIC \"-//JPEG//\" \"image/jpeg\">]><r />"))
         (n (first notations)))
    (is (string= "-//JPEG//"   (cl-xml:xml-dtd-notation-public-id n)))
    (is (string= "image/jpeg"  (cl-xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with PUBLIC only (PublicID — no system literal)

(test dtd-notation-public-only
  "<!NOTATION name PUBLIC 'pub'> with no system literal records nil system-id."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION pdf PUBLIC \"-//Adobe//PDF\">]><r />"))
         (n (first notations)))
    (is (string= "-//Adobe//PDF" (cl-xml:xml-dtd-notation-public-id n)))
    (is (null (cl-xml:xml-dtd-notation-system-id n)))))

;;; Multiple notations

(test dtd-multiple-notations
  "Multiple <!NOTATION> declarations are captured in order."
  (let* ((notations (parse-doctype-notations
                     (concatenate 'string
                       "<!DOCTYPE r ["
                       "<!NOTATION gif SYSTEM \"image/gif\">"
                       "<!NOTATION png SYSTEM \"image/png\">"
                       "]><r />"))))
    (is (= 2 (length notations)))
    (is (string= "gif" (cl-xml:xml-dtd-notation-name (first notations))))
    (is (string= "png" (cl-xml:xml-dtd-notation-name (second notations))))))

;;; ── All DTD declaration types together ────────────────────────────────────

(test dtd-all-declaration-types
  "DOCTYPE internal subset with ELEMENT, ATTLIST, ENTITY, and NOTATION all parsed."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ELEMENT root (title, body)>"
                  "<!ATTLIST root id ID #REQUIRED>"
                  "<!ENTITY copyright \"(c) 2024\">"
                  "<!NOTATION svg SYSTEM \"image/svg+xml\">"
                  "]><root/>")))))
    (is (= 1 (length (cl-xml:xml-doctype-elements dtd))))
    (is (= 1 (length (cl-xml:xml-doctype-attlists dtd))))
    (is (= 1 (length (cl-xml:xml-doctype-entities dtd))))
    (is (= 1 (length (cl-xml:xml-doctype-notations dtd))))
    (is (string= "root"
                 (cl-xml:xml-dtd-element-name
                  (first (cl-xml:xml-doctype-elements dtd)))))
    (is (string= "root"
                 (cl-xml:xml-dtd-attlist-element-name
                  (first (cl-xml:xml-doctype-attlists dtd)))))
    (is (string= "copyright"
                 (cl-xml:xml-dtd-entity-name
                  (first (cl-xml:xml-doctype-entities dtd)))))
    (is (string= "svg"
                 (cl-xml:xml-dtd-notation-name
                  (first (cl-xml:xml-doctype-notations dtd)))))))

;;; PE references between declarations are tolerated

(test dtd-pe-reference-between-decls
  "Parameter entity references between markup declarations do not cause errors."
  (let* ((dtd (cl-xml:xml-document-doctype
               (cl-xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ENTITY % ignored \"whatever\">"
                  "%ignored;"
                  "<!ELEMENT root EMPTY>"
                  "]><root />")))))
    ;; The PE reference is skipped; ELEMENT is still parsed
    (is (= 1 (length (cl-xml:xml-doctype-elements dtd))))))

;;; ── XSD: load-xsd — schema parsing ──────────────────────────────────────

(defparameter +simple-xsd+
  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\" type=\"xs:string\"/>
</xs:schema>"
  "A minimal XSD with a single xs:string element declaration.")

(test load-xsd-returns-schema
  "load-xsd returns an xsd-schema struct."
  (let ((schema (cl-xml:load-xsd +simple-xsd+)))
    (is (cl-xml:xsd-schema-p schema))))

(test load-xsd-parses-target-namespace
  "load-xsd captures the targetNamespace attribute."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"
           targetNamespace=\"http://example.com/ns\">
  <xs:element name=\"root\" type=\"xs:string\"/>
</xs:schema>")))
    (is (string= "http://example.com/ns"
                 (cl-xml:xsd-schema-target-namespace schema)))))

(test load-xsd-parses-top-level-element
  "load-xsd populates the elements alist with top-level xs:element declarations."
  (let* ((schema (cl-xml:load-xsd +simple-xsd+))
         (elem   (cdr (assoc "root" (cl-xml:xsd-schema-elements schema)
                             :test #'string=))))
    (is (not (null elem)))
    (is (cl-xml:xsd-element-p elem))
    (is (string= "root" (cl-xml:xsd-element-name elem)))
    (is (eq :string (cl-xml:xsd-element-type elem)))
    (is (= 1 (cl-xml:xsd-element-min-occurs elem)))
    (is (= 1 (cl-xml:xsd-element-max-occurs elem)))))

(test load-xsd-parses-named-complex-type
  "load-xsd places a named xs:complexType in the types alist."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"person\" type=\"PersonType\"/>
  <xs:complexType name=\"PersonType\">
    <xs:sequence>
      <xs:element name=\"name\" type=\"xs:string\"/>
      <xs:element name=\"age\"  type=\"xs:integer\"/>
    </xs:sequence>
    <xs:attribute name=\"id\" type=\"xs:integer\" use=\"required\"/>
  </xs:complexType>
</xs:schema>"))
         (ctype (cdr (assoc "PersonType" (cl-xml:xsd-schema-types schema)
                            :test #'string=))))
    (is (cl-xml:xsd-complex-type-p ctype))
    (is (string= "PersonType" (cl-xml:xsd-complex-type-name ctype)))
    (is (eq :sequence (cl-xml:xsd-complex-type-compositor ctype)))
    (is (= 2 (length (cl-xml:xsd-complex-type-elements ctype))))
    (is (= 1 (length (cl-xml:xsd-complex-type-attributes ctype))))
    (is (eq :required
            (cl-xml:xsd-attribute-use
             (first (cl-xml:xsd-complex-type-attributes ctype)))))))

(test load-xsd-parses-named-simple-type
  "load-xsd places a named xs:simpleType with restriction in the types alist."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:simpleType name=\"SizeType\">
    <xs:restriction base=\"xs:string\">
      <xs:enumeration value=\"small\"/>
      <xs:enumeration value=\"medium\"/>
      <xs:enumeration value=\"large\"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:element name=\"size\" type=\"SizeType\"/>
</xs:schema>"))
         (stype (cdr (assoc "SizeType" (cl-xml:xsd-schema-types schema)
                            :test #'string=))))
    (is (cl-xml:xsd-simple-type-p stype))
    (is (string= "SizeType" (cl-xml:xsd-simple-type-name stype)))
    (is (eq :string (cl-xml:xsd-simple-type-base stype)))
    (is (equal '("small" "medium" "large")
               (getf (cl-xml:xsd-simple-type-facets stype) :enumeration)))))

(test load-xsd-parses-occurs
  "load-xsd parses minOccurs/maxOccurs including 'unbounded'."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"list\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"item\" type=\"xs:string\"
                    minOccurs=\"0\" maxOccurs=\"unbounded\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (list-elem (cdr (assoc "list" (cl-xml:xsd-schema-elements schema)
                                :test #'string=)))
         (ctype     (cl-xml:xsd-element-type list-elem))
         (item-decl (first (cl-xml:xsd-complex-type-elements ctype))))
    (is (= 0 (cl-xml:xsd-element-min-occurs item-decl)))
    (is (eq :unbounded (cl-xml:xsd-element-max-occurs item-decl)))))

(test load-xsd-inline-complex-type
  "load-xsd handles inline anonymous xs:complexType inside xs:element."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"child\" type=\"xs:integer\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (root-elem (cdr (assoc "root" (cl-xml:xsd-schema-elements schema)
                                :test #'string=)))
         (ctype     (cl-xml:xsd-element-type root-elem)))
    (is (cl-xml:xsd-complex-type-p ctype))
    (is (null (cl-xml:xsd-complex-type-name ctype)))  ; anonymous
    (is (= 1 (length (cl-xml:xsd-complex-type-elements ctype))))))

(test load-xsd-no-prefix
  "load-xsd accepts a schema document without a namespace prefix."
  (let ((schema (cl-xml:load-xsd "<schema><element name=\"x\" type=\"string\"/></schema>")))
    (is (cl-xml:xsd-schema-p schema))
    (is (= 1 (length (cl-xml:xsd-schema-elements schema))))))

(test load-xsd-wrong-root-signals-error
  "load-xsd signals an error when the root element is not xs:schema."
  (signals error (cl-xml:load-xsd "<root/>")))

;;; ── XSD: validate-xml — valid documents ──────────────────────────────────

(test validate-simple-string-element
  "validate-xml accepts a document whose root element holds a string value."
  (let ((schema (cl-xml:load-xsd +simple-xsd+))
        (doc    (cl-xml:parse-xml "<root>hello</root>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-complex-type-with-sequence
  "validate-xml accepts a document matching a complex type with xs:sequence."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"person\" type=\"PersonType\"/>
  <xs:complexType name=\"PersonType\">
    <xs:sequence>
      <xs:element name=\"name\" type=\"xs:string\"/>
      <xs:element name=\"age\"  type=\"xs:integer\"/>
    </xs:sequence>
    <xs:attribute name=\"id\" type=\"xs:integer\" use=\"required\"/>
  </xs:complexType>
</xs:schema>"))
         (doc (cl-xml:parse-xml
               "<person id=\"1\"><name>Alice</name><age>30</age></person>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-optional-elements-absent
  "validate-xml accepts a document where optional children are absent."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"opt\" type=\"xs:string\" minOccurs=\"0\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<root/>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-unbounded-element
  "validate-xml accepts multiple occurrences of an unbounded element."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"list\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"item\" type=\"xs:string\"
                    minOccurs=\"0\" maxOccurs=\"unbounded\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml
               "<list><item>a</item><item>b</item><item>c</item></list>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-xs-all-compositor
  "validate-xml accepts xs:all children in any order."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:all>
        <xs:element name=\"a\" type=\"xs:string\"/>
        <xs:element name=\"b\" type=\"xs:string\"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc-ab (cl-xml:parse-xml "<root><a>x</a><b>y</b></root>"))
         (doc-ba (cl-xml:parse-xml "<root><b>y</b><a>x</a></root>")))
    (is (cl-xml:validate-xml doc-ab schema))
    (is (cl-xml:validate-xml doc-ba schema))))

(test validate-xs-choice-compositor
  "validate-xml accepts either branch of an xs:choice."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:choice>
        <xs:element name=\"a\" type=\"xs:string\"/>
        <xs:element name=\"b\" type=\"xs:integer\"/>
      </xs:choice>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc-a (cl-xml:parse-xml "<root><a>hello</a></root>"))
         (doc-b (cl-xml:parse-xml "<root><b>42</b></root>")))
    (is (cl-xml:validate-xml doc-a schema))
    (is (cl-xml:validate-xml doc-b schema))))

(test validate-enumeration-restriction
  "validate-xml accepts a value that is in an xs:enumeration list."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"size\">
    <xs:simpleType>
      <xs:restriction base=\"xs:string\">
        <xs:enumeration value=\"small\"/>
        <xs:enumeration value=\"medium\"/>
        <xs:enumeration value=\"large\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<size>medium</size>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-length-facet
  "validate-xml accepts a string whose length satisfies xs:minLength/xs:maxLength."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"code\">
    <xs:simpleType>
      <xs:restriction base=\"xs:string\">
        <xs:minLength value=\"2\"/>
        <xs:maxLength value=\"5\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<code>abc</code>")))
    (is (cl-xml:validate-xml doc schema))))

(test validate-min-inclusive-facet
  "validate-xml accepts a numeric value satisfying xs:minInclusive."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"score\">
    <xs:simpleType>
      <xs:restriction base=\"xs:integer\">
        <xs:minInclusive value=\"0\"/>
        <xs:maxInclusive value=\"100\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<score>75</score>")))
    (is (cl-xml:validate-xml doc schema))))

;;; ── XSD: validate-xml — invalid documents (error signaling) ─────────────

(test validate-wrong-root-element
  "validate-xml signals xsd-validation-error when the root element is undeclared."
  (let ((schema (cl-xml:load-xsd +simple-xsd+))
        (doc    (cl-xml:parse-xml "<wrong>hello</wrong>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-wrong-type-integer
  "validate-xml signals xsd-validation-error for a non-integer value on xs:integer."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"count\" type=\"xs:integer\"/>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<count>not-a-number</count>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-wrong-type-boolean
  "validate-xml signals xsd-validation-error for an invalid xs:boolean value."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"flag\" type=\"xs:boolean\"/>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<flag>yes</flag>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-wrong-type-date
  "validate-xml signals xsd-validation-error for an invalid xs:date value."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"dob\" type=\"xs:date\"/>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<dob>01/01/2000</dob>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-missing-required-child
  "validate-xml signals xsd-validation-error when a required xs:sequence child is absent."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"person\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"name\" type=\"xs:string\"/>
        <xs:element name=\"age\"  type=\"xs:integer\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<person><name>Alice</name></person>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-missing-required-attribute
  "validate-xml signals xsd-validation-error when a required attribute is absent."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"item\">
    <xs:complexType>
      <xs:attribute name=\"id\" type=\"xs:integer\" use=\"required\"/>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<item/>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-prohibited-attribute-present
  "validate-xml signals xsd-validation-error when a prohibited attribute appears."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"item\">
    <xs:complexType>
      <xs:attribute name=\"banned\" type=\"xs:string\" use=\"prohibited\"/>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<item banned=\"oops\"/>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-unexpected-element-in-sequence
  "validate-xml signals xsd-validation-error when an unexpected element appears."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"known\" type=\"xs:string\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<root><known>ok</known><unknown/></root>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-enumeration-violation
  "validate-xml signals xsd-validation-error when a value is not in the enumeration."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"color\">
    <xs:simpleType>
      <xs:restriction base=\"xs:string\">
        <xs:enumeration value=\"red\"/>
        <xs:enumeration value=\"green\"/>
        <xs:enumeration value=\"blue\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<color>purple</color>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-max-length-violation
  "validate-xml signals xsd-validation-error when a string exceeds xs:maxLength."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"pin\">
    <xs:simpleType>
      <xs:restriction base=\"xs:string\">
        <xs:maxLength value=\"4\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<pin>12345</pin>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

(test validate-min-inclusive-violation
  "validate-xml signals xsd-validation-error when xs:minInclusive is violated."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"score\">
    <xs:simpleType>
      <xs:restriction base=\"xs:integer\">
        <xs:minInclusive value=\"0\"/>
        <xs:maxInclusive value=\"100\"/>
      </xs:restriction>
    </xs:simpleType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<score>-5</score>")))
    (signals cl-xml:xsd-validation-error (cl-xml:validate-xml doc schema))))

;;; ── XSD: xsd-validation-error condition ─────────────────────────────────

(test xsd-validation-error-has-message
  "xsd-validation-error condition carries a message string."
  (handler-case
      (cl-xml:validate-xml (cl-xml:parse-xml "<wrong/>")
                           (cl-xml:load-xsd +simple-xsd+))
    (cl-xml:xsd-validation-error (e)
      (is (stringp (cl-xml:xsd-validation-error-message e))))))

(test xsd-validation-error-path-is-set-for-nested-failure
  "xsd-validation-error carries a non-nil path for a failure inside a nested element."
  (let* ((schema (cl-xml:load-xsd
                  "<?xml version=\"1.0\"?>
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
  <xs:element name=\"root\">
    <xs:complexType>
      <xs:sequence>
        <xs:element name=\"count\" type=\"xs:integer\"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"))
         (doc (cl-xml:parse-xml "<root><count>bad</count></root>")))
    (handler-case (cl-xml:validate-xml doc schema)
      (cl-xml:xsd-validation-error (e)
        (is (not (null (cl-xml:xsd-validation-error-path e))))))))

;;; ── XSD: built-in type validators ────────────────────────────────────────

(test builtin-integer-valid
  "xs:integer accepts digit strings and values with a leading sign."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"n\" type=\"xs:integer\"/></xs:schema>")))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<n>0</n>") schema))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<n>-42</n>") schema))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<n>+999</n>") schema))))

(test builtin-boolean-valid-values
  "xs:boolean accepts true, false, 1, and 0."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"b\" type=\"xs:boolean\"/></xs:schema>")))
    (dolist (v '("true" "false" "1" "0"))
      (is (cl-xml:validate-xml
           (cl-xml:parse-xml (format nil "<b>~a</b>" v)) schema)))))

(test builtin-decimal-valid
  "xs:decimal accepts decimal number strings."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"d\" type=\"xs:decimal\"/></xs:schema>")))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<d>3.14</d>") schema))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<d>-0.5</d>") schema))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<d>42</d>") schema))))

(test builtin-date-valid
  "xs:date accepts YYYY-MM-DD formatted strings."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"dt\" type=\"xs:date\"/></xs:schema>")))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<dt>2024-01-15</dt>") schema))))

(test builtin-positive-integer-rejects-zero
  "xs:positiveInteger rejects zero."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"n\" type=\"xs:positiveInteger\"/></xs:schema>")))
    (signals cl-xml:xsd-validation-error
      (cl-xml:validate-xml (cl-xml:parse-xml "<n>0</n>") schema))))

(test builtin-byte-range
  "xs:byte rejects a value outside [-128, 127]."
  (let ((schema (cl-xml:load-xsd
                 "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
<xs:element name=\"n\" type=\"xs:byte\"/></xs:schema>")))
    (is (cl-xml:validate-xml (cl-xml:parse-xml "<n>127</n>") schema))
    (signals cl-xml:xsd-validation-error
      (cl-xml:validate-xml (cl-xml:parse-xml "<n>128</n>") schema))))

;;; ── SOAP ─────────────────────────────────────────────────────────────────

;;; Namespace URI constants

(test soap-namespace-constants
  "SOAP namespace URI constants have the correct values."
  (is (string= "http://schemas.xmlsoap.org/soap/envelope/"
               cl-xml:+soap-1.1-namespace+))
  (is (string= "http://www.w3.org/2003/05/soap-envelope"
               cl-xml:+soap-1.2-namespace+)))

;;; Structure construction

(test soap-envelope-struct
  "soap-envelope struct can be constructed and its fields read back."
  (let* ((body (cl-xml:make-soap-body :payload '()))
         (env  (cl-xml:make-soap-envelope :version :1.1 :body body)))
    (is (cl-xml:soap-envelope-p env))
    (is (eq :1.1 (cl-xml:soap-envelope-version env)))
    (is (null (cl-xml:soap-envelope-header env)))
    (is (cl-xml:soap-body-p (cl-xml:soap-envelope-body env)))))

(test soap-header-struct
  "soap-header struct stores entries correctly."
  (let ((hdr (cl-xml:make-soap-header :entries '())))
    (is (cl-xml:soap-header-p hdr))
    (is (null (cl-xml:soap-header-entries hdr)))))

(test soap-body-struct
  "soap-body struct stores payload correctly."
  (let ((body (cl-xml:make-soap-body :payload '())))
    (is (cl-xml:soap-body-p body))
    (is (null (cl-xml:soap-body-fault body)))
    (is (null (cl-xml:soap-body-payload body)))))

(test soap-fault-struct
  "soap-fault struct stores all fields correctly."
  (let ((f (cl-xml:make-soap-fault
            :code   "soap:Client"
            :string "Bad request"
            :actor  "http://example.com/"
            :detail nil)))
    (is (cl-xml:soap-fault-p f))
    (is (string= "soap:Client"         (cl-xml:soap-fault-code f)))
    (is (string= "Bad request"         (cl-xml:soap-fault-string f)))
    (is (string= "http://example.com/" (cl-xml:soap-fault-actor f)))
    (is (null (cl-xml:soap-fault-detail f)))))

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
  (let ((env (cl-xml:parse-soap +soap-1.1-minimal+)))
    (is (cl-xml:soap-envelope-p env))
    (is (eq :1.1 (cl-xml:soap-envelope-version env)))
    (is (null (cl-xml:soap-envelope-header env)))
    (is (cl-xml:soap-body-p (cl-xml:soap-envelope-body env)))
    (is (null (cl-xml:soap-body-fault (cl-xml:soap-envelope-body env))))
    (is (= 1 (length (cl-xml:soap-body-payload
                      (cl-xml:soap-envelope-body env)))))))

(test parse-soap-1.1-body-element-name
  "The body payload element is the GetPrice element."
  (let* ((env  (cl-xml:parse-soap +soap-1.1-minimal+))
         (body (cl-xml:soap-envelope-body env))
         (elem (first (cl-xml:soap-body-payload body)))
         (tag  (cl-xml:xml-node-tag elem)))
    (is (cl-xml:xml-qname-p tag))
    (is (string= "GetPrice" (cl-xml:xml-qname-local-name tag)))))

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
  (let ((env (cl-xml:parse-soap +soap-1.2-minimal+)))
    (is (cl-xml:soap-envelope-p env))
    (is (eq :1.2 (cl-xml:soap-envelope-version env)))
    (is (null (cl-xml:soap-envelope-header env)))
    (is (= 1 (length (cl-xml:soap-body-payload
                      (cl-xml:soap-envelope-body env)))))))

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
  (let* ((env (cl-xml:parse-soap +soap-1.1-with-header+))
         (hdr (cl-xml:soap-envelope-header env)))
    (is (cl-xml:soap-header-p hdr))
    (is (= 1 (length (cl-xml:soap-header-entries hdr))))
    (let* ((entry (first (cl-xml:soap-header-entries hdr)))
           (tag   (cl-xml:xml-node-tag entry)))
      (is (cl-xml:xml-qname-p tag))
      (is (string= "Auth" (cl-xml:xml-qname-local-name tag))))))

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
  (let* ((env   (cl-xml:parse-soap +soap-1.1-fault+))
         (body  (cl-xml:soap-envelope-body env))
         (fault (cl-xml:soap-body-fault body)))
    (is (cl-xml:soap-fault-p fault))
    (is (null (cl-xml:soap-body-payload body)))))

(test parse-soap-1.1-fault-code
  "SOAP 1.1 fault code is extracted from faultcode text."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.1-fault+)))))
    (is (string= "soap:Client" (cl-xml:soap-fault-code fault)))))

(test parse-soap-1.1-fault-string
  "SOAP 1.1 fault string is extracted from faultstring text."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.1-fault+)))))
    (is (string= "Invalid input" (cl-xml:soap-fault-string fault)))))

(test parse-soap-1.1-fault-actor
  "SOAP 1.1 fault actor is extracted from faultactor text."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.1-fault+)))))
    (is (string= "http://example.com/service" (cl-xml:soap-fault-actor fault)))))

(test parse-soap-1.1-fault-detail-present
  "SOAP 1.1 fault detail is the detail xml-node."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.1-fault+)))))
    (is (cl-xml:xml-node-p (cl-xml:soap-fault-detail fault)))
    (is (string= "detail"
                 (let ((tag (cl-xml:xml-node-tag (cl-xml:soap-fault-detail fault))))
                   (if (cl-xml:xml-qname-p tag)
                       (cl-xml:xml-qname-local-name tag)
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
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.2-fault+)))))
    (is (cl-xml:soap-fault-p fault))
    (is (string= "env:Sender" (cl-xml:soap-fault-code fault)))))

(test parse-soap-1.2-fault-string
  "SOAP 1.2 fault string is extracted from Reason/Text text."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.2-fault+)))))
    (is (string= "Bad request" (cl-xml:soap-fault-string fault)))))

(test parse-soap-1.2-fault-actor
  "SOAP 1.2 Role is mapped to soap-fault-actor."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.2-fault+)))))
    (is (string= "http://example.com/node" (cl-xml:soap-fault-actor fault)))))

(test parse-soap-1.2-fault-detail-present
  "SOAP 1.2 fault Detail is the Detail xml-node."
  (let* ((fault (cl-xml:soap-body-fault
                 (cl-xml:soap-envelope-body
                  (cl-xml:parse-soap +soap-1.2-fault+)))))
    (is (cl-xml:xml-node-p (cl-xml:soap-fault-detail fault)))
    (is (string= "Detail"
                 (let ((tag (cl-xml:xml-node-tag (cl-xml:soap-fault-detail fault))))
                   (if (cl-xml:xml-qname-p tag)
                       (cl-xml:xml-qname-local-name tag)
                       tag))))))

;;; parse-soap — error cases

(test parse-soap-not-envelope-error
  "parse-soap signals soap-error when root element is not Envelope."
  (signals cl-xml:soap-error
    (cl-xml:parse-soap
     "<root xmlns=\"http://schemas.xmlsoap.org/soap/envelope/\" />")))

(test parse-soap-unknown-namespace-error
  "parse-soap signals soap-error for an unknown SOAP namespace URI."
  (signals cl-xml:soap-error
    (cl-xml:parse-soap
     "<s:Envelope xmlns:s=\"http://example.com/soap\"><s:Body /></s:Envelope>")))

(test parse-soap-no-body-error
  "parse-soap signals soap-error when the Envelope has no Body."
  (signals cl-xml:soap-error
    (cl-xml:parse-soap
     "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" />")))

;;; serialize-soap

(defun parse-soap-from-string (str)
  "Round-trip helper: parse STR as a SOAP envelope."
  (cl-xml:parse-soap str))

(test serialize-soap-returns-string
  "serialize-soap returns a non-empty string by default."
  (let* ((body (cl-xml:make-soap-body :payload '()))
         (env  (cl-xml:make-soap-envelope :version :1.1 :body body))
         (xml  (cl-xml:serialize-soap env)))
    (is (stringp xml))
    (is (plusp (length xml)))))

(test serialize-soap-contains-envelope-tag
  "The serialized string contains soap:Envelope."
  (let* ((body (cl-xml:make-soap-body :payload '()))
         (env  (cl-xml:make-soap-envelope :version :1.1 :body body))
         (xml  (cl-xml:serialize-soap env)))
    (is (search "Envelope" xml))))

(test serialize-soap-1.1-namespace-present
  "Serialized SOAP 1.1 output contains the SOAP 1.1 namespace URI."
  (let* ((body (cl-xml:make-soap-body :payload '()))
         (env  (cl-xml:make-soap-envelope :version :1.1 :body body))
         (xml  (cl-xml:serialize-soap env)))
    (is (search "schemas.xmlsoap.org/soap/envelope/" xml))))

(test serialize-soap-1.2-namespace-present
  "Serialized SOAP 1.2 output contains the SOAP 1.2 namespace URI."
  (let* ((body (cl-xml:make-soap-body :payload '()))
         (env  (cl-xml:make-soap-envelope :version :1.2 :body body))
         (xml  (cl-xml:serialize-soap env)))
    (is (search "w3.org/2003/05/soap-envelope" xml))))

(test serialize-soap-to-stream
  "serialize-soap writes to a supplied stream and returns nil."
  (let* ((body   (cl-xml:make-soap-body :payload '()))
         (env    (cl-xml:make-soap-envelope :version :1.1 :body body))
         result)
    (with-output-to-string (s)
      (setf result (cl-xml:serialize-soap env :stream s)))
    (is (null result))))

(test serialize-soap-roundtrip-1.1
  "Serializing and re-parsing a SOAP 1.1 envelope preserves version and body."
  (let* ((env (cl-xml:parse-soap +soap-1.1-minimal+))
         (xml (cl-xml:serialize-soap env))
         (env2 (cl-xml:parse-soap xml)))
    (is (eq :1.1 (cl-xml:soap-envelope-version env2)))
    (is (= 1 (length (cl-xml:soap-body-payload
                      (cl-xml:soap-envelope-body env2)))))))

(test serialize-soap-roundtrip-1.2
  "Serializing and re-parsing a SOAP 1.2 envelope preserves version."
  (let* ((env  (cl-xml:parse-soap +soap-1.2-minimal+))
         (xml  (cl-xml:serialize-soap env))
         (env2 (cl-xml:parse-soap xml)))
    (is (eq :1.2 (cl-xml:soap-envelope-version env2)))))

(test serialize-soap-roundtrip-header
  "Serializing and re-parsing preserves the header block count."
  (let* ((env  (cl-xml:parse-soap +soap-1.1-with-header+))
         (xml  (cl-xml:serialize-soap env))
         (env2 (cl-xml:parse-soap xml))
         (hdr  (cl-xml:soap-envelope-header env2)))
    (is (cl-xml:soap-header-p hdr))
    (is (= 1 (length (cl-xml:soap-header-entries hdr))))))

(test serialize-soap-1.1-fault-roundtrip
  "Serializing and re-parsing a SOAP 1.1 fault preserves code and string."
  (let* ((fault-in (cl-xml:make-soap-fault
                    :code "soap:Server" :string "Internal error"))
         (body     (cl-xml:make-soap-body :fault fault-in))
         (env      (cl-xml:make-soap-envelope :version :1.1 :body body))
         (xml      (cl-xml:serialize-soap env))
         (env2     (cl-xml:parse-soap xml))
         (fault    (cl-xml:soap-body-fault (cl-xml:soap-envelope-body env2))))
    (is (cl-xml:soap-fault-p fault))
    (is (string= "soap:Server"    (cl-xml:soap-fault-code fault)))
    (is (string= "Internal error" (cl-xml:soap-fault-string fault)))))

(test serialize-soap-1.2-fault-roundtrip
  "Serializing and re-parsing a SOAP 1.2 fault preserves code and string."
  (let* ((fault-in (cl-xml:make-soap-fault
                    :code "env:Receiver" :string "Processing failed"))
         (body     (cl-xml:make-soap-body :fault fault-in))
         (env      (cl-xml:make-soap-envelope :version :1.2 :body body))
         (xml      (cl-xml:serialize-soap env))
         (env2     (cl-xml:parse-soap xml))
         (fault    (cl-xml:soap-body-fault (cl-xml:soap-envelope-body env2))))
    (is (cl-xml:soap-fault-p fault))
    (is (string= "env:Receiver"     (cl-xml:soap-fault-code fault)))
    (is (string= "Processing failed" (cl-xml:soap-fault-string fault)))))

;;; soap-make-envelope

(test soap-make-envelope-basic
  "soap-make-envelope wraps body XML in a SOAP 1.1 envelope."
  (let* ((env (cl-xml:soap-make-envelope
               "<m:GetUser xmlns:m=\"http://example.com/\"><m:id>1</m:id></m:GetUser>"))
         (body (cl-xml:soap-envelope-body env)))
    (is (cl-xml:soap-envelope-p env))
    (is (eq :1.1 (cl-xml:soap-envelope-version env)))
    (is (null (cl-xml:soap-envelope-header env)))
    (is (= 1 (length (cl-xml:soap-body-payload body))))
    (is (string= "GetUser"
                 (cl-xml:xml-qname-local-name
                  (cl-xml:xml-node-tag
                   (first (cl-xml:soap-body-payload body))))))))

(test soap-make-envelope-version-1.2
  "soap-make-envelope respects the :version :1.2 keyword argument."
  (let ((env (cl-xml:soap-make-envelope
              "<ping />" :version :1.2)))
    (is (eq :1.2 (cl-xml:soap-envelope-version env)))))

(test soap-make-envelope-with-header
  "soap-make-envelope with :header-xml adds header entries."
  (let* ((env (cl-xml:soap-make-envelope
               "<ping />"
               :header-xml "<auth><token>xyz</token></auth>"))
         (hdr (cl-xml:soap-envelope-header env)))
    (is (cl-xml:soap-header-p hdr))
    (is (= 1 (length (cl-xml:soap-header-entries hdr))))
    (let ((tag (cl-xml:xml-node-tag (first (cl-xml:soap-header-entries hdr)))))
      (is (string= "auth"
                   (if (cl-xml:xml-qname-p tag)
                       (cl-xml:xml-qname-local-name tag)
                       tag))))))

(test soap-make-envelope-serialize-roundtrip
  "An envelope built with soap-make-envelope survives a serialize/parse round-trip."
  (let* ((env  (cl-xml:soap-make-envelope
                "<m:Op xmlns:m=\"http://example.com/\" />" :version :1.1))
         (xml  (cl-xml:serialize-soap env))
         (env2 (cl-xml:parse-soap xml)))
    (is (eq :1.1 (cl-xml:soap-envelope-version env2)))
    (is (= 1 (length (cl-xml:soap-body-payload
                      (cl-xml:soap-envelope-body env2)))))))

(test soap-error-condition
  "soap-error condition reports message correctly."
  (let ((err (make-condition 'cl-xml:soap-error :message "test error")))
    (is (string= "test error" (cl-xml:soap-error-message err)))
    (is (null (cl-xml:soap-error-path err)))
    (is (search "test error" (format nil "~a" err)))))


