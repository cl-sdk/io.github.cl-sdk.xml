(in-package #:io.github.cl-sdk.xml.test)

(def-suite cl-xml-suite
  :description "Test suite for cl-xml.")

(in-suite cl-xml-suite)

;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun parse-root (str)
  "Parse STR and return the root xml-node."
  (io.github.cl-sdk.xml:xml-document-root (io.github.cl-sdk.xml:parse-xml str)))

;;; ── Original regression tests ────────────────────────────────────────────

(test self-closing-node
  "A self-closing element is parsed into an xml-node with no children."
  (let ((node (parse-root "<tag />")))
    (is (string= "tag" (io.github.cl-sdk.xml:xml-node-tag node)))
    (is (null (io.github.cl-sdk.xml:xml-node-attributes node)))
    (is (null (io.github.cl-sdk.xml:xml-node-children node)))))

(test self-closing-node-with-attribute
  "A self-closing element with one attribute."
  (let ((node (parse-root "<img src=\"logo.png\" />")))
    (is (string= "img" (io.github.cl-sdk.xml:xml-node-tag node)))
    (is (equal '(("src" . "logo.png")) (io.github.cl-sdk.xml:xml-node-attributes node)))
    (is (null (io.github.cl-sdk.xml:xml-node-children node)))))

(test empty-element
  "An element with an explicit closing tag but no children."
  (let ((node (parse-root "<div></div>")))
    (is (string= "div" (io.github.cl-sdk.xml:xml-node-tag node)))
    (is (null (io.github.cl-sdk.xml:xml-node-attributes node)))
    (is (null (io.github.cl-sdk.xml:xml-node-children node)))))

(test element-with-attribute
  "An element with one attribute and an explicit closing tag."
  (let ((node (parse-root "<a href=\"https://example.com\"></a>")))
    (is (string= "a" (io.github.cl-sdk.xml:xml-node-tag node)))
    (is (equal '(("href" . "https://example.com"))
               (io.github.cl-sdk.xml:xml-node-attributes node)))
    (is (null (io.github.cl-sdk.xml:xml-node-children node)))))

(test element-with-children
  "An element whose children are parsed recursively."
  (let ((root (parse-root "<root><child /></root>")))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag root)))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-node-children root))))
    (let ((child (first (io.github.cl-sdk.xml:xml-node-children root))))
      (is (string= "child" (io.github.cl-sdk.xml:xml-node-tag child)))
      (is (null (io.github.cl-sdk.xml:xml-node-children child))))))

(test problem-statement-example
  "Parse the full example from the problem statement."
  (let* ((xml "
<root>
    <node-without-children attribute-name=\"attribute-value\" />
    <node-with-children attribute-name=\"attribute-value\">
    </node-with-children>
</root>")
         (root (parse-root xml)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag root)))
    (is (null (io.github.cl-sdk.xml:xml-node-attributes root)))
    (is (= 2 (length (io.github.cl-sdk.xml:xml-node-children root))))
    (let ((self-closing (first (io.github.cl-sdk.xml:xml-node-children root)))
          (with-children (second (io.github.cl-sdk.xml:xml-node-children root))))
      ;; self-closing child
      (is (string= "node-without-children" (io.github.cl-sdk.xml:xml-node-tag self-closing)))
      (is (equal '(("attribute-name" . "attribute-value"))
                 (io.github.cl-sdk.xml:xml-node-attributes self-closing)))
      (is (null (io.github.cl-sdk.xml:xml-node-children self-closing)))
      ;; child with explicit closing tag
      (is (string= "node-with-children" (io.github.cl-sdk.xml:xml-node-tag with-children)))
      (is (equal '(("attribute-name" . "attribute-value"))
                 (io.github.cl-sdk.xml:xml-node-attributes with-children)))
      (is (null (io.github.cl-sdk.xml:xml-node-children with-children))))))

(test multiple-attributes
  "An element with multiple attributes preserves order."
  (let ((node (parse-root "<el a=\"1\" b=\"2\" c=\"3\" />")))
    (is (equal '(("a" . "1") ("b" . "2") ("c" . "3"))
               (io.github.cl-sdk.xml:xml-node-attributes node)))))

(test deeply-nested
  "Deeply nested elements are parsed correctly."
  (let* ((root (parse-root "<a><b><c /></b></a>"))
         (b (first (io.github.cl-sdk.xml:xml-node-children root)))
         (c (first (io.github.cl-sdk.xml:xml-node-children b))))
    (is (string= "a" (io.github.cl-sdk.xml:xml-node-tag root)))
    (is (string= "b" (io.github.cl-sdk.xml:xml-node-tag b)))
    (is (string= "c" (io.github.cl-sdk.xml:xml-node-tag c)))
    (is (null (io.github.cl-sdk.xml:xml-node-children c)))))

;;; ── xml-document structure ───────────────────────────────────────────────

(test parse-xml-returns-document
  "parse-xml returns an xml-document with prolog and root fields."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<root />")))
    (is (io.github.cl-sdk.xml:xml-document-p doc))
    (is (null (io.github.cl-sdk.xml:xml-document-prolog doc)))
    (is (io.github.cl-sdk.xml:xml-node-p (io.github.cl-sdk.xml:xml-document-root doc)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

;;; ── XML 1.0 conformance — Names (§2.3) ──────────────────────────────────

(test valid-name-with-underscore-and-hyphen
  "Names may contain underscores, hyphens, and dots after the first char."
  (let ((node (parse-root "<_my-tag.1 />")))
    (is (string= "_my-tag.1" (io.github.cl-sdk.xml:xml-node-tag node)))))

(test valid-name-with-colon
  "Colons are valid XML name characters."
  (let ((node (parse-root "<ns:tag />")))
    (is (string= "ns:tag" (io.github.cl-sdk.xml:xml-node-tag node)))))

(test invalid-name-start-digit
  "A name starting with a digit is a well-formedness error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<1tag />")))

(test invalid-name-start-hyphen
  "A name starting with a hyphen is a well-formedness error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<-tag />")))

;;; ── XML 1.0 conformance — Attributes (§3.1, §3.3.3) ─────────────────────

(test duplicate-attribute-error
  "Duplicate attribute names on the same element are a well-formedness error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<el a=\"1\" a=\"2\" />")))

(test lt-in-attribute-value-error
  "A literal '<' inside an attribute value is a well-formedness error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<el a=\"x<y\" />")))

;;; ── XML 1.0 conformance — Entity references (§4.6) ──────────────────────

(test entity-ref-amp-in-attribute
  "&amp; in an attribute value expands to '&'."
  (let ((node (parse-root "<el v=\"a&amp;b\" />")))
    (is (string= "a&b" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-lt-in-attribute
  "&lt; in an attribute value expands to '<'."
  (let ((node (parse-root "<el v=\"a&lt;b\" />")))
    (is (string= "a<b" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-gt-in-attribute
  "&gt; in an attribute value expands to '>'."
  (let ((node (parse-root "<el v=\"a&gt;b\" />")))
    (is (string= "a>b" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                   :test #'string=))))))

(test entity-ref-quot-in-attribute
  "&quot; in a single-quoted attribute value expands to '\"'."
  (let ((node (parse-root "<el v='a&quot;b' />")))
    (is (string= "a\"b" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                    :test #'string=))))))

(test entity-ref-apos-in-attribute
  "&apos; in a double-quoted attribute value expands to \"'\"."
  (let ((node (parse-root "<el v=\"a&apos;b\" />")))
    (is (string= "a'b" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                   :test #'string=))))))

(test unknown-entity-ref-error
  "An unknown named entity reference signals an error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<el v=\"&unknown;\" />")))

;;; ── XML 1.0 conformance — Character references (§4.1) ───────────────────

(test char-ref-decimal
  "A decimal character reference &#65; expands to 'A'."
  (let ((node (parse-root "<el v=\"&#65;\" />")))
    (is (string= "A" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                 :test #'string=))))))

(test char-ref-hex
  "A hexadecimal character reference &#x41; expands to 'A'."
  (let ((node (parse-root "<el v=\"&#x41;\" />")))
    (is (string= "A" (cdr (assoc "v" (io.github.cl-sdk.xml:xml-node-attributes node)
                                 :test #'string=))))))

;;; ── XML 1.0 conformance — Character data (§2.4) ─────────────────────────

(test text-content-preserved
  "Non-whitespace text content is preserved as a string child."
  (let* ((root (parse-root "<p>hello</p>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (stringp (first children)))
    (is (string= "hello" (first children)))))

(test text-with-entity-ref
  "Entity references in text content are expanded."
  (let* ((root (parse-root "<p>a&amp;b</p>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (string= "a&b" (first children)))))

(test whitespace-only-text-discarded
  "Whitespace-only text runs between elements do not become text children."
  (let ((root (parse-root "<root>
  <a />
  <b />
</root>")))
    (is (= 2 (length (io.github.cl-sdk.xml:xml-node-children root))))
    (is (every #'io.github.cl-sdk.xml:xml-node-p (io.github.cl-sdk.xml:xml-node-children root)))))

;;; ── XML 1.0 conformance — CDATA sections (§2.7) ─────────────────────────

(test cdata-section-preserved-as-node
  "A CDATA section is preserved as an xml-cdata child node."
  (let* ((root (parse-root "<el><![CDATA[a<b>&c]]></el>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (io.github.cl-sdk.xml:xml-cdata-p (first children)))
    (is (string= "a<b>&c" (io.github.cl-sdk.xml:xml-cdata-data (first children))))))

(test cdata-whitespace-only-preserved
  "A CDATA section containing only whitespace is still preserved as a node."
  (let* ((root (parse-root "<el><![CDATA[   ]]></el>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 1 (length children)))
    (is (io.github.cl-sdk.xml:xml-cdata-p (first children)))
    (is (string= "   " (io.github.cl-sdk.xml:xml-cdata-data (first children))))))

;;; ── XML 1.0 conformance — Comments (§2.5) ───────────────────────────────

(test comment-preserved-as-node
  "Comments inside an element are preserved as xml-comment child nodes."
  (let* ((root (parse-root "<root><!-- hello --><child /></root>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 2 (length children)))
    (is (io.github.cl-sdk.xml:xml-comment-p (first children)))
    (is (string= " hello " (io.github.cl-sdk.xml:xml-comment-data (first children))))
    (is (io.github.cl-sdk.xml:xml-node-p (second children)))
    (is (string= "child" (io.github.cl-sdk.xml:xml-node-tag (second children))))))

(test multiple-comments-in-element
  "Multiple comments between elements are each preserved as xml-comment nodes."
  (let* ((root (parse-root "<root><!-- a --><!-- b --><x /></root>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 3 (length children)))
    (is (io.github.cl-sdk.xml:xml-comment-p (first children)))
    (is (string= " a " (io.github.cl-sdk.xml:xml-comment-data (first children))))
    (is (io.github.cl-sdk.xml:xml-comment-p (second children)))
    (is (string= " b " (io.github.cl-sdk.xml:xml-comment-data (second children))))
    (is (io.github.cl-sdk.xml:xml-node-p (third children)))))

(test illegal-double-dash-in-comment
  "The sequence '--' inside a comment (not as part of '-->') is an error."
  (signals error (io.github.cl-sdk.xml:parse-xml "<el><!-- bad -- comment --></el>")))

;;; ── XML 1.0 conformance — Processing instructions (§2.6) ────────────────

(test pi-preserved-as-node
  "Processing instructions inside an element are preserved as xml-pi nodes."
  (let* ((root (parse-root "<root><?app data?><child /></root>"))
         (children (io.github.cl-sdk.xml:xml-node-children root)))
    (is (= 2 (length children)))
    (is (io.github.cl-sdk.xml:xml-pi-p (first children)))
    (is (string= "app" (io.github.cl-sdk.xml:xml-pi-target (first children))))
    (is (string= "data" (io.github.cl-sdk.xml:xml-pi-data (first children))))
    (is (io.github.cl-sdk.xml:xml-node-p (second children)))))

(test pi-no-data
  "A processing instruction with no data has an empty data string."
  (let* ((root (parse-root "<el><?foo?></el>"))
         (pinstruction (first (io.github.cl-sdk.xml:xml-node-children root))))
    (is (io.github.cl-sdk.xml:xml-pi-p pinstruction))
    (is (string= "foo" (io.github.cl-sdk.xml:xml-pi-target pinstruction)))
    (is (string= "" (io.github.cl-sdk.xml:xml-pi-data pinstruction)))))

;;; ── XML 1.0 conformance — Prolog (§2.8) ─────────────────────────────────

(test xml-declaration-in-prolog
  "An XML declaration is preserved as an xml-pi node in the document prolog."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root />")))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (let ((decl (first (io.github.cl-sdk.xml:xml-document-prolog doc))))
      (is (io.github.cl-sdk.xml:xml-pi-p decl))
      (is (string= "xml" (io.github.cl-sdk.xml:xml-pi-target decl))))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

(test doctype-skipped
  "A DOCTYPE declaration in the prolog is silently skipped."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE root><root />")))
    (is (null (io.github.cl-sdk.xml:xml-document-prolog doc)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

(test doctype-with-internal-subset-skipped
  "A DOCTYPE with an internal subset is silently skipped."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />")))
    (is (null (io.github.cl-sdk.xml:xml-document-prolog doc)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

(test prolog-comment-preserved
  "A comment in the document prolog is preserved as an xml-comment node."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<!-- intro --><root />")))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (io.github.cl-sdk.xml:xml-comment-p (first (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (string= " intro "
                 (io.github.cl-sdk.xml:xml-comment-data
                  (first (io.github.cl-sdk.xml:xml-document-prolog doc)))))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

(test prolog-pi-preserved
  "A processing instruction in the document prolog is preserved as xml-pi."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<?stylesheet type=\"text/xsl\" href=\"a.xsl\"?><root />")))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (io.github.cl-sdk.xml:xml-pi-p (first (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (string= "stylesheet"
                 (io.github.cl-sdk.xml:xml-pi-target (first (io.github.cl-sdk.xml:xml-document-prolog doc)))))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

;;; ── SAX handler — collecting helper ─────────────────────────────────────

(defclass collecting-handler (io.github.cl-sdk.xml:sax-handler)
  ((events :initform '() :accessor handler-events))
  (:documentation "SAX handler that accumulates all events into a list."))

(defmethod io.github.cl-sdk.xml:start-element ((h collecting-handler) tag attributes)
  (push (list :start-element tag attributes) (handler-events h)))

(defmethod io.github.cl-sdk.xml:end-element ((h collecting-handler) tag)
  (push (list :end-element tag) (handler-events h)))

(defmethod io.github.cl-sdk.xml:characters ((h collecting-handler) text)
  (push (list :characters text) (handler-events h)))

(defmethod io.github.cl-sdk.xml:comment ((h collecting-handler) data)
  (push (list :comment data) (handler-events h)))

(defmethod io.github.cl-sdk.xml:processing-instruction ((h collecting-handler) target data)
  (push (list :pi target data) (handler-events h)))

(defmethod io.github.cl-sdk.xml:cdata-section ((h collecting-handler) data)
  (push (list :cdata data) (handler-events h)))

(defmethod io.github.cl-sdk.xml:end-document ((h collecting-handler))
  (nreverse (handler-events h)))

(defun sax-events (str)
  "Parse STR with a COLLECTING-HANDLER and return the event list."
  (io.github.cl-sdk.xml:parse-xml str :handler (make-instance 'collecting-handler)))

;;; ── SAX handler tests ─────────────────────────────────────────────────────

(test sax-default-returns-document
  "parse-xml without :handler returns an xml-document (backward compat)."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<root />")))
    (is (io.github.cl-sdk.xml:xml-document-p doc))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))))

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
  (let ((result (io.github.cl-sdk.xml:parse-xml "<x />" :handler (make-instance 'collecting-handler))))
    (is (listp result))
    (is (equal '(:start-element "x" nil) (first result)))
    (is (equal '(:end-element   "x")     (second result)))))

(test sax-dom-builder-is-default
  "The dom-builder preserves whitespace-filtering: whitespace-only text is discarded."
  (let ((root (parse-root "<root>
  <a />
</root>")))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-node-children root))))
    (is (io.github.cl-sdk.xml:xml-node-p (first (io.github.cl-sdk.xml:xml-node-children root))))))

(test full-prolog
  "A document with XML decl, DOCTYPE, and a prolog comment is parsed correctly."
  (let* ((xml (concatenate 'string
                "<?xml version=\"1.0\"?>"
                "<!DOCTYPE root>"
                "<!-- comment -->"
                "<root><child /></root>"))
         (doc (io.github.cl-sdk.xml:parse-xml xml)))
    ;; XML decl (as xml-pi) + prolog comment = 2 items; DOCTYPE is skipped
    (is (= 2 (length (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (io.github.cl-sdk.xml:xml-pi-p (first (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (is (io.github.cl-sdk.xml:xml-comment-p (second (io.github.cl-sdk.xml:xml-document-prolog doc))))
    (let ((root (io.github.cl-sdk.xml:xml-document-root doc)))
      (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag root)))
      (is (= 1 (length (io.github.cl-sdk.xml:xml-node-children root)))))))

;;; ── Stream input ──────────────────────────────────────────────────────────

(test parse-xml-accepts-stream
  "parse-xml accepts a character stream in addition to a string."
  (let* ((stream (make-string-input-stream "<tag attr=\"val\">text</tag>"))
         (root   (io.github.cl-sdk.xml:xml-document-root (io.github.cl-sdk.xml:parse-xml stream))))
    (is (string= "tag" (io.github.cl-sdk.xml:xml-node-tag root)))
    (is (equal '(("attr" . "val")) (io.github.cl-sdk.xml:xml-node-attributes root)))
    (is (string= "text" (first (io.github.cl-sdk.xml:xml-node-children root))))))

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
         (root   (io.github.cl-sdk.xml:xml-document-root (io.github.cl-sdk.xml:parse-xml stream))))
    (is (string= "el" (io.github.cl-sdk.xml:xml-node-tag root)))
    (is (equal '(("key" . "v")) (io.github.cl-sdk.xml:xml-node-attributes root)))
    (is (string= "hello" (first (io.github.cl-sdk.xml:xml-node-children root))))))

;;; ── Namespace resolution (resolve-namespaces) ────────────────────────────

(defun resolve-root-tag (str)
  "Parse STR, resolve namespaces, and return the tag (xml-qname) of the root xml-node."
  (io.github.cl-sdk.xml:xml-node-tag
   (io.github.cl-sdk.xml:xml-document-root
    (io.github.cl-sdk.xml:resolve-namespaces (io.github.cl-sdk.xml:parse-xml str)))))

(test ns-qname-struct
  "xml-qname struct has prefix, local-name, and namespace-uri fields."
  (let ((q (io.github.cl-sdk.xml:make-xml-qname :prefix "ns" :local-name "tag"
                                  :namespace-uri "http://example.com/")))
    (is (io.github.cl-sdk.xml:xml-qname-p q))
    (is (string= "ns"                  (io.github.cl-sdk.xml:xml-qname-prefix q)))
    (is (string= "tag"                 (io.github.cl-sdk.xml:xml-qname-local-name q)))
    (is (string= "http://example.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri q)))))

(test ns-resolve-prefixed-element
  "A prefixed element tag is resolved to an xml-qname with the correct URI."
  (let ((tag (resolve-root-tag "<ns:root xmlns:ns=\"http://example.com/\" />")))
    (is (io.github.cl-sdk.xml:xml-qname-p tag))
    (is (string= "ns"                  (io.github.cl-sdk.xml:xml-qname-prefix tag)))
    (is (string= "root"                (io.github.cl-sdk.xml:xml-qname-local-name tag)))
    (is (string= "http://example.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-default-namespace
  "An unprefixed element in a default namespace gets the declared namespace URI."
  (let ((tag (resolve-root-tag "<root xmlns=\"http://example.com/\" />")))
    (is (io.github.cl-sdk.xml:xml-qname-p tag))
    (is (null (io.github.cl-sdk.xml:xml-qname-prefix tag)))
    (is (string= "root"                (io.github.cl-sdk.xml:xml-qname-local-name tag)))
    (is (string= "http://example.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-no-namespace
  "An unprefixed element with no default namespace has nil namespace-uri."
  (let ((tag (resolve-root-tag "<root />")))
    (is (io.github.cl-sdk.xml:xml-qname-p tag))
    (is (null (io.github.cl-sdk.xml:xml-qname-prefix tag)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-qname-local-name tag)))
    (is (null (io.github.cl-sdk.xml:xml-qname-namespace-uri tag)))))

(test ns-resolve-xmlns-attrs-removed
  "xmlns and xmlns:prefix attributes are absent after resolve-namespaces."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns=\"http://a.com/\" xmlns:b=\"http://b.com/\" />"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (attrs    (io.github.cl-sdk.xml:xml-node-attributes
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (null attrs))))

(test ns-resolve-prefixed-attribute
  "A prefixed attribute key is resolved to an xml-qname with the correct URI."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns:ns=\"http://example.com/\" ns:attr=\"val\" />"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (attrs    (io.github.cl-sdk.xml:xml-node-attributes
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (io.github.cl-sdk.xml:xml-qname-p key))
      (is (string= "ns"                  (io.github.cl-sdk.xml:xml-qname-prefix key)))
      (is (string= "attr"                (io.github.cl-sdk.xml:xml-qname-local-name key)))
      (is (string= "http://example.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri key))))
    (is (string= "val" (cdar attrs)))))

(test ns-resolve-unprefixed-attribute-has-no-ns
  "An unprefixed attribute has nil namespace-uri even when a default ns is active."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns=\"http://example.com/\" attr=\"val\" />"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (attrs    (io.github.cl-sdk.xml:xml-node-attributes
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (io.github.cl-sdk.xml:xml-qname-p key))
      (is (null (io.github.cl-sdk.xml:xml-qname-prefix key)))
      (is (string= "attr" (io.github.cl-sdk.xml:xml-qname-local-name key)))
      (is (null (io.github.cl-sdk.xml:xml-qname-namespace-uri key))))))

(test ns-resolve-inherited-binding
  "Namespace bindings declared on a parent are visible in child elements."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns:ns=\"http://example.com/\"><ns:child /></root>"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (child    (first (io.github.cl-sdk.xml:xml-node-children
                           (io.github.cl-sdk.xml:xml-document-root resolved))))
         (ctag     (io.github.cl-sdk.xml:xml-node-tag child)))
    (is (io.github.cl-sdk.xml:xml-qname-p ctag))
    (is (string= "ns"                  (io.github.cl-sdk.xml:xml-qname-prefix ctag)))
    (is (string= "child"               (io.github.cl-sdk.xml:xml-qname-local-name ctag)))
    (is (string= "http://example.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri ctag)))))

(test ns-resolve-xml-prefix-predeclared
  "The 'xml' prefix is pre-declared and resolves without an explicit xmlns:xml."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml "<root xml:lang=\"en\" />"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (attrs    (io.github.cl-sdk.xml:xml-node-attributes
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (= 1 (length attrs)))
    (let ((key (caar attrs)))
      (is (string= "xml"  (io.github.cl-sdk.xml:xml-qname-prefix key)))
      (is (string= "lang" (io.github.cl-sdk.xml:xml-qname-local-name key)))
      (is (string= "http://www.w3.org/XML/1998/namespace"
                   (io.github.cl-sdk.xml:xml-qname-namespace-uri key))))))

(test ns-resolve-undeclared-prefix-error
  "Using an undeclared namespace prefix signals an error."
  (signals error
    (io.github.cl-sdk.xml:resolve-namespaces (io.github.cl-sdk.xml:parse-xml "<ns:root />"))))

(test ns-resolve-default-ns-reset
  "An xmlns='' declaration on a child resets the default namespace to nil."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns=\"http://example.com/\"><child xmlns=\"\" /></root>"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (child    (first (io.github.cl-sdk.xml:xml-node-children
                           (io.github.cl-sdk.xml:xml-document-root resolved))))
         (ctag     (io.github.cl-sdk.xml:xml-node-tag child)))
    (is (null (io.github.cl-sdk.xml:xml-qname-namespace-uri ctag)))))

(test ns-resolve-returns-xml-document
  "resolve-namespaces returns an xml-document."
  (let ((resolved (io.github.cl-sdk.xml:resolve-namespaces (io.github.cl-sdk.xml:parse-xml "<root />"))))
    (is (io.github.cl-sdk.xml:xml-document-p resolved))))

(test ns-resolve-preserves-prolog
  "resolve-namespaces preserves the document prolog unchanged."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml "<!-- comment --><root />"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc)))
    (is (equal (io.github.cl-sdk.xml:xml-document-prolog doc)
               (io.github.cl-sdk.xml:xml-document-prolog resolved)))))

(test ns-resolve-multiple-prefixes
  "Multiple namespace prefixes on the same element are all resolved correctly."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    (concatenate 'string
                      "<root xmlns:a=\"http://a.com/\""
                      "      xmlns:b=\"http://b.com/\""
                      "      a:x=\"1\" b:y=\"2\" />")))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (attrs    (io.github.cl-sdk.xml:xml-node-attributes
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (= 2 (length attrs)))
    (let ((a-attr (find "x" attrs :key (lambda (e) (io.github.cl-sdk.xml:xml-qname-local-name (car e)))
                                  :test #'string=))
          (b-attr (find "y" attrs :key (lambda (e) (io.github.cl-sdk.xml:xml-qname-local-name (car e)))
                                  :test #'string=)))
      (is (string= "http://a.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri (car a-attr))))
      (is (string= "http://b.com/" (io.github.cl-sdk.xml:xml-qname-namespace-uri (car b-attr)))))))

(test ns-resolve-preserves-text-children
  "Text and CDATA children are passed through unchanged by resolve-namespaces."
  (let* ((doc      (io.github.cl-sdk.xml:parse-xml
                    "<root xmlns=\"http://example.com/\">hello</root>"))
         (resolved (io.github.cl-sdk.xml:resolve-namespaces doc))
         (children (io.github.cl-sdk.xml:xml-node-children
                    (io.github.cl-sdk.xml:xml-document-root resolved))))
    (is (= 1 (length children)))
    (is (string= "hello" (first children)))))

;;; ── DTD element parsing ───────────────────────────────────────────────────

;;; Helpers

(defun parse-doctype-elements (str)
  "Parse STR and return the list of xml-dtd-element structs from the DOCTYPE."
  (io.github.cl-sdk.xml:xml-doctype-elements
   (io.github.cl-sdk.xml:xml-document-doctype (io.github.cl-sdk.xml:parse-xml str))))

;;; xml-dtd-element struct

(test dtd-element-struct
  "xml-dtd-element struct has name and content-model fields."
  (let ((e (io.github.cl-sdk.xml:make-xml-dtd-element :name "root" :content-model :empty)))
    (is (io.github.cl-sdk.xml:xml-dtd-element-p e))
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-element-name e)))
    (is (eq :empty (io.github.cl-sdk.xml:xml-dtd-element-content-model e)))))

;;; xml-doctype struct

(test dtd-doctype-struct
  "xml-doctype struct has name, public-id, system-id, and elements fields."
  (let ((d (io.github.cl-sdk.xml:make-xml-doctype :name "root" :public-id nil
                                    :system-id "root.dtd" :elements '())))
    (is (io.github.cl-sdk.xml:xml-doctype-p d))
    (is (string= "root" (io.github.cl-sdk.xml:xml-doctype-name d)))
    (is (null (io.github.cl-sdk.xml:xml-doctype-public-id d)))
    (is (string= "root.dtd" (io.github.cl-sdk.xml:xml-doctype-system-id d)))
    (is (null (io.github.cl-sdk.xml:xml-doctype-elements d)))))

;;; xml-document-doctype accessor

(test no-doctype-gives-nil
  "A document without a DOCTYPE has xml-document-doctype = NIL."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<root />")))
    (is (null (io.github.cl-sdk.xml:xml-document-doctype doc)))))

(test doctype-present
  "A document with a DOCTYPE has a non-nil xml-doctype in xml-document-doctype."
  (let* ((doc (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE root><root />"))
         (dtd (io.github.cl-sdk.xml:xml-document-doctype doc)))
    (is (io.github.cl-sdk.xml:xml-doctype-p dtd))
    (is (string= "root" (io.github.cl-sdk.xml:xml-doctype-name dtd)))))

(test doctype-no-external-id
  "A DOCTYPE with no external identifier has nil public-id and system-id."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE html><html />"))))
    (is (null (io.github.cl-sdk.xml:xml-doctype-public-id dtd)))
    (is (null (io.github.cl-sdk.xml:xml-doctype-system-id dtd)))))

(test doctype-system-id
  "A DOCTYPE with SYSTEM identifier records the system-id."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml
                "<!DOCTYPE root SYSTEM \"root.dtd\"><root />"))))
    (is (null (io.github.cl-sdk.xml:xml-doctype-public-id dtd)))
    (is (string= "root.dtd" (io.github.cl-sdk.xml:xml-doctype-system-id dtd)))))

(test doctype-public-id
  "A DOCTYPE with PUBLIC identifier records both public-id and system-id."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml
                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0//EN\" \"xhtml1.dtd\"><html />"))))
    (is (string= "-//W3C//DTD XHTML 1.0//EN" (io.github.cl-sdk.xml:xml-doctype-public-id dtd)))
    (is (string= "xhtml1.dtd" (io.github.cl-sdk.xml:xml-doctype-system-id dtd)))))

(test doctype-root-element-preserved
  "The root element is still correctly parsed when a DOCTYPE is present."
  (let ((doc (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE root><root><child /></root>")))
    (is (string= "root" (io.github.cl-sdk.xml:xml-node-tag (io.github.cl-sdk.xml:xml-document-root doc))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-node-children
                      (io.github.cl-sdk.xml:xml-document-root doc)))))))

;;; DTD ELEMENT content models

(test dtd-element-empty
  "<!ELEMENT> with EMPTY content model is parsed as :empty."
  (let ((elems (parse-doctype-elements
                "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />")))
    (is (= 1 (length elems)))
    (let ((e (first elems)))
      (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-element-name e)))
      (is (eq :empty (io.github.cl-sdk.xml:xml-dtd-element-content-model e))))))

(test dtd-element-any
  "<!ELEMENT> with ANY content model is parsed as :any."
  (let ((elems (parse-doctype-elements
                "<!DOCTYPE root [<!ELEMENT root ANY>]><root />")))
    (is (= 1 (length elems)))
    (is (eq :any (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))))

(test dtd-element-pcdata-only
  "<!ELEMENT> with (#PCDATA) is parsed as (:mixed)."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (#PCDATA)>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (consp cm))
    (is (eq :mixed (car cm)))
    (is (null (cdr cm)))))

(test dtd-element-mixed-content
  "<!ELEMENT> with (#PCDATA|a|b)* is parsed as (:mixed \"a\" \"b\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (#PCDATA|a|b)*>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (eq :mixed (car cm)))
    (is (equal '("a" "b") (cdr cm)))))

(test dtd-element-sequence
  "<!ELEMENT> with (a, b, c) is parsed as (:seq \"a\" \"b\" \"c\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b, c)>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:seq "a" "b" "c") cm))))

(test dtd-element-choice
  "<!ELEMENT> with (a|b) is parsed as (:choice \"a\" \"b\")."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a|b)>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:choice "a" "b") cm))))

(test dtd-element-sequence-plus
  "<!ELEMENT> with (a, b)+ wraps the group with :+."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b)+>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:+ (:seq "a" "b")) cm))))

(test dtd-element-choice-star
  "<!ELEMENT> with (a|b)* wraps the group with :*."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a|b)*>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:* (:choice "a" "b")) cm))))

(test dtd-element-sequence-opt
  "<!ELEMENT> with (a, b)? wraps the group with :?."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a, b)?>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:? (:seq "a" "b")) cm))))

(test dtd-element-leaf-quantifier
  "A leaf content particle with + quantifier is wrapped with :+."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root (a+)>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    ;; (a+) → single-element sequence containing (:+ "a")
    (is (equal '(:seq (:+ "a")) cm))))

(test dtd-element-nested-group
  "<!ELEMENT> with ((a,b)|c) is parsed as a nested group."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ((a,b)|c)>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
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
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-element-name (first elems))))
    (is (string= "a"    (io.github.cl-sdk.xml:xml-dtd-element-name (second elems))))
    (is (string= "b"    (io.github.cl-sdk.xml:xml-dtd-element-name (third elems))))
    (is (equal '(:seq "a" "b")
               (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (eq :empty (io.github.cl-sdk.xml:xml-dtd-element-content-model (third elems))))))

(test dtd-attlist-coexists-with-element
  "<!ATTLIST> declaration does not interfere with <!ELEMENT> parsing."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ELEMENT root EMPTY>"
                  "<!ATTLIST root id ID #REQUIRED>"
                  "]><root />"))))
         (elems (io.github.cl-sdk.xml:xml-doctype-elements dtd)))
    (is (= 1 (length elems)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-element-name (first elems))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-attlists dtd))))))

(test dtd-comment-in-subset-skipped
  "Comments inside the internal subset are silently skipped."
  (let ((elems (parse-doctype-elements
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!-- declare the root -->"
                  "<!ELEMENT root EMPTY>"
                  "]><root />"))))
    (is (= 1 (length elems)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-element-name (first elems))))))

(test dtd-whitespace-in-content-model
  "Whitespace around operators in a content model is handled correctly."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ( a | b | c )>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:choice "a" "b" "c") cm))))

(test dtd-whitespace-sequence
  "Whitespace around ',' in a sequence is handled correctly."
  (let* ((elems (parse-doctype-elements
                 "<!DOCTYPE root [<!ELEMENT root ( a , b , c )>]><root />"))
         (cm (io.github.cl-sdk.xml:xml-dtd-element-content-model (first elems))))
    (is (equal '(:seq "a" "b" "c") cm))))

;;; SAX doctype-declaration event

(test sax-doctype-declaration-event
  "The doctype-declaration SAX event is fired with the parsed xml-doctype."
  (let* ((handler (make-instance 'collecting-handler))
         (result  (io.github.cl-sdk.xml:parse-xml
                   "<!DOCTYPE root [<!ELEMENT root EMPTY>]><root />"
                   :handler handler)))
    ;; collecting-handler doesn't define doctype-declaration, so the default
    ;; no-op is used; the event must not crash and the rest must parse.
    (is (listp result))
    (is (some (lambda (e) (equal '(:start-element "root" nil) e)) result))))

;;; ── DTD ATTLIST parsing ───────────────────────────────────────────────────

(defun parse-doctype-attlists (str)
  "Parse STR and return the list of xml-dtd-attlist structs from the DOCTYPE."
  (io.github.cl-sdk.xml:xml-doctype-attlists
   (io.github.cl-sdk.xml:xml-document-doctype (io.github.cl-sdk.xml:parse-xml str))))

;;; xml-dtd-att-def struct

(test dtd-att-def-struct
  "xml-dtd-att-def struct has name, type, and default fields."
  (let ((d (io.github.cl-sdk.xml:make-xml-dtd-att-def :name "id" :type :id :default :required)))
    (is (io.github.cl-sdk.xml:xml-dtd-att-def-p d))
    (is (string= "id" (io.github.cl-sdk.xml:xml-dtd-att-def-name d)))
    (is (eq :id (io.github.cl-sdk.xml:xml-dtd-att-def-type d)))
    (is (eq :required (io.github.cl-sdk.xml:xml-dtd-att-def-default d)))))

;;; xml-dtd-attlist struct

(test dtd-attlist-struct
  "xml-dtd-attlist struct has element-name and definitions fields."
  (let ((a (io.github.cl-sdk.xml:make-xml-dtd-attlist :element-name "root" :definitions '())))
    (is (io.github.cl-sdk.xml:xml-dtd-attlist-p a))
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-attlist-element-name a)))
    (is (null (io.github.cl-sdk.xml:xml-dtd-attlist-definitions a)))))

;;; xml-doctype-attlists accessor

(test dtd-doctype-has-attlists
  "xml-doctype-attlists returns NIL when no ATTLIST is present."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml "<!DOCTYPE root><root />"))))
    (is (null (io.github.cl-sdk.xml:xml-doctype-attlists dtd)))))

;;; Tokenized attribute types

(test dtd-attlist-cdata-required
  "<!ATTLIST> with CDATA type and #REQUIRED default is parsed correctly."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE root [<!ATTLIST root title CDATA #REQUIRED>]><root />"))
         (al (first alists))
         (def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions al))))
    (is (= 1 (length alists)))
    (is (string= "root" (io.github.cl-sdk.xml:xml-dtd-attlist-element-name al)))
    (is (string= "title" (io.github.cl-sdk.xml:xml-dtd-att-def-name def)))
    (is (eq :cdata (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))
    (is (eq :required (io.github.cl-sdk.xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-id-implied
  "<!ATTLIST> with ID type and #IMPLIED default is parsed correctly."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE root [<!ATTLIST root id ID #IMPLIED>]><root />"))
         (def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions (first alists)))))
    (is (eq :id (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))
    (is (eq :implied (io.github.cl-sdk.xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-idref-type
  "<!ATTLIST> with IDREF type is parsed as :idref."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x IDREF #IMPLIED>]><r />"))))))
    (is (eq :idref (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-idrefs-type
  "<!ATTLIST> with IDREFS type is parsed as :idrefs."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x IDREFS #IMPLIED>]><r />"))))))
    (is (eq :idrefs (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-entity-type
  "<!ATTLIST> with ENTITY type is parsed as :entity."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x ENTITY #IMPLIED>]><r />"))))))
    (is (eq :entity (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-entities-type
  "<!ATTLIST> with ENTITIES type is parsed as :entities."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x ENTITIES #IMPLIED>]><r />"))))))
    (is (eq :entities (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-nmtoken-type
  "<!ATTLIST> with NMTOKEN type is parsed as :nmtoken."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x NMTOKEN #IMPLIED>]><r />"))))))
    (is (eq :nmtoken (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-nmtokens-type
  "<!ATTLIST> with NMTOKENS type is parsed as :nmtokens."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r x NMTOKENS #IMPLIED>]><r />"))))))
    (is (eq :nmtokens (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

;;; Enumerated and NOTATION types

(test dtd-attlist-enumeration-type
  "<!ATTLIST> with enumerated type is parsed as (:enumeration token+)."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE r [<!ATTLIST r size (small|medium|large) \"medium\">]><r />"))
         (def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions (first alists)))))
    (is (equal '(:enumeration "small" "medium" "large")
               (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

(test dtd-attlist-notation-type
  "<!ATTLIST> with NOTATION type is parsed as (:notation name+)."
  (let* ((alists (parse-doctype-attlists
                  "<!DOCTYPE r [<!ATTLIST r fmt NOTATION (gif|png) #IMPLIED>]><r />"))
         (def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions (first alists)))))
    (is (equal '(:notation "gif" "png")
               (io.github.cl-sdk.xml:xml-dtd-att-def-type def)))))

;;; Default declarations

(test dtd-attlist-fixed-default
  "<!ATTLIST> with #FIXED AttValue is parsed as (:fixed value)."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r lang CDATA #FIXED \"en\">]><r />"))))))
    (is (equal '(:fixed "en") (io.github.cl-sdk.xml:xml-dtd-att-def-default def)))))

(test dtd-attlist-bare-default
  "<!ATTLIST> with a bare AttValue is parsed as (:default value)."
  (let* ((def (first (io.github.cl-sdk.xml:xml-dtd-attlist-definitions
                      (first (parse-doctype-attlists
                              "<!DOCTYPE r [<!ATTLIST r lang CDATA \"en\">]><r />"))))))
    (is (equal '(:default "en") (io.github.cl-sdk.xml:xml-dtd-att-def-default def)))))

;;; Multiple attribute definitions in one ATTLIST

(test dtd-attlist-multiple-defs
  "Multiple AttDef entries in one <!ATTLIST> declaration are all captured."
  (let* ((al (first (parse-doctype-attlists
                     (concatenate 'string
                       "<!DOCTYPE r [<!ATTLIST r"
                       "  id ID #REQUIRED"
                       "  class CDATA #IMPLIED"
                       "  lang CDATA \"en\">]><r />"))))
         (defs (io.github.cl-sdk.xml:xml-dtd-attlist-definitions al)))
    (is (= 3 (length defs)))
    (is (string= "id"    (io.github.cl-sdk.xml:xml-dtd-att-def-name (first defs))))
    (is (string= "class" (io.github.cl-sdk.xml:xml-dtd-att-def-name (second defs))))
    (is (string= "lang"  (io.github.cl-sdk.xml:xml-dtd-att-def-name (third defs))))))

;;; Multiple ATTLIST declarations

(test dtd-multiple-attlists
  "Multiple <!ATTLIST> declarations are captured in order."
  (let* ((alists (parse-doctype-attlists
                  (concatenate 'string
                    "<!DOCTYPE r [<!ATTLIST r id ID #REQUIRED>"
                    "<!ATTLIST p class CDATA #IMPLIED>]><r />"))))
    (is (= 2 (length alists)))
    (is (string= "r" (io.github.cl-sdk.xml:xml-dtd-attlist-element-name (first alists))))
    (is (string= "p" (io.github.cl-sdk.xml:xml-dtd-attlist-element-name (second alists))))))

;;; ── DTD ENTITY parsing ────────────────────────────────────────────────────

(defun parse-doctype-entities (str)
  "Parse STR and return the list of xml-dtd-entity structs from the DOCTYPE."
  (io.github.cl-sdk.xml:xml-doctype-entities
   (io.github.cl-sdk.xml:xml-document-doctype (io.github.cl-sdk.xml:parse-xml str))))

;;; xml-dtd-entity struct

(test dtd-entity-struct
  "xml-dtd-entity struct has name, parameter-p, and definition fields."
  (let ((e (io.github.cl-sdk.xml:make-xml-dtd-entity :name "amp" :parameter-p nil
                                        :definition "&")))
    (is (io.github.cl-sdk.xml:xml-dtd-entity-p e))
    (is (string= "amp" (io.github.cl-sdk.xml:xml-dtd-entity-name e)))
    (is (null (io.github.cl-sdk.xml:xml-dtd-entity-parameter-p e)))
    (is (string= "&" (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; Internal general entity

(test dtd-entity-internal-general
  "<!ENTITY name 'value'> is parsed as an internal general entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY greeting \"Hello\">]><r />"))
         (e (first ents)))
    (is (= 1 (length ents)))
    (is (string= "greeting" (io.github.cl-sdk.xml:xml-dtd-entity-name e)))
    (is (null (io.github.cl-sdk.xml:xml-dtd-entity-parameter-p e)))
    (is (string= "Hello" (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; Internal parameter entity

(test dtd-entity-internal-parameter
  "<!ENTITY % name 'value'> is parsed as an internal parameter entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY % inline \"(#PCDATA)\">]><r />"))
         (e (first ents)))
    (is (= 1 (length ents)))
    (is (string= "inline" (io.github.cl-sdk.xml:xml-dtd-entity-name e)))
    (is (io.github.cl-sdk.xml:xml-dtd-entity-parameter-p e))
    (is (string= "(#PCDATA)" (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; External general entity — SYSTEM

(test dtd-entity-external-system
  "<!ENTITY name SYSTEM 'uri'> is parsed as an external entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY logo SYSTEM \"logo.png\">]><r />"))
         (e (first ents)))
    (is (equal '(:external nil "logo.png") (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; External general entity — PUBLIC

(test dtd-entity-external-public
  "<!ENTITY name PUBLIC 'pub' 'sys'> is parsed with both identifiers."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY iso PUBLIC \"-//ISO//EN\" \"iso.ent\">]><r />"))
         (e (first ents)))
    (is (equal '(:external "-//ISO//EN" "iso.ent")
               (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; Unparsed (NDATA) entity

(test dtd-entity-unparsed-ndata
  "<!ENTITY name SYSTEM 'uri' NDATA fmt> is parsed as an unparsed entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY logo SYSTEM \"logo.gif\" NDATA gif>]><r />"))
         (e (first ents)))
    (is (equal '(:unparsed nil "logo.gif" "gif")
               (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

;;; External parameter entity

(test dtd-entity-external-parameter
  "<!ENTITY % name SYSTEM 'uri'> is parsed as an external parameter entity."
  (let* ((ents (parse-doctype-entities
                "<!DOCTYPE r [<!ENTITY % common SYSTEM \"common.ent\">]><r />"))
         (e (first ents)))
    (is (io.github.cl-sdk.xml:xml-dtd-entity-parameter-p e))
    (is (equal '(:external nil "common.ent")
               (io.github.cl-sdk.xml:xml-dtd-entity-definition e)))))

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
    (is (string= "a" (io.github.cl-sdk.xml:xml-dtd-entity-name (first ents))))
    (is (string= "b" (io.github.cl-sdk.xml:xml-dtd-entity-name (second ents))))))

;;; ── DTD NOTATION parsing ──────────────────────────────────────────────────

(defun parse-doctype-notations (str)
  "Parse STR and return the list of xml-dtd-notation structs from the DOCTYPE."
  (io.github.cl-sdk.xml:xml-doctype-notations
   (io.github.cl-sdk.xml:xml-document-doctype (io.github.cl-sdk.xml:parse-xml str))))

;;; xml-dtd-notation struct

(test dtd-notation-struct
  "xml-dtd-notation struct has name, public-id, and system-id fields."
  (let ((n (io.github.cl-sdk.xml:make-xml-dtd-notation :name "gif" :public-id nil
                                          :system-id "image/gif")))
    (is (io.github.cl-sdk.xml:xml-dtd-notation-p n))
    (is (string= "gif" (io.github.cl-sdk.xml:xml-dtd-notation-name n)))
    (is (null (io.github.cl-sdk.xml:xml-dtd-notation-public-id n)))
    (is (string= "image/gif" (io.github.cl-sdk.xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with SYSTEM identifier

(test dtd-notation-system
  "<!NOTATION name SYSTEM 'uri'> records system-id and nil public-id."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION gif SYSTEM \"image/gif\">]><r />"))
         (n (first notations)))
    (is (= 1 (length notations)))
    (is (string= "gif"       (io.github.cl-sdk.xml:xml-dtd-notation-name n)))
    (is (null               (io.github.cl-sdk.xml:xml-dtd-notation-public-id n)))
    (is (string= "image/gif" (io.github.cl-sdk.xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with PUBLIC + SYSTEM identifiers (ExternalID)

(test dtd-notation-public-and-system
  "<!NOTATION name PUBLIC 'pub' 'sys'> records both identifiers."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION jpeg PUBLIC \"-//JPEG//\" \"image/jpeg\">]><r />"))
         (n (first notations)))
    (is (string= "-//JPEG//"   (io.github.cl-sdk.xml:xml-dtd-notation-public-id n)))
    (is (string= "image/jpeg"  (io.github.cl-sdk.xml:xml-dtd-notation-system-id n)))))

;;; NOTATION with PUBLIC only (PublicID — no system literal)

(test dtd-notation-public-only
  "<!NOTATION name PUBLIC 'pub'> with no system literal records nil system-id."
  (let* ((notations (parse-doctype-notations
                     "<!DOCTYPE r [<!NOTATION pdf PUBLIC \"-//Adobe//PDF\">]><r />"))
         (n (first notations)))
    (is (string= "-//Adobe//PDF" (io.github.cl-sdk.xml:xml-dtd-notation-public-id n)))
    (is (null (io.github.cl-sdk.xml:xml-dtd-notation-system-id n)))))

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
    (is (string= "gif" (io.github.cl-sdk.xml:xml-dtd-notation-name (first notations))))
    (is (string= "png" (io.github.cl-sdk.xml:xml-dtd-notation-name (second notations))))))

;;; ── All DTD declaration types together ────────────────────────────────────

(test dtd-all-declaration-types
  "DOCTYPE internal subset with ELEMENT, ATTLIST, ENTITY, and NOTATION all parsed."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ELEMENT root (title, body)>"
                  "<!ATTLIST root id ID #REQUIRED>"
                  "<!ENTITY copyright \"(c) 2024\">"
                  "<!NOTATION svg SYSTEM \"image/svg+xml\">"
                  "]><root/>")))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-elements dtd))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-attlists dtd))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-entities dtd))))
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-notations dtd))))
    (is (string= "root"
                 (io.github.cl-sdk.xml:xml-dtd-element-name
                  (first (io.github.cl-sdk.xml:xml-doctype-elements dtd)))))
    (is (string= "root"
                 (io.github.cl-sdk.xml:xml-dtd-attlist-element-name
                  (first (io.github.cl-sdk.xml:xml-doctype-attlists dtd)))))
    (is (string= "copyright"
                 (io.github.cl-sdk.xml:xml-dtd-entity-name
                  (first (io.github.cl-sdk.xml:xml-doctype-entities dtd)))))
    (is (string= "svg"
                 (io.github.cl-sdk.xml:xml-dtd-notation-name
                  (first (io.github.cl-sdk.xml:xml-doctype-notations dtd)))))))

;;; PE references between declarations are tolerated

(test dtd-pe-reference-between-decls
  "Parameter entity references between markup declarations do not cause errors."
  (let* ((dtd (io.github.cl-sdk.xml:xml-document-doctype
               (io.github.cl-sdk.xml:parse-xml
                (concatenate 'string
                  "<!DOCTYPE root ["
                  "<!ENTITY % ignored \"whatever\">"
                  "%ignored;"
                  "<!ELEMENT root EMPTY>"
                  "]><root />")))))
    ;; The PE reference is skipped; ELEMENT is still parsed
    (is (= 1 (length (io.github.cl-sdk.xml:xml-doctype-elements dtd))))))

