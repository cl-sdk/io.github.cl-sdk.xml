(in-package #:cl-xml)

;;; Data structures

(defstruct xml-node
  "Represents an XML element node with a tag name, an alist of attributes,
and a list of children. Each child may be an xml-node (element), xml-comment,
xml-pi, xml-cdata, or a string (character data)."
  tag
  attributes
  children)

(defstruct xml-comment
  "Represents an XML comment <!-- … -->."
  data)

(defstruct xml-pi
  "Represents an XML processing instruction <?target data?>."
  target
  data)

(defstruct xml-cdata
  "Represents an XML CDATA section <![CDATA[…]]>."
  data)

(defstruct xml-document
  "Represents a parsed XML document.
PROLOG is a list of xml-comment and xml-pi nodes that precede the root element.
DOCTYPE is an xml-doctype struct if a DOCTYPE declaration was present, or NIL.
ROOT is the root xml-node."
  prolog
  doctype
  root)

(defstruct xml-dtd-element
  "Represents a DTD <!ELEMENT> declaration.
NAME is the element type name string.
CONTENT-MODEL is the parsed content model:
  :empty               — EMPTY keyword
  :any                 — ANY keyword
  (:mixed name*)       — mixed content (#PCDATA possibly with element names)
  content-particle     — element content (see below)

A content-particle is one of:
  \"name\"              — element name, occurs exactly once
  (:? content)         — content occurs 0 or 1 times (optional)
  (:* content)         — content occurs 0 or more times
  (:+ content)         — content occurs 1 or more times
  (:seq  cp*)          — sequence of content particles, exactly once
  (:choice cp*)        — choice among content particles, exactly once"
  name
  content-model)

(defstruct xml-doctype
  "Represents a parsed DOCTYPE declaration.
NAME is the root element type name string.
PUBLIC-ID is the public identifier string, or NIL.
SYSTEM-ID is the system identifier string, or NIL.
ELEMENTS is a list of xml-dtd-element structs parsed from the internal subset."
  name
  public-id
  system-id
  elements)

(defstruct xml-qname
  "Represents a namespace-qualified XML name (Namespaces in XML 1.0 §2.1).
PREFIX is the namespace prefix string, or NIL when there is no prefix.
LOCAL-NAME is the local part of the name (the portion after ':'), or the full
unqualified name when PREFIX is NIL.
NAMESPACE-URI is the namespace URI string that PREFIX is bound to, or NIL when
the name is in no namespace."
  prefix
  local-name
  namespace-uri)

;;; Character classification — XML 1.0 §2.3

(defun name-start-char-p (ch)
  "True if CH is a valid XML 1.0 NameStartChar."
  (let ((c (char-code ch)))
    (or (char= ch #\:)
        (char= ch #\_)
        (and (char>= ch #\A) (char<= ch #\Z))
        (and (char>= ch #\a) (char<= ch #\z))
        (and (>= c #xC0)    (<= c #xD6))
        (and (>= c #xD8)    (<= c #xF6))
        (and (>= c #xF8)    (<= c #x2FF))
        (and (>= c #x370)   (<= c #x37D))
        (and (>= c #x37F)   (<= c #x1FFF))
        (and (>= c #x200C)  (<= c #x200D))
        (and (>= c #x2070)  (<= c #x218F))
        (and (>= c #x2C00)  (<= c #x2FEF))
        (and (>= c #x3001)  (<= c #xD7FF))
        (and (>= c #xF900)  (<= c #xFDCF))
        (and (>= c #xFDF0)  (<= c #xFFFD))
        (and (>= c #x10000) (<= c #xEFFFF)))))

(defun name-char-p (ch)
  "True if CH is a valid XML 1.0 NameChar."
  (let ((c (char-code ch)))
    (or (name-start-char-p ch)
        (char= ch #\-)
        (char= ch #\.)
        (and (char>= ch #\0) (char<= ch #\9))
        (= c #xB7)
        (and (>= c #x0300) (<= c #x036F))
        (and (>= c #x203F) (<= c #x2040)))))

(defun xml-whitespace-p (ch)
  "True if CH is an XML 1.0 whitespace character (S production)."
  (member ch '(#\Space #\Tab #\Newline #\Return)))

;;; Low-level stream helpers

(defun skip-whitespace (stream)
  "Advance past XML whitespace characters in STREAM."
  (loop while (let ((ch (peek-char nil stream nil nil)))
                (and ch (xml-whitespace-p ch)))
        do (read-char stream)))

(defun parse-name (stream)
  "Parse an XML Name (tag or attribute name).
Validates NameStartChar for the first character and NameChar for the rest.
Returns the name as a string."
  (let ((first-ch (peek-char nil stream nil nil)))
    (unless (and first-ch (name-start-char-p first-ch))
      (error "Invalid XML name start character"))
    (let ((buf (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)))
      (vector-push-extend (read-char stream) buf)
      (loop while (let ((ch (peek-char nil stream nil nil)))
                    (and ch (name-char-p ch)))
            do (vector-push-extend (read-char stream) buf))
      (copy-seq buf))))

;;; Entity and character reference expansion — XML 1.0 §4.1, §4.6

(defun expand-char-ref (stream)
  "Expand a character reference starting just after '&#'.
Returns the expanded string."
  (let ((hex-p (eql (peek-char nil stream nil nil) #\x)))
    (when hex-p (read-char stream))
    (let ((buf (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop while (let ((ch (peek-char nil stream nil nil)))
                    (and ch (digit-char-p ch (if hex-p 16 10))))
            do (vector-push-extend (read-char stream) buf))
      (when (zerop (fill-pointer buf))
        (error "Empty character reference"))
      (unless (eql (peek-char nil stream nil nil) #\;)
        (error "Unterminated character reference"))
      (read-char stream)                ; consume ';'
      (string (code-char (parse-integer (copy-seq buf) :radix (if hex-p 16 10)))))))

(defun expand-entity-ref (stream)
  "Parse and expand a predefined entity or character reference starting just
after '&'.  Supports &amp; &lt; &gt; &quot; &apos; and &#N; / &#xN;.
Returns the expanded string."
  (if (eql (peek-char nil stream nil nil) #\#)
      (progn
        (read-char stream)              ; consume '#'
        (expand-char-ref stream))
      (let ((name (parse-name stream)))
        (unless (eql (peek-char nil stream nil nil) #\;)
          (error "Unterminated entity reference '&~a'" name))
        (read-char stream)              ; consume ';'
        (let ((expansion (cdr (assoc name
                                     '(("amp"  . "&")
                                       ("lt"   . "<")
                                       ("gt"   . ">")
                                       ("quot" . "\"")
                                       ("apos" . "'"))
                                     :test #'string=))))
          (unless expansion
            (error "Unknown entity reference '&~a;'" name))
          expansion))))

;;; Attribute value parsing — XML 1.0 §2.3, §3.3.3

(defun parse-attribute-value (stream)
  "Parse a single- or double-quoted attribute value from STREAM,
expanding entity and character references and forbidding bare '<'.
Returns the value string."
  (let ((quote (read-char stream nil nil)))
    (unless (member quote '(#\" #\'))
      (error "Expected a quote character"))
    (let ((buf (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (peek-char nil stream nil nil)))
          (unless ch (error "Unterminated attribute value"))
          (cond
            ((char= ch quote)
             (read-char stream)
             (return))
            ((char= ch #\<)
             (error "Illegal '<' in attribute value"))
            ((char= ch #\&)
             (read-char stream)         ; consume '&'
             (let ((expansion (expand-entity-ref stream)))
               (loop for c across expansion do (vector-push-extend c buf))))
            (t
             (vector-push-extend (read-char stream) buf)))))
      (copy-seq buf))))

;;; Attribute list parsing — XML 1.0 §3.1

(defun parse-attributes (stream)
  "Parse zero or more attributes from STREAM, stopping at '>' or '/>'.
Detects duplicate attribute names.
Returns an alist of (name . value) pairs."
  (let ((attributes '()))
    (loop
      (skip-whitespace stream)
      (let ((ch (peek-char nil stream nil nil)))
        (when (or (null ch) (member ch '(#\> #\/)))
          (return)))
      (let ((name (parse-name stream)))
        (when (assoc name attributes :test #'string=)
          (error "Duplicate attribute '~a'" name))
        (skip-whitespace stream)
        (unless (eql (peek-char nil stream nil nil) #\=)
          (error "Expected '=' after attribute name '~a'" name))
        (read-char stream)              ; consume '='
        (skip-whitespace stream)
        (let ((value (parse-attribute-value stream)))
          (push (cons name value) attributes))))
    (nreverse attributes)))

;;; Comment parsing — XML 1.0 §2.5

(defun parse-comment (stream)
  "Parse a comment body and closing '-->'.  STREAM must be just past '<!--'.
Returns an xml-comment node."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (read-char stream nil nil)))
        (unless ch (error "Unterminated comment"))
        (cond
          ((char= ch #\-)
           (let ((ch2 (peek-char nil stream nil nil)))
             (if (eql ch2 #\-)
                 (progn
                   (read-char stream)   ; consume second '-'
                   (let ((ch3 (peek-char nil stream nil nil)))
                     (if (eql ch3 #\>)
                         (progn
                           (read-char stream) ; consume '>'
                           (return (make-xml-comment :data (copy-seq buf))))
                         (error "Illegal '--' inside comment"))))
                 (vector-push-extend ch buf))))
          (t
           (vector-push-extend ch buf)))))))

;;; Processing instruction parsing — XML 1.0 §2.6

(defun parse-pi (stream)
  "Parse a processing instruction body and closing '?>'.
STREAM must be just past the opening '<?'.
Returns an xml-pi node."
  (let ((target (parse-name stream)))
    (skip-whitespace stream)
    (let ((buf (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (read-char stream nil nil)))
          (unless ch (error "Unterminated processing instruction"))
          (cond
            ((char= ch #\?)
             (let ((ch2 (peek-char nil stream nil nil)))
               (if (eql ch2 #\>)
                   (progn
                     (read-char stream) ; consume '>'
                     (return (make-xml-pi :target target :data (copy-seq buf))))
                   (vector-push-extend ch buf))))
            (t
             (vector-push-extend ch buf))))))))

;;; DTD content-model parsing — XML 1.0 §3.2

(defun %parse-dtd-quoted-string (stream)
  "Parse a quoted string (single- or double-quoted) for DOCTYPE external identifiers.
Returns the string contents."
  (let ((quote (read-char stream nil nil)))
    (unless (member quote '(#\" #\') :test #'char=)
      (error "Expected a quote character in DOCTYPE external identifier"))
    (let ((buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop
        (let ((ch (read-char stream nil nil)))
          (unless ch (error "Unterminated external identifier in DOCTYPE"))
          (if (char= ch quote)
              (return (copy-seq buf))
              (vector-push-extend ch buf)))))))

(defun %skip-dtd-to-close-angle (stream)
  "Skip characters until the first unquoted '>' is consumed.
Used to skip DTD markup declarations that are not parsed (ATTLIST, ENTITY, etc.)."
  (loop
    (let ((ch (read-char stream nil nil)))
      (unless ch (error "Unterminated markup declaration in DOCTYPE internal subset"))
      (cond
        ((char= ch #\>) (return))
        ;; Skip over quoted strings so embedded '>' characters don't end the decl.
        ((member ch '(#\" #\') :test #'char=)
         (loop
           (let ((qch (read-char stream nil nil)))
             (unless qch (error "Unterminated quoted string in markup declaration"))
             (when (char= qch ch) (return)))))))))

(defun %parse-dtd-cp (stream)
  "Parse a content particle (cp): a Name or a group, with an optional quantifier.
Returns a string (Name), or (:? c), (:* c), (:+ c), (:seq cp*), or (:choice cp*)."
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (when (null ch) (error "Unexpected end of input in DTD content particle"))
    (let ((content (if (char= ch #\()
                       (%parse-dtd-group stream)
                       (parse-name stream))))
      ;; Quantifier (no whitespace allowed between content and quantifier per spec)
      (let ((q (peek-char nil stream nil nil)))
        (cond
          ((eql q #\?) (read-char stream) (list :? content))
          ((eql q #\*) (read-char stream) (list :* content))
          ((eql q #\+) (read-char stream) (list :+ content))
          (t content))))))

(defun %parse-dtd-mixed (stream)
  "Parse mixed content model starting at '#' (opening '(' already consumed).
Consumes '#PCDATA', optional '| Name' pairs, the closing ')', and optional '*'.
Returns (:mixed name*)."
  (loop for expected across "#PCDATA"
        do (let ((c (read-char stream nil nil)))
             (unless (and c (char= c expected))
               (error "Expected #PCDATA in DTD mixed content model"))))
  (skip-whitespace stream)
  (let ((names '()))
    (loop
      (let ((ch (peek-char nil stream nil nil)))
        (cond
          ((null ch) (error "Unterminated DTD mixed content model"))
          ((char= ch #\))
           (read-char stream)                   ; consume ')'
           (when (eql (peek-char nil stream nil nil) #\*)
             (read-char stream))                ; consume optional '*'
           (return (list* :mixed (nreverse names))))
          ((char= ch #\|)
           (read-char stream)                   ; consume '|'
           (skip-whitespace stream)
           (push (parse-name stream) names)
           (skip-whitespace stream))
          (t
           (error "Expected ')' or '|' in DTD mixed content model, got '~c'" ch)))))))

(defun %parse-dtd-seq-or-choice (stream)
  "Parse a sequence or choice element-content group (opening '(' already consumed).
Consumes content particles and the closing ')'.
Returns (:seq cp*) or (:choice cp*)."
  (let ((first-cp (%parse-dtd-cp stream)))
    (skip-whitespace stream)
    (let ((sep (peek-char nil stream nil nil)))
      (cond
        ((null sep) (error "Unexpected end of input in DTD content group"))
        ;; Single cp wrapped in a group — treat as a one-element sequence.
        ((char= sep #\))
         (read-char stream)              ; consume ')'
         (list :seq first-cp))
        ;; Sequence: (cp , cp , ...)
        ((char= sep #\,)
         (let ((children (list first-cp)))
           (loop while (progn (skip-whitespace stream)
                              (eql (peek-char nil stream nil nil) #\,))
                 do (read-char stream)   ; consume ','
                 do (push (%parse-dtd-cp stream) children))
           (skip-whitespace stream)
           (unless (eql (peek-char nil stream nil nil) #\))
             (error "Expected ')' at end of DTD sequence group"))
           (read-char stream)
           (cons :seq (nreverse children))))
        ;; Choice: (cp | cp | ...)
        ((char= sep #\|)
         (let ((children (list first-cp)))
           (loop while (progn (skip-whitespace stream)
                              (eql (peek-char nil stream nil nil) #\|))
                 do (read-char stream)   ; consume '|'
                 do (push (%parse-dtd-cp stream) children))
           (skip-whitespace stream)
           (unless (eql (peek-char nil stream nil nil) #\))
             (error "Expected ')' at end of DTD choice group"))
           (read-char stream)
           (cons :choice (nreverse children))))
        (t
         (error "Expected ',', '|', or ')' in DTD content group, got '~c'" sep))))))

(defun %parse-dtd-group (stream)
  "Parse a content group '(' body ')'.
Consumes the opening '(' and closing ')'.
Does NOT consume a trailing quantifier (except mixed content which absorbs its '*').
Returns (:seq cp*), (:choice cp*), or (:mixed name*)."
  (read-char stream)                    ; consume '('
  (skip-whitespace stream)
  (let ((next (peek-char nil stream nil nil)))
    (cond
      ((null next) (error "Unexpected end of input in DTD content group"))
      ((char= next #\#) (%parse-dtd-mixed stream))
      (t              (%parse-dtd-seq-or-choice stream)))))

(defun parse-dtd-content-model (stream)
  "Parse a DTD element content model specification from STREAM.
Returns one of:
  :empty               — EMPTY
  :any                 — ANY
  (:mixed name*)       — mixed content (#PCDATA possibly with element names)
  content-particle     — element content group with optional top-level quantifier"
  (skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (cond
      ((null ch) (error "Unexpected end of input in DTD content model"))
      ((char= ch #\()
       (let ((group (%parse-dtd-group stream)))
         ;; Mixed content already absorbed its quantifier inside %parse-dtd-mixed.
         ;; Element content groups may carry a top-level quantifier.
         (if (and (consp group) (eq (car group) :mixed))
             group
             (let ((q (peek-char nil stream nil nil)))
               (cond
                 ((eql q #\?) (read-char stream) (list :? group))
                 ((eql q #\*) (read-char stream) (list :* group))
                 ((eql q #\+) (read-char stream) (list :+ group))
                 (t group))))))
      (t
       (let ((kw (parse-name stream)))
         (cond
           ((string= kw "EMPTY") :empty)
           ((string= kw "ANY")   :any)
           (t (error "Expected EMPTY, ANY, or '(' in DTD content model, got '~a'" kw))))))))

;;; DTD internal subset parsing

(defun %parse-dtd-element-decl (stream)
  "Parse a DTD ELEMENT declaration.
STREAM must be positioned just after 'ELEMENT' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-dtd-element struct."
  (skip-whitespace stream)
  (let ((name (parse-name stream)))
    (skip-whitespace stream)
    (let ((content-model (parse-dtd-content-model stream)))
      (skip-whitespace stream)
      (unless (eql (read-char stream nil nil) #\>)
        (error "Expected '>' to close DTD ELEMENT declaration for '~a'" name))
      (make-xml-dtd-element :name name :content-model content-model))))

(defun %parse-dtd-internal-subset (stream)
  "Parse the internal subset of a DOCTYPE declaration.
STREAM is positioned just after '['.
Consumes through and including the closing ']'.
Returns a list of xml-dtd-element structs."
  (let ((elements '()))
    (loop
      (skip-whitespace stream)
      (let ((ch (peek-char nil stream nil nil)))
        (unless ch (error "Unterminated DOCTYPE internal subset"))
        (cond
          ;; End of internal subset
          ((char= ch #\])
           (read-char stream)            ; consume ']'
           (return (nreverse elements)))
          ;; Markup declaration or comment starting with '<'
          ((char= ch #\<)
           (read-char stream)            ; consume '<'
           (let ((next (peek-char nil stream nil nil)))
             (unless next (error "Unexpected end of input in DOCTYPE internal subset"))
             (cond
               ;; Processing instruction: <? — parse and discard
               ((char= next #\?)
                (read-char stream)       ; consume '?'
                (parse-pi stream))
               ;; Markup declaration: <!ELEMENT, <!ATTLIST, <!ENTITY, <!NOTATION, <!--
               ((char= next #\!)
                (read-char stream)       ; consume '!'
                (let ((after-bang (peek-char nil stream nil nil)))
                  (cond
                    ;; Comment: <!-- — parse and discard
                    ((eql after-bang #\-)
                     (read-char stream)  ; consume first '-'
                     (unless (eql (peek-char nil stream nil nil) #\-)
                       (error "Expected second '-' in comment in DOCTYPE internal subset"))
                     (read-char stream)  ; consume second '-'
                     (parse-comment stream))
                    ;; Named declaration: ELEMENT, ATTLIST, ENTITY, NOTATION
                    ((and after-bang (name-start-char-p after-bang))
                     (let ((decl-type (parse-name stream)))
                       (cond
                         ((string= decl-type "ELEMENT")
                          (push (%parse-dtd-element-decl stream) elements))
                         ;; ATTLIST, ENTITY, NOTATION are skipped
                         (t
                          (%skip-dtd-to-close-angle stream)))))
                    (t
                     (error "Unexpected '<!~a' in DOCTYPE internal subset"
                            (or after-bang ""))))))
               (t
                (error "Unexpected '<~c' in DOCTYPE internal subset" next)))))
          (t
           (error "Unexpected character '~c' in DOCTYPE internal subset" ch)))))))

;;; DOCTYPE declaration parsing — XML 1.0 §2.8

(defun parse-doctype (stream)
  "Parse a DOCTYPE declaration.
STREAM must be positioned just after '<!DOCTYPE' (whitespace not yet consumed).
Consumes through and including the closing '>'.
Returns an xml-doctype struct."
  (skip-whitespace stream)
  (let ((name (parse-name stream))
        public-id system-id elements)
    (skip-whitespace stream)
    ;; Optional external identifier: SYSTEM or PUBLIC
    (let ((ch (peek-char nil stream nil nil)))
      (when (and ch (name-start-char-p ch))
        (let ((keyword (parse-name stream)))
          (cond
            ((string= keyword "SYSTEM")
             (skip-whitespace stream)
             (setf system-id (%parse-dtd-quoted-string stream)))
            ((string= keyword "PUBLIC")
             (skip-whitespace stream)
             (setf public-id (%parse-dtd-quoted-string stream))
             (skip-whitespace stream)
             ;; System identifier is optional after PUBLIC in external subsets
             (when (member (peek-char nil stream nil nil) '(#\" #\') :test #'eql)
               (setf system-id (%parse-dtd-quoted-string stream))))
            (t
             (error "Expected SYSTEM or PUBLIC in DOCTYPE declaration, got '~a'"
                    keyword))))))
    (skip-whitespace stream)
    ;; Optional internal subset
    (when (eql (peek-char nil stream nil nil) #\[)
      (read-char stream)                ; consume '['
      (setf elements (%parse-dtd-internal-subset stream)))
    (skip-whitespace stream)
    (unless (eql (read-char stream nil nil) #\>)
      (error "Expected '>' to close DOCTYPE declaration"))
    (make-xml-doctype :name      name
                      :public-id public-id
                      :system-id system-id
                      :elements  elements)))

;;; Character data parsing — XML 1.0 §2.4

(defun parse-content-text (stream)
  "Parse character data and entity/character references from STREAM up to
the next '<'.  Returns the text string."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (peek-char nil stream nil nil)))
        (when (or (null ch) (char= ch #\<))
          (return)))
      (let ((ch (read-char stream)))
        (cond
          ((char= ch #\&)
           (let ((expansion (expand-entity-ref stream)))
             (loop for c across expansion do (vector-push-extend c buf))))
          (t
           (vector-push-extend ch buf)))))
    (copy-seq buf)))

;;; CDATA section parsing — XML 1.0 §2.7

(defun parse-cdata-section (stream)
  "Parse a CDATA section.  STREAM must be just past '<![CDATA['.
Returns the raw content string."
  (let ((buf (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer 0)))
    (loop
      (let ((ch (read-char stream nil nil)))
        (unless ch (error "Unterminated CDATA section"))
        (cond
          ((char= ch #\])
           (let ((ch2 (peek-char nil stream nil nil)))
             (cond
               ((eql ch2 #\])
                (read-char stream)      ; consume second ']'
                (let ((ch3 (peek-char nil stream nil nil)))
                  (cond
                    ((eql ch3 #\>)
                     (read-char stream) ; consume '>'
                     (return (copy-seq buf)))
                    ;; ']]' not followed by '>' — emit first ']', unread second ']' for reprocessing
                    (t
                     (vector-push-extend ch buf)
                     (unread-char #\] stream)))))
               (t
                (vector-push-extend ch buf)))))
          (t
           (vector-push-extend ch buf)))))))

;;; SAX handler protocol — event-driven parsing interface

(defclass sax-handler ()
  ()
  (:documentation
   "Base class for SAX event handlers.
Specialize the generic functions below to process the events you care about;
default methods are no-ops (except END-DOCUMENT, which returns NIL)."))

(defgeneric start-document (handler)
  (:documentation "Called once before any other event.")
  (:method ((handler sax-handler)) nil))

(defgeneric end-document (handler)
  (:documentation
   "Called once after all events have been fired.
The return value becomes the return value of PARSE-XML.")
  (:method ((handler sax-handler)) nil))

(defgeneric start-element (handler tag attributes)
  (:documentation
   "Called when an opening (or self-closing) tag is encountered.
TAG is a string; ATTRIBUTES is an alist of (name . value) string pairs.")
  (:method ((handler sax-handler) tag attributes)
    (declare (ignore tag attributes))
    nil))

(defgeneric end-element (handler tag)
  (:documentation
   "Called when a closing (or self-closing) tag has been processed.
TAG is a string.")
  (:method ((handler sax-handler) tag)
    (declare (ignore tag))
    nil))

(defgeneric characters (handler text)
  (:documentation
   "Called for a run of character data content.
TEXT is a string with entity and character references already expanded.")
  (:method ((handler sax-handler) text)
    (declare (ignore text))
    nil))

(defgeneric comment (handler data)
  (:documentation
   "Called when a comment is encountered.
DATA is the raw comment body (between <!-- and -->).")
  (:method ((handler sax-handler) data)
    (declare (ignore data))
    nil))

(defgeneric processing-instruction (handler target data)
  (:documentation
   "Called when a processing instruction is encountered.
TARGET and DATA are both strings.")
  (:method ((handler sax-handler) target data)
    (declare (ignore target data))
    nil))

(defgeneric cdata-section (handler data)
  (:documentation
   "Called when a CDATA section is encountered.
DATA is the raw content string (between <![CDATA[ and ]]>).")
  (:method ((handler sax-handler) data)
    (declare (ignore data))
    nil))

(defgeneric doctype-declaration (handler doctype)
  (:documentation
   "Called when a DOCTYPE declaration is parsed.
DOCTYPE is an xml-doctype struct containing the parsed name, optional external
identifiers, and a list of xml-dtd-element structs from the internal subset.")
  (:method ((handler sax-handler) doctype)
    (declare (ignore doctype))
    nil))

;;; Default DOM-building SAX handler

(defclass dom-builder (sax-handler)
  ((%prolog  :initform '())
   (%doctype :initform nil)
   (%stack   :initform '())
   (%root    :initform nil))
  (:documentation
   "SAX handler that builds an XML-DOCUMENT structure — the default behaviour
of PARSE-XML when no custom handler is supplied.
Each stack frame is a list (tag attributes children-accumulator)."))

(defmethod start-element ((handler dom-builder) tag attributes)
  (push (list tag attributes '()) (slot-value handler '%stack)))

(defmethod end-element ((handler dom-builder) tag)
  (declare (ignore tag))
  (let* ((frame (pop (slot-value handler '%stack)))
         (node  (make-xml-node :tag        (first frame)
                               :attributes (second frame)
                               :children   (nreverse (third frame)))))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (setf (slot-value handler '%root) node))))

(defmethod characters ((handler dom-builder) text)
  ;; Whitespace-only runs between elements are discarded, matching the
  ;; original DOM parser behaviour.
  (when (and (slot-value handler '%stack)
             (not (every #'xml-whitespace-p text)))
    (push text (third (first (slot-value handler '%stack))))))

(defmethod comment ((handler dom-builder) data)
  (let ((node (make-xml-comment :data data)))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (push node (slot-value handler '%prolog)))))

(defmethod processing-instruction ((handler dom-builder) target data)
  (let ((node (make-xml-pi :target target :data data)))
    (if (slot-value handler '%stack)
        (push node (third (first (slot-value handler '%stack))))
        (push node (slot-value handler '%prolog)))))

(defmethod cdata-section ((handler dom-builder) data)
  (when (slot-value handler '%stack)
    (push (make-xml-cdata :data data)
          (third (first (slot-value handler '%stack))))))

(defmethod doctype-declaration ((handler dom-builder) doctype)
  (setf (slot-value handler '%doctype) doctype))

(defmethod end-document ((handler dom-builder))
  (make-xml-document :prolog  (nreverse (slot-value handler '%prolog))
                     :doctype (slot-value handler '%doctype)
                     :root    (slot-value handler '%root)))

;;; SAX-based element parsing — XML 1.0 §3.1

(defun parse-element-sax (stream handler)
  "Parse an XML element whose opening '<' has already been consumed.
STREAM is positioned at the first character of the tag name.
Fires START-ELEMENT, child events, and END-ELEMENT on HANDLER."
  (let* ((tag        (parse-name stream))
         (attributes (parse-attributes stream)))
    (skip-whitespace stream)
    (let ((ch (peek-char nil stream nil nil)))
      (cond
        ;; Self-closing tag: />
        ((eql ch #\/)
         (read-char stream)             ; consume '/'
         (unless (eql (read-char stream nil nil) #\>)
           (error "Expected '>' to close self-closing tag '~a'" tag))
         (start-element handler tag attributes)
         (end-element handler tag))
        ;; Opening tag: >  …content…  </tag>
        ((eql ch #\>)
         (read-char stream)             ; consume '>'
         (start-element handler tag attributes)
         (parse-children-sax stream tag handler)
         (end-element handler tag))
        (t
         (error "Expected '>' or '/>' after attributes of '~a'" tag))))))

(defun parse-children-sax (stream parent-tag handler)
  "Parse the content of an element, firing SAX events on HANDLER, until the
matching closing tag is consumed."
  (loop
    (let ((ch (peek-char nil stream nil nil)))
      (unless ch
        (error "Unexpected end of input while parsing children of <~a>" parent-tag))
      (cond
        ;; Character data (possibly containing entity references)
        ((char/= ch #\<)
         (characters handler (parse-content-text stream)))
        ;; Markup starting with '<'
        (t
         (read-char stream)             ; consume '<'
         (let ((next (peek-char nil stream nil nil)))
           (unless next
             (error "Unexpected end of input after '<'"))
           (cond
             ;; Closing tag: </parent-tag>
             ((char= next #\/)
              (read-char stream)        ; consume '/'
              (let ((tag (parse-name stream)))
                (unless (string= tag parent-tag)
                  (error "Mismatched closing tag: expected </~a>, got </~a>"
                         parent-tag tag))
                (skip-whitespace stream)
                (unless (eql (read-char stream nil nil) #\>)
                  (error "Expected '>' to close </~a>" tag))
                (return)))
             ;; Nodes beginning with '<!'
             ((char= next #\!)
              (read-char stream)        ; consume '!'
              (let ((after-bang (peek-char nil stream nil nil)))
                (cond
                  ;; Comment: <!--
                  ((eql after-bang #\-)
                   (read-char stream)   ; consume first '-'
                   (unless (eql (peek-char nil stream nil nil) #\-)
                     (error "Expected second '-' in comment opening '<!--'"))
                   (read-char stream)   ; consume second '-'
                   (comment handler (xml-comment-data (parse-comment stream))))
                  ;; CDATA section: <![CDATA[
                  ((eql after-bang #\[)
                   (read-char stream)   ; consume '['
                   (loop for expected across "CDATA["
                         do (let ((c (read-char stream nil nil)))
                              (unless (and c (char= c expected))
                                (error "Invalid CDATA section start"))))
                   (cdata-section handler (parse-cdata-section stream)))
                  (t
                   (error "Unexpected '<!' sequence")))))
             ;; Processing instruction: <?
             ((char= next #\?)
              (read-char stream)        ; consume '?'
              (let ((pi-node (parse-pi stream)))
                (processing-instruction handler
                                        (xml-pi-target pi-node)
                                        (xml-pi-data pi-node))))
             ;; Child element
             (t
              (parse-element-sax stream handler)))))))))

;;; SAX-based document prolog — XML 1.0 §2.8

(defun parse-prolog-sax (stream handler)
  "Parse the XML document prolog, firing SAX events for comments and
processing instructions on HANDLER.  DOCTYPE declarations are silently
skipped.  Leaves STREAM positioned at the '<' of the root element."
  (loop
    (skip-whitespace stream)
    (unless (eql (peek-char nil stream nil nil) #\<)
      (return))
    (read-char stream)                  ; consume '<'
    (let ((next (peek-char nil stream nil nil)))
      (cond
        ;; Processing instruction or XML declaration: <?
        ((eql next #\?)
         (read-char stream)             ; consume '?'
         (let ((pi-node (parse-pi stream)))
           (processing-instruction handler
                                   (xml-pi-target pi-node)
                                   (xml-pi-data pi-node))))
        ;; Comment or DOCTYPE: <!
        ((eql next #\!)
         (read-char stream)             ; consume '!'
         (let ((after-bang (peek-char nil stream nil nil)))
           (cond
             ;; Comment: <!--
             ((eql after-bang #\-)
              (read-char stream)        ; consume first '-'
              (unless (eql (peek-char nil stream nil nil) #\-)
                (error "Expected second '-' in comment opening '<!--'"))
              (read-char stream)        ; consume second '-'
              (comment handler (xml-comment-data (parse-comment stream))))
             ;; DOCTYPE: <!DOCTYPE
             ((eql after-bang #\D)
              (loop for expected across "DOCTYPE"
                    do (let ((c (read-char stream nil nil)))
                         (unless (and c (char= c expected))
                           (error "Invalid DOCTYPE declaration"))))
              (doctype-declaration handler (parse-doctype stream)))
             (t
              (error "Unexpected '<!~c' in prolog" after-bang)))))
        ;; Anything else is the root element: unread '<' and stop
        (t
         (unread-char #\< stream)
         (return))))))

;;; Namespace resolution — Namespaces in XML 1.0

(defparameter +xml-namespace-uri+
  "http://www.w3.org/XML/1998/namespace"
  "Pre-declared namespace URI for the 'xml' prefix (Namespaces in XML 1.0 §3).")

(defparameter +xmlns-namespace-uri+
  "http://www.w3.org/2000/xmlns/"
  "Pre-declared namespace URI for the 'xmlns' prefix (Namespaces in XML 1.0 §3).")

(defun %split-qname (name)
  "Split a raw NAME string at the first colon.
Returns (values prefix local-name) where PREFIX is NIL when there is no colon."
  (let ((colon (position #\: name)))
    (if colon
        (values (subseq name 0 colon) (subseq name (1+ colon)))
        (values nil name))))

(defun %resolve-attr-qname (name ns-bindings)
  "Resolve a raw attribute NAME string into an XML-QNAME using NS-BINDINGS.
Attributes without a prefix are in no namespace (the default namespace does
not apply to attributes per Namespaces in XML 1.0 §6.2)."
  (multiple-value-bind (prefix local-name) (%split-qname name)
    (let ((uri (cond
                 ((null prefix) nil)
                 ((string= prefix "xml")   +xml-namespace-uri+)
                 ((string= prefix "xmlns") +xmlns-namespace-uri+)
                 (t
                  (let ((binding (assoc prefix ns-bindings :test #'string=)))
                    (unless binding
                      (error "Undeclared namespace prefix '~a'" prefix))
                    (cdr binding))))))
      (make-xml-qname :prefix        prefix
                      :local-name    local-name
                      :namespace-uri uri))))

(defun %resolve-elem-qname (name ns-bindings default-ns)
  "Resolve a raw element NAME string into an XML-QNAME.
NS-BINDINGS is an alist of (prefix . uri) pairs; DEFAULT-NS is the current
default namespace URI (NIL when there is no default namespace)."
  (multiple-value-bind (prefix local-name) (%split-qname name)
    (let ((uri (cond
                 ((null prefix) default-ns)
                 ((string= prefix "xml")   +xml-namespace-uri+)
                 ((string= prefix "xmlns") +xmlns-namespace-uri+)
                 (t
                  (let ((binding (assoc prefix ns-bindings :test #'string=)))
                    (unless binding
                      (error "Undeclared namespace prefix '~a'" prefix))
                    (cdr binding))))))
      (make-xml-qname :prefix        prefix
                      :local-name    local-name
                      :namespace-uri uri))))

(defun %collect-ns-decls (attributes)
  "Partition ATTRIBUTES (an alist of raw-name . value pairs) into namespace
declarations and ordinary attributes.
Returns (values ns-decls remaining) where NS-DECLS is an alist of (prefix . uri)
pairs with NIL as the prefix for a default-namespace declaration (xmlns=\"...\")
and REMAINING is the list of non-declaration attributes."
  (let (ns-decls remaining)
    (dolist (attr attributes)
      (let ((k (car attr)) (v (cdr attr)))
        (cond
          ((string= k "xmlns")
           (push (cons nil v) ns-decls))
          ((and (> (length k) 6) (string= k "xmlns:" :end1 6))
           (push (cons (subseq k 6) v) ns-decls))
          (t
           (push attr remaining)))))
    (values ns-decls (nreverse remaining))))

(defun %resolve-ns-node (node ns-bindings default-ns)
  "Return a fresh XML-NODE tree with namespace-resolved tag and attribute keys.
NS-BINDINGS is the inherited alist of (prefix . uri) pairs; DEFAULT-NS is the
inherited default namespace URI."
  (multiple-value-bind (ns-decls remaining-attrs)
      (%collect-ns-decls (xml-node-attributes node))
    (let* (;; Update default namespace: xmlns="" resets it to NIL
           (default-ns-entry (assoc nil ns-decls))
           (new-default-ns   (if default-ns-entry
                                 (let ((v (cdr default-ns-entry)))
                                   (if (string= v "") nil v))
                                 default-ns))
           ;; Prepend new prefix bindings so they shadow outer ones
           (new-bindings     (append (remove nil ns-decls :key #'car)
                                     ns-bindings))
           (qname            (%resolve-elem-qname (xml-node-tag node)
                                                  new-bindings
                                                  new-default-ns))
           (resolved-attrs   (mapcar (lambda (attr)
                                       (cons (%resolve-attr-qname (car attr)
                                                                   new-bindings)
                                             (cdr attr)))
                                     remaining-attrs))
           (resolved-children
            (mapcar (lambda (child)
                      (typecase child
                        (xml-node (%resolve-ns-node child
                                                    new-bindings
                                                    new-default-ns))
                        (t child)))
                    (xml-node-children node))))
      (make-xml-node :tag        qname
                     :attributes resolved-attrs
                     :children   resolved-children))))

(defun resolve-namespaces (document)
  "Return a copy of DOCUMENT with all element and attribute names resolved to
XML-QNAME instances according to Namespaces in XML 1.0.

Each XML-NODE's TAG field becomes an XML-QNAME struct, and each attribute key
in the attribute alist also becomes an XML-QNAME.  Namespace declaration
attributes (xmlns and xmlns:prefix) are consumed and do not appear in the
resolved attribute alist.

The 'xml' and 'xmlns' prefixes are pre-bound per the specification and require
no explicit xmlns declaration.

Signals an error if a namespace prefix is used but has not been declared.
An xmlns=\"\" declaration resets the default namespace to NIL (no namespace)."
  (let ((ns-bindings (list (cons "xml"   +xml-namespace-uri+)
                           (cons "xmlns" +xmlns-namespace-uri+))))
    (make-xml-document
     :prolog (xml-document-prolog document)
     :root   (%resolve-ns-node (xml-document-root document)
                               ns-bindings
                               nil))))

;;; Public API

(defun parse-xml (input &key (handler (make-instance 'dom-builder)))
  "Parse INPUT (a string, standard character stream, or trivial-gray-streams
character stream) as an XML document using a SAX-style event handler.

When called without a HANDLER keyword argument, uses the built-in DOM-BUILDER
handler and returns an XML-DOCUMENT node (backward-compatible behaviour).

When a custom SAX-HANDLER subclass instance is supplied, the parser fires the
following generic functions on it as it walks the input:
  START-DOCUMENT, START-ELEMENT, END-ELEMENT, CHARACTERS, COMMENT,
  PROCESSING-INSTRUCTION, CDATA-SECTION, END-DOCUMENT.
The return value of END-DOCUMENT on the handler becomes the return value of
PARSE-XML.

Entity references (&amp; &lt; &gt; &quot; &apos; &#N; &#xN;) are expanded
before CHARACTERS and attribute values are reported."
  (let ((stream (etypecase input
                  (string (make-string-input-stream input))
                  (fundamental-character-input-stream input)
                  (stream input))))
    (start-document handler)
    (parse-prolog-sax stream handler)
    (unless (eql (peek-char nil stream nil nil) #\<)
      (error "Expected root element"))
    (read-char stream)                  ; consume '<'
    (parse-element-sax stream handler)
    (end-document handler)))
